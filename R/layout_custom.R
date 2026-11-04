# Two layouts that are not in ggraph: UMAP and ForceAtlas2. They are used by
# layout_networks() (layout = "umap" or "forceatlas2") and return a matrix
# with one row per node (in the order of the node table) and two columns.

custom_layouts <- c("umap", "forceatlas2")

# Edge weights of a graph as a numeric vector (1 when there is no weight, or
# when `weights = NA`, the convention of igraph to ignore them).
layout_edge_weights <- function(graph, weights = NULL) {
  n_edges <- igraph::ecount(graph)
  if (length(weights) == 1 && is.na(weights)) {
    return(rep(1, n_edges))
  }
  if (is.null(weights)) {
    weights <- if ("weight" %in% igraph::edge_attr_names(graph)) igraph::E(graph)$weight else rep(1, n_edges)
  }
  if (length(weights) != n_edges || anyNA(weights) || any(weights <= 0)) {
    cli::cli_abort("{.arg weights} must be positive numbers, one per edge (or {.code NA} to ignore the weights).")
  }
  weights
}

# UMAP (McInnes et al., 2018) applied to the distances between nodes in the
# network. A UMAP needs distances between all pairs of points, whereas a
# network gives ties between some of them: the distance between two nodes is
# the length of the shortest path, an edge being as long as the inverse of its
# weight (a strong tie is a short one). Nodes of different components are put
# at a distance larger than any finite one, so that components are kept
# apart.
layout_umap <- function(graph, n_neighbors = 15, min_dist = 0.1, spread = 1,
                        weights = NULL, ...) {
  rlang::check_installed("uwot", reason = "to compute the UMAP layout.")
  n <- igraph::vcount(graph)
  if (n < 4) {
    cli::cli_abort("A UMAP layout needs at least 4 nodes, not {n}.")
  }
  lengths <- 1 / layout_edge_weights(graph, weights)
  # UMAP only uses the nearest neighbours of each point: they are computed
  # from the shortest paths by blocks of rows and given to uwot, instead of
  # the matrix of all the distances (on 9,600 nodes, 90 seconds and 5.7 GB
  # with the full matrix).
  k <- min(n_neighbors, n - 1) + 1 # uwot counts the point itself as its first neighbour
  block_size <- max(1L, floor(2e7 / n))
  blocks <- split(seq_len(n), ceiling(seq_len(n) / block_size))
  nn_idx <- matrix(0L, n, k)
  nn_dist <- matrix(0, n, k)
  for (rows in blocks) {
    d <- igraph::distances(graph, v = rows, weights = lengths)
    finite <- is.finite(d)
    if (!all(finite)) {
      # nodes of another component: farther than any connected pair
      d[!finite] <- 1.5 * max(d[finite])
    }
    nearest <- t(apply(d, 1, function(row) order(row)[seq_len(k)]))
    nn_idx[rows, ] <- nearest
    nn_dist[rows, ] <- matrix(d[cbind(rep(seq_along(rows), k), as.vector(nearest))], ncol = k)
  }
  coords <- uwot::umap(
    X = NULL,
    nn_method = list(idx = nn_idx, dist = nn_dist),
    n_components = 2,
    min_dist = min_dist,
    spread = spread,
    ...
  )
  unname(coords)
}

# ForceAtlas2 (Jacomy et al., 2014), a force-directed layout designed for
# the exploration of networks, with the adaptive speed of the original
# algorithm: attraction along edges proportional to the distance and to the
# weight, repulsion between all nodes proportional to the product of their
# degrees plus one (so that hubs push others away), and a gravity that keeps
# the components around the origin. This is a compact implementation: the
# repulsion between all pairs is computed exactly (no Barnes-Hut
# approximation), with a cost in the square of the number of nodes: on a Web
# of Science sample, about 1 minute for 1,000 nodes and 4 to 5 minutes for
# 2,000 (500 iterations). A faster version would need Barnes-Hut in compiled
# code (see to_do.qmd).
layout_forceatlas2 <- function(graph, iterations = 500, scaling_ratio = 2,
                               gravity = 1, strong_gravity = FALSE,
                               linlog = FALSE, jitter_tolerance = 1,
                               coords = NULL, weights = NULL) {
  n <- igraph::vcount(graph)
  if (n < 2) {
    return(matrix(0, nrow = n, ncol = 2))
  }
  # Weights are rescaled by the largest one: similarity weights are often
  # tiny (1e-4), and attraction would then vanish against repulsion.
  w <- layout_edge_weights(graph, weights)
  w <- w / max(w)
  edge_list <- igraph::as_edgelist(graph, names = FALSE)
  mass <- igraph::degree(graph) + 1
  if (n > 1000) {
    cli::cli_inform(c("!" = "{.val forceatlas2} computes the repulsion between all the pairs of nodes: with {n} nodes, expect several minutes (about 1 minute for 1,000 nodes, the time growing with the square of the number of nodes). {.val fr} and {.val umap} are much faster on large networks."))
  }
  # The repulsion is computed by blocks of rows, so that memory stays bounded
  # (about 20 million pairs at a time) whatever the number of nodes.
  block_size <- max(1L, floor(2e7 / n))
  blocks <- split(seq_len(n), ceiling(seq_len(n) / block_size))

  # Starting positions: the ones given (e.g. the previous window), or random
  # ones (set a seed before calling the function to reproduce the layout).
  if (is.null(coords)) {
    coords <- matrix(stats::runif(2 * n, -1, 1) * sqrt(n), ncol = 2)
  }
  x <- coords[, 1]
  y <- coords[, 2]
  old_fx <- old_fy <- rep(0, n)
  speed <- 1
  speed_efficiency <- 1

  for (iteration in seq_len(iterations)) {
    # Repulsion between all the pairs: kr (m_i m_j) / d, along the unit vector
    fx <- fy <- numeric(n)
    for (rows in blocks) {
      dx <- outer(x[rows], x, "-")
      dy <- outer(y[rows], y, "-")
      squared_distance <- dx^2 + dy^2
      squared_distance[cbind(seq_along(rows), rows)] <- Inf # a node does not repel itself
      squared_distance <- pmax(squared_distance, 1e-9) # two nodes at the same place
      repulsion <- scaling_ratio * outer(mass[rows], mass) / squared_distance
      fx[rows] <- rowSums(repulsion * dx)
      fy[rows] <- rowSums(repulsion * dy)
    }

    # Attraction along the edges: w d (linear) or w log(1 + d) (linlog)
    edge_x <- x[edge_list[, 2]] - x[edge_list[, 1]]
    edge_y <- y[edge_list[, 2]] - y[edge_list[, 1]]
    factor <- if (linlog) {
      edge_d <- sqrt(edge_x^2 + edge_y^2)
      w * log1p(edge_d) / pmax(edge_d, 1e-9)
    } else {
      w
    }
    fx <- fx + tabulate_edges(edge_list[, 1], factor * edge_x, n) - tabulate_edges(edge_list[, 2], factor * edge_x, n)
    fy <- fy + tabulate_edges(edge_list[, 1], factor * edge_y, n) - tabulate_edges(edge_list[, 2], factor * edge_y, n)

    # Gravity towards the origin: constant (or growing with the distance when
    # `strong_gravity`), proportional to the mass
    distance_to_origin <- pmax(sqrt(x^2 + y^2), 1e-9)
    pull <- if (strong_gravity) gravity * mass else gravity * mass / distance_to_origin
    fx <- fx - pull * x
    fy <- fy - pull * y

    # Adaptive speed (Jacomy et al., 2014): the global swinging (how much
    # forces change direction) limits the speed
    swinging <- sqrt((old_fx - fx)^2 + (old_fy - fy)^2)
    traction <- 0.5 * sqrt((old_fx + fx)^2 + (old_fy + fy)^2)
    total_swinging <- sum(mass * swinging)
    total_traction <- sum(mass * traction)
    estimated_tolerance <- 0.05 * sqrt(n)
    min_tolerance <- sqrt(estimated_tolerance)
    tolerance <- jitter_tolerance * max(min_tolerance, min(10, estimated_tolerance * total_traction / n^2))
    if (total_swinging / max(total_traction, 1e-9) > 2) {
      if (speed_efficiency > 0.05) speed_efficiency <- speed_efficiency * 0.5
      tolerance <- max(tolerance, jitter_tolerance)
    }
    target_speed <- if (total_swinging > 0) tolerance * speed_efficiency * total_traction / total_swinging else speed
    if (total_swinging > tolerance * total_traction) {
      if (speed_efficiency > 0.05) speed_efficiency <- speed_efficiency * 0.7
    } else if (speed < 1000) {
      speed_efficiency <- speed_efficiency * 1.3
    }
    speed <- speed + min(target_speed - speed, 0.5 * speed)

    factor <- speed / (1 + sqrt(speed * mass * swinging))
    x <- x + fx * factor
    y <- y + fy * factor
    old_fx <- fx
    old_fy <- fy
  }
  cbind(x, y, deparse.level = 0)
}

# Sum of `values` by node (the forces along the edges)
tabulate_edges <- function(nodes, values, n) {
  # (rowsum() returns one row per node present, named by the node: the sums
  # are put back at the position of their node)
  sums <- rowsum(values, nodes)
  out <- numeric(n)
  out[as.integer(rownames(sums))] <- sums[, 1]
  out
}

# Coordinates of a graph with the layout: UMAP and ForceAtlas2 above, or any
# layout of ggraph::create_layout(). `coords` are starting positions (for
# dynamic coordinates) and are only used by the layouts that accept them.
compute_layout <- function(graph, layout, coords = NULL, ...) {
  if (layout %in% custom_layouts) {
    igraph_graph <- tidygraph::as.igraph(graph)
    if (layout == "umap") {
      return(layout_umap(igraph_graph, ...))
    }
    return(layout_forceatlas2(igraph_graph, coords = coords, ...))
  }
  layout_df <- tryCatch(
    if (is.null(coords)) {
      ggraph::create_layout(graph, layout = layout, ...)
    } else {
      ggraph::create_layout(graph, layout = layout, coords = coords, ...)
    },
    error = function(e) {
      if (grepl("layout_tbl_graph_", conditionMessage(e), fixed = TRUE)) {
        cli::cli_abort(c(
          "{.val {layout}} is not a known layout.",
          "i" = "Use a layout of {.fn ggraph::create_layout} (e.g. {.val fr}, {.val kk}, {.val stress}, {.val drl}), or {.val forceatlas2} or {.val umap}."
        ), call = rlang::caller_env(4))
      }
      cli::cli_abort("The layout {.val {layout}} failed.", parent = e, call = rlang::caller_env(4))
    }
  )
  cbind(x = layout_df$x, y = layout_df$y)
}
