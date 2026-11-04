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
  distances <- igraph::distances(graph, weights = lengths)
  finite <- is.finite(distances)
  if (!all(finite)) {
    distances[!finite] <- 1.5 * max(distances[finite])
  }
  coords <- uwot::umap(
    stats::as.dist(distances),
    n_neighbors = min(n_neighbors, n - 1),
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
# approximation), with a cost in the square of the number of nodes. It is
# comfortable up to a few thousand nodes.
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
  # (a dense matrix: the repulsion below is already a matrix of all pairs)
  adjacency <- matrix(0, n, n)
  adjacency[edge_list] <- w
  adjacency[edge_list[, 2:1, drop = FALSE]] <- w
  mass <- igraph::degree(graph) + 1
  mass_product <- outer(mass, mass)

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
    dx <- outer(x, x, "-")
    dy <- outer(y, y, "-")
    squared_distance <- dx^2 + dy^2
    diag(squared_distance) <- Inf
    squared_distance <- pmax(squared_distance, 1e-9) # two nodes at the same place

    # Repulsion between all the pairs: kr (m_i m_j) / d, along the unit vector
    repulsion <- scaling_ratio * mass_product / squared_distance
    fx <- rowSums(repulsion * dx)
    fy <- rowSums(repulsion * dy)

    # Attraction along the edges: w d (linear) or w log(1 + d) (linlog)
    if (linlog) {
      edge_x <- x[edge_list[, 2]] - x[edge_list[, 1]]
      edge_y <- y[edge_list[, 2]] - y[edge_list[, 1]]
      edge_d <- sqrt(edge_x^2 + edge_y^2)
      factor <- w * log1p(edge_d) / pmax(edge_d, 1e-9)
      fx <- fx + tabulate_edges(edge_list[, 1], factor * edge_x, n) - tabulate_edges(edge_list[, 2], factor * edge_x, n)
      fy <- fy + tabulate_edges(edge_list[, 1], factor * edge_y, n) - tabulate_edges(edge_list[, 2], factor * edge_y, n)
    } else {
      fx <- fx + as.numeric(adjacency %*% x) - rowSums(adjacency) * x
      fy <- fy + as.numeric(adjacency %*% y) - rowSums(adjacency) * y
    }

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

# Sum of `values` by node, for the linlog attraction
tabulate_edges <- function(nodes, values, n) {
  as.numeric(rowsum(values, factor(nodes, levels = seq_len(n)), reorder = FALSE)[, 1])
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
  if (is.null(coords)) {
    layout_df <- ggraph::create_layout(graph, layout = layout, ...)
  } else {
    layout_df <- ggraph::create_layout(graph, layout = layout, coords = coords, ...)
  }
  cbind(x = layout_df$x, y = layout_df$y)
}
