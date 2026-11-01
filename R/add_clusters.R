#' Detect communities in networks
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The second step of the pipeline. In a bibliographic coupling network,
#' groups of articles that cite the same literature, much more among
#' themselves than with the rest of the network, are a usual operational
#' definition of a research specialty or a research front
#' \insertCite{boyack2010,morris2008}{networkflow}. `add_clusters()` finds such
#' groups (communities, or clusters) with one of five algorithms of igraph,
#' adds the cluster of each node and of each edge, and the relative size of
#' each cluster. It works on one `tbl_graph` or on each graph of a list, as
#' produced by [build_dynamic_networks()].
#'
#' Most methods search for the partition that maximises the *modularity*
#' \insertCite{newman2004}{networkflow}: the share of the edge weight that
#' falls within clusters, minus the share expected if edges were placed at
#' random given the weighted degree \eqn{k_i} of each node,
#' \deqn{Q = \frac{1}{2m} \sum_{i,j} \left( A_{ij} - \gamma \frac{k_i k_j}{2m}
#' \right) \delta(c_i, c_j),}
#' with \eqn{A_{ij}} the weight of the edge between \eqn{i} and \eqn{j},
#' \eqn{m} the total weight, \eqn{c_i} the cluster of \eqn{i}, and
#' \eqn{\gamma} the resolution (1 in the original definition).
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param weights Edge weights. `NULL` (default) uses the `weight` edge
#'   column when it exists, `NA` ignores it, and a numeric vector gives the
#'   weights directly. Weights are read as strengths of ties: a larger weight
#'   pulls two nodes into the same cluster.
#' @param clustering_method The algorithm, to choose explicitly among
#'   `"leiden"`, `"louvain"`, `"fast_greedy"`, `"infomap"` and `"walktrap"`
#'   (see the details).
#' @param objective_function For Leiden, the quality function to maximise:
#'   `"modularity"` or `"CPM"` (constant Potts model). Required for Leiden.
#' @param resolution For Leiden, the resolution \eqn{\gamma}. Higher values
#'   give more and smaller clusters.
#' @param n_iterations For Leiden, the number of iterations of the algorithm.
#'   The default, `-1`, iterates until the partition no longer improves (see
#'   [igraph::cluster_leiden()]). A fixed number is much slower on large
#'   networks for no gain: on a window of 8,700 articles and 116,000 edges,
#'   1,000 iterations took 44 s, against 0.6 s for `-1`, for the same
#'   modularity.
#' @param n_groups For fast greedy and walktrap, an optional number of
#'   clusters at which to cut the dendrogram.
#' @param node_weights For Leiden and Infomap, optional name of a node column
#'   with node weights (see [igraph::cluster_leiden()] and
#'   [igraph::cluster_infomap()]).
#' @param trials For Infomap, the number of attempts to partition the network.
#' @param steps For walktrap, the length of the random walks.
#' @param verbose Logical. If `TRUE` (default), reports the number of
#'   clusters and the size of the largest one.
#' @param seed Optional integer. Leiden, Louvain and Infomap are stochastic:
#'   two runs can give different partitions. A seed makes the result
#'   reproducible. It is set locally (with [withr::local_seed()]): the
#'   random-number stream of the session is left unchanged.
#'
#' @details
#' **The algorithms.**
#'
#' - `"leiden"` \insertCite{traag2019}{networkflow}, the default choice of
#'   recent bibliometric studies, improves on Louvain: it guarantees that
#'   clusters are connected, and iterating it refines the partition. It can
#'   maximise the modularity or the constant Potts model (CPM)
#'   \insertCite{traag2011}{networkflow}, which compares the internal weight
#'   \eqn{e_c} of each cluster with \eqn{\gamma} times its number of pairs,
#'   \deqn{H = \sum_c \left( e_c - \gamma \binom{n_c}{2} \right):}
#'   a cluster is worth creating when its density exceeds \eqn{\gamma}.
#'   Unlike the modularity, CPM does not depend on the size of the network,
#'   which makes clusters more comparable across networks of different sizes,
#'   but \eqn{\gamma} must then be set on the scale of the edge weights (with
#'   weights well below 1, `resolution = 1` yields almost only singletons).
#' - `"louvain"` \insertCite{blondel2008}{networkflow}, the greedy
#'   multi-level optimisation of modularity that Leiden refines.
#' - `"fast_greedy"` \insertCite{clauset2004}{networkflow}, an agglomerative
#'   optimisation of modularity, deterministic.
#' - `"walktrap"` \insertCite{pons2006}{networkflow}, which groups nodes that
#'   short random walks tend to visit together.
#' - `"infomap"` \insertCite{rosvall2008}{networkflow}, which finds the
#'   partition that best compresses the description of a random walk on the
#'   network.
#'
#' All algorithms are called from igraph ([igraph::cluster_leiden()],
#' [igraph::cluster_louvain()], [igraph::cluster_fast_greedy()],
#' [igraph::cluster_walktrap()], [igraph::cluster_infomap()]), and all use the
#' edge weights unless `weights = NA`.
#'
#' **The resolution.** Modularity has a resolution limit: in large networks,
#' it can merge small but clear-cut communities
#' \insertCite{fortunato2010}{networkflow}. There is no single right number of
#' clusters: a network of a discipline can be described at the level of
#' fields or of specialties \insertCite{sjogarde2020}{networkflow}. Changing
#' `resolution`, and checking that the main clusters persist across close
#' values and across seeds \insertCite{traag2013}{networkflow}, is part of
#' the analysis.
#'
#' **The output columns.** With `clustering_method = "leiden"`, nodes get
#' `cluster_leiden` (identifiers numbered by decreasing cluster size, "01"
#' being the largest, written with two digits so that they sort correctly) and
#' `size_cluster_leiden` (the share of the nodes of the graph in the cluster).
#' Edges get `cluster_leiden_from`, `cluster_leiden_to` and `cluster_leiden`,
#' the latter being the common cluster of both ends, or "00" for edges
#' between clusters. The edge cluster is what lets [color_networks()] give
#' within-cluster edges the colour of their cluster. Other methods produce
#' the same columns with their own suffix.
#'
#' @return The same object as `graphs` (a `tbl_graph` or a list of
#'   `tbl_graph`s) with the cluster columns described in the details, with
#'   the node table active.
#'
#' @seealso [merge_dynamic_clusters()] to follow clusters across time
#'   windows, [name_clusters()] to label them, [add_node_roles()] to
#'   characterise the position of nodes within and between clusters.
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1
#' )
#'
#' network <- add_clusters(
#'   network,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   resolution = 1,
#'   seed = 123
#' )
#' table(tidygraph::as_tibble(network, active = "nodes")$cluster_leiden)
#'
#' @export
add_clusters <- function(
  graphs,
  weights = NULL,
  clustering_method = c(
    "leiden",
    "louvain",
    "fast_greedy",
    "infomap",
    "walktrap"
  ),
  objective_function = c("modularity", "CPM"), #leiden
  resolution = 1, #leiden
  n_iterations = -1, #leiden
  n_groups = NULL, #fast_greedy & walktrap
  node_weights = NULL, #infomap & Leiden
  trials = 10, #infomap
  steps = 4, #walktrap
  verbose = TRUE,
  seed = NA
) {
  clustering_method <- check_explicit_choice(
    clustering_method,
    c("leiden", "louvain", "fast_greedy", "infomap", "walktrap"),
    "clustering_method"
  )
  if (clustering_method == "leiden") {
    objective_function <- check_explicit_choice(
      objective_function,
      c("modularity", "CPM"),
      "objective_function",
      context = "With the Leiden algorithm"
    )
  }
  single <- is_single_graph(graphs)

  # The seed is set locally: the random-number stream of the session is
  # restored when the function exits (CRAN policy: no change of the global
  # state).
  if (!is.null(seed) && !is.na(seed)) {
    withr::local_seed(seed)
  }

  cluster_one <- function(graph) {
    detect_cluster(
      graph,
      weights = weights,
      clustering_method = clustering_method,
      objective_function = objective_function,
      resolution = resolution,
      n_iterations = n_iterations,
      n_groups = n_groups,
      node_weights = node_weights,
      trials = trials,
      steps = steps
    )
  }

  if (single) {
    graphs <- cluster_one(graphs)
    if (verbose) report_clusters(list(graphs), clustering_method)
    return(graphs)
  }
  if (verbose) cli::cli_progress_bar("Detecting clusters", total = length(graphs))
  for (i in seq_along(graphs)) {
    graphs[[i]] <- cluster_one(graphs[[i]])
    if (verbose) cli::cli_progress_update()
  }
  if (verbose) report_clusters(graphs, clustering_method)
  graphs
}

# Membership vector of one graph with the chosen algorithm of igraph.
cluster_membership <- function(graph, weights, clustering_method, objective_function,
                               resolution, n_iterations, n_groups, node_weights,
                               trials, steps) {
  # `weights = NULL` uses the `weight` edge column when it exists (as igraph
  # does), `NA` ignores it, a numeric vector is used as it is.
  edge_names <- igraph::edge_attr_names(graph)
  if (is.null(weights)) {
    weights <- if ("weight" %in% edge_names) igraph::E(graph)$weight else NULL
  } else if (length(weights) == 1 && is.na(weights)) {
    weights <- NA
  }
  if (!is.null(node_weights)) {
    node_weights <- node_table(graph)[[node_weights]]
  }
  communities <- switch(
    clustering_method,
    leiden = igraph::cluster_leiden(
      graph,
      objective_function = objective_function,
      weights = weights,
      resolution = resolution,
      n_iterations = n_iterations,
      vertex_weights = node_weights
    ),
    louvain = igraph::cluster_louvain(graph, weights = weights),
    fast_greedy = igraph::cluster_fast_greedy(graph, weights = weights),
    infomap = igraph::cluster_infomap(graph, e.weights = weights, v.weights = node_weights, nb.trials = trials),
    walktrap = igraph::cluster_walktrap(graph, weights = weights, steps = steps)
  )
  if (!is.null(n_groups) && clustering_method %in% c("fast_greedy", "walktrap")) {
    return(as.integer(igraph::cut_at(communities, no = n_groups)))
  }
  as.integer(igraph::membership(communities))
}

# Renumber clusters by decreasing size (1 = largest), with the rule of
# tidygraph's group_*() functions (ties keep the order of the identifiers).
rank_by_size <- function(membership) {
  sizes <- sort(table(membership), decreasing = TRUE)
  match(membership, as.integer(names(sizes)))
}

# Detect the clusters of one graph and add the node and edge columns.
detect_cluster <- function(graph, weights, clustering_method, objective_function,
                           resolution, n_iterations, n_groups, node_weights,
                           trials, steps) {
  membership <- cluster_membership(
    graph, weights, clustering_method, objective_function, resolution,
    n_iterations, n_groups, node_weights, trials, steps
  )
  membership <- sprintf("%02d", rank_by_size(membership))
  size <- as.vector(table(membership)[membership]) / length(membership)

  cluster_col <- paste0("cluster_", clustering_method)
  size_col <- paste0("size_cluster_", clustering_method)
  graph <- graph %N>%
    dplyr::mutate(!!cluster_col := membership, !!size_col := size)

  # Edges: the cluster of each end, and the common cluster ("00" between
  # clusters), so that within-cluster edges can take their cluster's colour.
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  cluster_from <- membership[ends[, 1]]
  cluster_to <- membership[ends[, 2]]
  graph %E>%
    dplyr::mutate(
      !!paste0(cluster_col, "_from") := cluster_from,
      !!paste0(cluster_col, "_to") := cluster_to,
      !!cluster_col := ifelse(cluster_from == cluster_to, cluster_from, "00")
    ) %>%
    tidygraph::activate("nodes")
}

# One summary message for one graph or a list of graphs.
report_clusters <- function(graphs, clustering_method) {
  cluster_col <- paste0("cluster_", clustering_method)
  size_col <- paste0("size_cluster_", clustering_method)
  n_clusters <- vapply(graphs, function(g) length(unique(node_table(g)[[cluster_col]])), integer(1))
  max_size <- vapply(graphs, function(g) max(node_table(g)[[size_col]]), numeric(1))
  if (length(graphs) == 1) {
    cli::cli_inform(c(
      "i" = "{.val {clustering_method}} found {n_clusters} cluster{?s}; the largest gathers {round(100 * max_size, 1)}% of the nodes."
    ))
  } else {
    cli::cli_inform(c(
      "i" = "{.val {clustering_method}} found {min(n_clusters)} to {max(n_clusters)} clusters per graph ({length(graphs)} graphs)."
    ))
  }
}
