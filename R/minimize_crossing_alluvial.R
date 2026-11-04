#' Order the clusters of an alluvial to reduce crossing flows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' In an alluvial plot, flows between clusters drawn far apart cross many
#' other flows and make the plot unreadable. `minimize_crossing_alluvial()`
#' computes an order of the clusters that places clusters exchanging many
#' nodes next to each other. It is a heuristic, not an optimal solution to
#' the crossing problem.
#'
#' The order is built in two steps. First, the function builds a network of
#' clusters: two clusters are tied when nodes move from one to the other
#' between windows, with a cosine weight
#' \eqn{n_{ab} / \sqrt{\bar{s}_a \bar{s}_b}}, \eqn{n_{ab}} being the number
#' of moving nodes and \eqn{\bar{s}_a} the mean size of cluster \eqn{a} over
#' its windows. It removes the ties one by one, from the weakest to the
#' strongest, and records at each step the connected component of each
#' cluster. Clusters are ordered by this sequence of components: clusters
#' that stay together until the strongest ties are removed end up side by
#' side (this is the order of the leaves of a single-linkage hierarchical
#' clustering of the clusters, computed with [stats::hclust()]). This groups
#' clusters that exchange nodes, but it does not count the crossings.
#'
#' Second (`refine = TRUE`, the default), the order is improved against the
#' crossings themselves. For each pair of consecutive windows, the flows are
#' the numbers of nodes going from a cluster of the first window to a cluster
#' of the second; two flows cross when the order of their origins and the
#' order of their destinations are opposite, and the pair is weighted by the
#' product of the two flows. Each cluster in turn is moved to the position that
#' minimises the total, until no move helps. This is a local search, which
#' can end in a local optimum, and which costs more with many clusters and
#' flows. For large alluvials (more than 100 clusters, or more than two million
#' pairs of flows), the barycentre heuristic is used
#' instead: each cluster is placed at the mean position of the clusters it
#' exchanges nodes with, as long as this reduces the crossings. Filtering the
#' small clusters first (see [networks_to_alluv()]) gives faster and more
#' readable alluvials.
#'
#' @param alluv_dt A data frame created with [networks_to_alluv()], whose rows
#'   follow the chronological order of the windows (the order of the levels of
#'   `window_column` if it is a factor, as in [networks_to_alluv()], or of
#'   their appearance).
#' @param intertemporal_cluster_column Character scalar, the column with the
#'   dynamic clusters.
#' @param node_id Character scalar, the column with the node identifiers.
#' @param window_column Character scalar, the column with the time windows
#'   (`"window"` by default).
#' @param refine Logical. If `TRUE` (default), refines the order against the
#'   crossings (see the description); if `FALSE`, keeps the order of the
#'   components.
#' @param max_passes Whole number, the maximum number of passes of the
#'   refinement over all the clusters.
#' @param verbose Logical. If `TRUE` (default), reports the weighted number of
#'   crossings before and after the refinement.
#'
#' @return `alluv_dt` with a new column `minimize_crossing_order`, the rank
#'   of each cluster, that [plot_alluvial()] uses to order the blocks.
#'
#' @details
#' The alluvial method (following clusters over time with `networks_to_alluv()`,
#' `minimize_crossing_alluvial()` and `plot_alluvial()`) was initiated by
#' Alexandre Truc.
#'
#' @examples
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   method = "similarity",
#'   edges_threshold = 1,
#'   verbose = FALSE
#' )
#' networks <- add_clusters(
#'   networks,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#' networks <- merge_dynamic_clusters(
#'   networks,
#'   cluster_id = "cluster_leiden",
#'   node_id = "source_id",
#'   method = "majority", threshold_similarity = 0.51,
#'   similarity_type = "partial"
#' )
#' alluv_dt <- networks_to_alluv(
#'   networks,
#'   intertemporal_cluster_column = "dynamic_cluster_leiden",
#'   node_id = "source_id"
#' )
#'
#' alluv_dt <- minimize_crossing_alluvial(
#'   alluv_dt,
#'   intertemporal_cluster_column = "dynamic_cluster_leiden",
#'   node_id = "source_id"
#' )
#' unique(alluv_dt[, c("dynamic_cluster_leiden", "minimize_crossing_order")])
#'
#' @export
minimize_crossing_alluvial <- function(alluv_dt ,
                                       intertemporal_cluster_column = NULL,
                                       node_id = NULL,
                                       window_column = "window",
                                       refine = TRUE,
                                       max_passes = 10,
                                       verbose = TRUE){


  . <- N <- Origin <- Source <- Window <- cl_id <- cosine_strength <- head <- link_strength <- minimize_crossing_order <- nodes <- nodes_id <- order_column_raw <- tot_window_leiden_Origin <- tot_window_leiden_Source <- weight <- cl <- node <- w <- step <- id <- NULL

  dt <- data.table::copy(alluv_dt)
  dt <- dt[, .SD, .SDcols = c(intertemporal_cluster_column, node_id, window_column)]
  dt[, c(intertemporal_cluster_column, node_id, window_column) := lapply(.SD, as.character), .SDcols = c(intertemporal_cluster_column, node_id, window_column)]
  data.table::setnames(dt, c(intertemporal_cluster_column, node_id, window_column), c("cl_id", "nodes_id", "Window"))

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Create a network of communities ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Edges
  edges_meta <- data.table::copy(dt)

  id_nb_cit <- edges_meta[, .N, .(cl_id, Window)]
  id_nb_cit <- id_nb_cit[, mean(N), cl_id] # we take the cluster size as the average cluster size across all window to weight links

  edges_meta[, Source:=cl_id, nodes_id]
  edges_meta[, Origin:=shift(cl_id), nodes_id] # for each time window we have the community of the node, and the community of origin of the same node

  edges_meta <- edges_meta[, head(.SD,1), .(cl_id,nodes_id)] # we keep only one observation

  edges_meta <- edges_meta[Source > Origin, c("Origin", "Source") := list(Source, Origin)] # coupling_angle weighting
  edges_meta <- merge(edges_meta, id_nb_cit, by.x = "Origin", by.y = "cl_id", all.x = TRUE)
  data.table::setnames(edges_meta, "V1", "tot_window_leiden_Origin")
  edges_meta <- merge(edges_meta, id_nb_cit, by.x = "Source", by.y = "cl_id", all.x = TRUE)
  data.table::setnames(edges_meta, "V1", "tot_window_leiden_Source")

  edges_meta[,link_strength := .N, .(Source,Origin)]

  edges_meta <- edges_meta[is.na(Origin) == FALSE & Source != Origin] # we keep only links between communities and one with origin

  edges_meta[, cosine_strength:=link_strength/sqrt(tot_window_leiden_Origin*tot_window_leiden_Source)]
  edges_meta[, weight:=max(cosine_strength), .(Source,Origin)] # between each communities, we keep the strongest link that exists between them at any given time.
  edges_meta <- edges_meta[, .N, .(Source,Origin,weight)][order(-N)]

  # Nodes
  nodes_meta <- dt[,.N,cl_id]


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Find an order ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Removing the links from the weakest to the strongest and following the
  # connected components is a single-linkage hierarchical clustering of the
  # clusters (two clusters are in the same component at a given step if a
  # chain of links stronger than the step joins them). Its dendrogram gives
  # the first order: clusters that stay together until the strongest links
  # are removed are side by side. It is computed in one call to hclust(),
  # instead of one graph per weight.
  cluster_ids <- as.character(nodes_meta$cl_id)
  if (length(cluster_ids) > 2 && nrow(edges_meta) > 0) {
    max_weight <- max(edges_meta$weight)
    distance <- matrix(max_weight + 2, length(cluster_ids), length(cluster_ids),
                       dimnames = list(cluster_ids, cluster_ids)) # no link: farther than any link
    link_index <- cbind(match(edges_meta$Source, cluster_ids), match(edges_meta$Origin, cluster_ids))
    distance[link_index] <- max_weight + 1 - edges_meta$weight
    distance[link_index[, 2:1, drop = FALSE]] <- max_weight + 1 - edges_meta$weight
    diag(distance) <- 0
    clusters <- cluster_ids[stats::hclust(stats::as.dist(distance), method = "single")$order]
  } else {
    clusters <- cluster_ids
  }
  position <- stats::setNames(seq_along(clusters), clusters)

  # Then the order is refined to reduce the crossings themselves.
  flows <- alluvial_flows(alluv_dt, intertemporal_cluster_column, node_id, window_column)
  crossings_before <- weighted_crossings(position, flows)
  if (refine && length(clusters) > 2 && nrow(flows$table) > 1) {
    position <- refine_crossing_order(position, flows, max_passes)
  }
  crossings_after <- weighted_crossings(position, flows)
  if (verbose) {
    cli::cli_inform(c("i" = "Weighted crossings of the flows between windows: {round(crossings_before)} with the order of the links, {round(crossings_after)} {if (refine) 'after the refinement' else '(no refinement)'}."))
  }

  community_order <- data.table::data.table(cl_id = names(position), minimize_crossing_order = as.integer(position))
  data.table::setnames(community_order, c("cl_id"), c(intertemporal_cluster_column))

  alluv_dt <- alluv_dt[, setdiff(names(alluv_dt), "minimize_crossing_order"), with = FALSE]
  alluv_dt_meta <- merge(alluv_dt, community_order[, .SD, .SDcols = c(intertemporal_cluster_column, "minimize_crossing_order")], by = intertemporal_cluster_column, all.x = TRUE)

  return(alluv_dt_meta)
}

# The flows of an alluvial: for each pair of consecutive windows, the number of
# nodes that go from a cluster of the first window to a cluster of the second.
# Windows are taken in the order of the factor levels, or of appearance.
# Returns the flows as two parallel tables: `from` and `to` (clusters, by
# step) and the number of nodes `n`, and the pairs of flows of the same step
# that can cross, with the product of their sizes.
alluvial_flows <- function(alluv_dt, cluster_column, node_id, window_column, max_pairs = 2e6) {
  cl <- node <- w <- step <- id <- from <- to <- NULL
  dt <- data.table::as.data.table(alluv_dt)[, .SD, .SDcols = c(cluster_column, node_id, window_column)]
  data.table::setnames(dt, c(cluster_column, node_id, window_column), c("cl", "node", "w"))
  dt[, cl := as.character(cl)]
  windows <- if (is.factor(dt$w)) levels(droplevels(dt$w)) else unique(as.character(dt$w))
  dt[, w := as.character(w)]
  steps <- lapply(seq_along(windows)[-1], function(i) {
    flow <- data.table::merge.data.table(
      dt[w == windows[i - 1], list(node, from = cl)],
      dt[w == windows[i], list(node, to = cl)],
      by = "node"
    )
    flow[, list(n = .N), by = list(from, to)][, step := i]
  })
  flows <- data.table::rbindlist(steps)
  if (nrow(flows) == 0) {
    return(list(table = flows, pairs = data.frame(i = integer(0), j = integer(0), weight = numeric(0))))
  }
  flows[, id := .I]
  # Pairs of flows of the same step (the flows of different steps never cross).
  # The table of pairs makes the local search fast, but its size is the square
  # of the number of flows: beyond `max_pairs`, it is not built, and the
  # crossings are counted step by step (see weighted_crossings()).
  n_pairs <- sum(choose(flows[, .N, by = step]$N, 2))
  if (n_pairs > max_pairs) {
    return(list(table = flows, pairs = NULL))
  }
  pairs <- lapply(split(flows, by = "step"), function(f) {
    if (nrow(f) < 2) return(NULL)
    combos <- utils::combn(f$id, 2)
    data.frame(i = combos[1, ], j = combos[2, ], weight = f$n[match(combos[1, ], f$id)] * f$n[match(combos[2, ], f$id)])
  })
  pairs <- do.call(rbind, pairs)
  if (is.null(pairs)) pairs <- data.frame(i = integer(0), j = integer(0), weight = numeric(0))
  list(table = flows, pairs = pairs)
}

# Number of crossings of the flows, each pair of crossing flows weighted by
# the product of their sizes. `position` is the rank of each cluster.
weighted_crossings <- function(position, flows) {
  pairs <- flows$pairs
  if (is.null(pairs)) {
    # step by step, without the table of pairs (large alluvials)
    total <- 0
    for (f in split(flows$table, by = "step")) {
      if (nrow(f) < 2) next
      from <- position[f$from]
      to <- position[f$to]
      crossing <- outer(from, from, "-") * outer(to, to, "-") < 0
      total <- total + sum(outer(f$n, f$n)[crossing]) / 2
    }
    return(total)
  }
  if (nrow(pairs) == 0) return(0)
  from <- position[flows$table$from]
  to <- position[flows$table$to]
  crossing <- (from[pairs$i] - from[pairs$j]) * (to[pairs$i] - to[pairs$j]) < 0
  sum(pairs$weight[crossing])
}

# Local search on the order: each cluster in turn is moved to the position that
# gives the fewest crossings, until no move helps (or `max_passes` passes). It
# finds a local optimum, not the optimum of the crossing problem.
refine_crossing_order <- function(position, flows, max_passes = 10) {
  pairs <- flows$pairs
  # the local search costs about the square of the number of clusters times
  # the number of pairs: beyond 100 clusters (or without the table of pairs),
  # the barycentre heuristic is used
  if (is.null(pairs) || length(position) > 100) return(barycentre_order(position, flows, max_passes))
  if (nrow(pairs) == 0) return(position)
  table <- flows$table
  # Moving a cluster only changes the crossings of the pairs of flows that
  # involve it (as origin or destination of one of the two flows): only these
  # pairs are evaluated for each candidate position.
  pair_clusters <- cbind(table$from[pairs$i], table$to[pairs$i], table$from[pairs$j], table$to[pairs$j])
  involved <- lapply(names(position), function(cluster) which(rowSums(pair_clusters == cluster) > 0))
  names(involved) <- names(position)
  crossings_of <- function(position, idx) {
    from <- position[table$from]
    to <- position[table$to]
    p <- pairs[idx, , drop = FALSE]
    sum(p$weight[(from[p$i] - from[p$j]) * (to[p$i] - to[p$j]) < 0])
  }
  for (pass in seq_len(max_passes)) {
    improved <- FALSE
    for (cluster in names(sort(position))) {
      idx <- involved[[cluster]]
      if (length(idx) == 0) next
      current <- crossings_of(position, idx)
      others <- setdiff(names(sort(position)), cluster)
      candidates <- vapply(0:length(others), function(k) {
        candidate <- append(others, cluster, after = k)
        crossings_of(stats::setNames(seq_along(candidate), candidate)[names(position)], idx)
      }, numeric(1))
      if (min(candidates) < current - 1e-9) {
        candidate <- append(others, cluster, after = which.min(candidates) - 1)
        position <- stats::setNames(seq_along(candidate), candidate)[names(position)]
        improved <- TRUE
      }
    }
    if (!improved) break
  }
  position
}

# For large alluvials (too many pairs of flows for the local search): the
# barycentre heuristic of crossing reduction. Each cluster is placed at the
# mean position of the clusters it exchanges nodes with, weighted by the
# flows; the new order is kept as long as it reduces the crossings.
barycentre_order <- function(position, flows, max_passes = 10) {
  cl <- other <- n <- pos <- NULL
  table <- flows$table
  partners <- data.table::rbindlist(list(
    data.table::data.table(cl = table$from, other = table$to, n = table$n),
    data.table::data.table(cl = table$to, other = table$from, n = table$n)
  ))
  best <- weighted_crossings(position, flows)
  for (pass in seq_len(max_passes)) {
    partners[, pos := position[other]]
    barycentre <- partners[, list(b = sum(n * pos) / sum(n)), by = cl]
    target <- position
    target[barycentre$cl] <- barycentre$b
    new_order <- names(target)[order(target, position)]
    candidate <- stats::setNames(seq_along(new_order), new_order)[names(position)]
    value <- weighted_crossings(candidate, flows)
    if (value >= best) break
    position <- candidate
    best <- value
  }
  position
}
