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
#' side. This groups clusters that exchange nodes, but it does not count the
#' crossings.
#'
#' Second (`refine = TRUE`, the default), the order is improved against the
#' crossings themselves. For each pair of consecutive windows, the flows are
#' the numbers of nodes going from a cluster of the first window to a cluster
#' of the second; two flows cross when the order of their origins and the
#' order of their destinations are opposite, and the pair is weighted by the
#' product of the two flows. Each cluster in turn is moved to the position that
#' minimises the total, until no move helps. This is a local search, which
#' can end in a local optimum, and which costs more with many clusters and
#' flows.
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

  # Network
  tbl_meta <- tidygraph::tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "cl_id")

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Find an order ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # First we extract the different components, we want to keep stratum with the same
  components <- tbl_meta %N>%
    dplyr::mutate(components_att = tidygraph::group_components(type = "weak")) %>%
    data.table::as.data.table()

  data.table::setnames(components, "components_att", paste0("components_att_","0"))
  components <- components[,.(cl_id, "components_att_0"= get("components_att_0"))] # the column components_att_0 give us the first order of statums

  for (links_to_remove in unique(edges_meta[order(weight)]$weight)) { # then we remove each the weakeast link, and create a new column, with a new order for each components
    edges_meta <- edges_meta[weight > links_to_remove] # remove link of this treshold

    tbl_meta <- tidygraph::tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "cl_id")

    components2 <- tbl_meta %N>%
      dplyr::mutate(components_att = tidygraph::group_components(type = "weak")) %>%
      data.table::as.data.table()

    name <- paste0("components_att_", links_to_remove)
    data.table::setnames(components2, "components_att", name)
    components2 <- components2[, .SD, .SDcols = c("cl_id", name)]
    components <- merge(components, components2, all.x = TRUE, all.y = TRUE, by= "cl_id")

  }

  columns_to_paste <- names(components)
  columns_to_paste <- columns_to_paste[columns_to_paste != "cl_id"]

  # The components at each step of the removal of the links give a first
  # order: clusters that stay in the same component the longest are side by
  # side. The component numbers are compared as numbers, one step after the
  # other (they were once pasted into a string, which mixes "1" "10" and
  # "11" "0" as soon as there are ten components).
  starting_order <- do.call(order, unname(as.list(components[, .SD, .SDcols = columns_to_paste])))
  clusters <- components$cl_id[starting_order]
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
alluvial_flows <- function(alluv_dt, cluster_column, node_id, window_column) {
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
  # pairs of flows of the same step (the flows of different steps never cross)
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
  best <- weighted_crossings(position, flows)
  for (pass in seq_len(max_passes)) {
    improved <- FALSE
    for (cluster in names(sort(position))) {
      others <- setdiff(names(sort(position)), cluster)
      candidates <- vapply(0:length(others), function(k) {
        candidate <- append(others, cluster, after = k)
        weighted_crossings(stats::setNames(seq_along(candidate), candidate)[names(position)], flows)
      }, numeric(1))
      if (min(candidates) < best - 1e-9) {
        k <- which.min(candidates) - 1
        candidate <- append(others, cluster, after = k)
        position <- stats::setNames(seq_along(candidate), candidate)[names(position)]
        best <- min(candidates)
        improved <- TRUE
      }
    }
    if (!improved) break
  }
  position
}
