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
#' The function builds a network of clusters: two clusters are tied when
#' nodes move from one to the other between windows, with a cosine weight
#' \eqn{n_{ab} / \sqrt{\bar{s}_a \bar{s}_b}}, \eqn{n_{ab}} being the number
#' of moving nodes and \eqn{\bar{s}_a} the mean size of cluster \eqn{a} over
#' its windows. It then removes the ties one by one, from the weakest to the
#' strongest, and records at each step the connected component of each
#' cluster. Clusters are ordered by this sequence of components: clusters
#' that stay together until the strongest ties are removed end up side by
#' side.
#'
#' @param alluv_dt A data frame created with [networks_to_alluv()].
#' @param intertemporal_cluster_column Character scalar, the column with the
#'   dynamic clusters.
#' @param node_id Character scalar, the column with the node identifiers.
#' @param window_column Character scalar, the column with the time windows
#'   (`"window"` by default).
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
#'   cooccurrence_method = "coupling_similarity",
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
#'   threshold_similarity = 0.51,
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
                                       window_column = "window"){


  . <- N <- Origin <- Source <- Window <- cl_id <- cosine_strength <- fct_reorder <- head <- link_strength <- minimize_crossing_order <- nodes <- nodes_id <- order_column_raw <- tot_window_leiden_Origin <- tot_window_leiden_Source <- weight <- NULL

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

  community_order <- components[,order_column_raw := do.call(paste0, .SD), .SDcols = columns_to_paste]
  community_order <- community_order[,.(cl_id,order_column_raw)][order(order_column_raw)]
  community_order[,minimize_crossing_order:=seq(1:nrow(community_order))] # give order as simple number
  data.table::setnames(community_order, c("cl_id"), c(intertemporal_cluster_column))

  alluv_dt_meta <- merge(alluv_dt, community_order[, .SD, .SDcols = c(intertemporal_cluster_column, "minimize_crossing_order")], by = intertemporal_cluster_column, all.x = TRUE)

 # alluv_dt_meta[[intertemporal_cluster_column]] <- forcats::fct_reorder(alluv_dt_meta[[intertemporal_cluster_column]], alluv_dt_meta[["minimize_crossing_order"]], min, .desc = TRUE)

  return(alluv_dt_meta)
}
