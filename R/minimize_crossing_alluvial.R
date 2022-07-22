minimize_crossing_alluvial <- function(alluv_dt = NA,
                                       intertemporal_cluster_column = "intertemporal_name",
                                       node_key = NA,
                                       window_column = "Window"){

  #' Order the rectangles of the alluvial in a way that minimize crossing of flow
  #'
  #' This function transform the node_key into a factor ordered by a newly created column named "minimize_crossing_order".
  #'
  #' @param alluv_dt
  #' Data.frame of the alluvial created using the networkflow::networks_to_alluv function
  #'
  #' @param intertemporal_cluster_column
  #' The column with the identifier of the inter-temporal cluster. By default, "intertemporal_name", as it is
  #' the name of the column created with [intertemporal_cluster_naming()][networkflow::intertemporal_cluster_naming()].
  #'
  #' @param node_key
  #' The column with the unique identifier of each node. This is the alluvium of the alluvial.
  #'
  #' @param window_column
  #' The column with your time windows.
  #'
  #'  #' @examples
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph
  #' @importFrom forcats fct_reorder

  . <- N <- Origin <- Source <- Window <- cl_id <- cosine_strength <- fct_reorder <- head <- link_strength <- minimize_crossing_order <- nodes <- nodes_id <- order_column_raw <- tot_window_leiden_Origin <- tot_window_leiden_Source <- weight <- NULL

  dt <- copy(alluv_dt)
  dt <- dt[, .SD, .SDcols = c(intertemporal_cluster_column, node_key, window_column)]
  dt[, c(intertemporal_cluster_column, node_key, window_column) := lapply(.SD, as.character), .SDcols = c(intertemporal_cluster_column, node_key, window_column)]
  setnames(dt, c(intertemporal_cluster_column, node_key, window_column), c("cl_id", "nodes_id", "Window"))

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Create a network of communities ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Edges
  edges_meta <- copy(dt)

  id_nb_cit <- edges_meta[, .N, .(cl_id, Window)]
  id_nb_cit <- id_nb_cit[, mean(N), cl_id] # we take the cluster size as the average cluster size across all window to weight links

  edges_meta[, Source:=cl_id, nodes_id]
  edges_meta[, Origin:=shift(cl_id), nodes_id] # for each time window we have the community of the node, and the community of origin of the same node

  edges_meta <- edges_meta[, head(.SD,1), .(cl_id,nodes_id)] # we keep only one observation

  edges_meta <- edges_meta[Source > Origin, c("Origin", "Source") := list(Source, Origin)] # coupling_angle weighting
  edges_meta <- merge(edges_meta, id_nb_cit, by.x = "Origin", by.y = "cl_id", all.x = TRUE)
  setnames(edges_meta, "V1", "tot_window_leiden_Origin")
  edges_meta <- merge(edges_meta, id_nb_cit, by.x = "Source", by.y = "cl_id", all.x = TRUE)
  setnames(edges_meta, "V1", "tot_window_leiden_Source")

  edges_meta[,link_strength := .N, .(Source,Origin)]

  edges_meta <- edges_meta[is.na(Origin) == FALSE & Source != Origin] # we keep only links between communities and one with origin

  edges_meta[, cosine_strength:=link_strength/sqrt(tot_window_leiden_Origin*tot_window_leiden_Source)]
  edges_meta[, weight:=max(cosine_strength), .(Source,Origin)] # between each communities, we keep the strongest link that exists between them at any given time.
  edges_meta <- edges_meta[, .N, .(Source,Origin,weight)][order(-N)]

  # Nodes
  nodes_meta <- dt[,.N,cl_id]

  # Network
  tbl_meta <- tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "cl_id")

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  #### Find an order ####
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # First we extract the different components, we want to keep stratum with the same
  components <- tbl_meta %N>%
    mutate(components_att = group_components(type = "weak")) %>%
    as.data.table()

  setnames(components, "components_att", paste0("components_att_","0"))
  components <- components[,.(cl_id, "components_att_0"= get("components_att_0"))] # the column components_att_0 give us the first order of statums

  for (links_to_remove in unique(edges_meta[order(weight)]$weight)) { # then we remove each the weakeast link, and create a new column, with a new order for each components
    edges_meta <- edges_meta[weight > links_to_remove] # remove link of this treshold

    tbl_meta <- tbl_graph(nodes = nodes_meta, edges = edges_meta, directed = FALSE, node_key = "cl_id")

    components2 <- tbl_meta %N>%
      mutate(components_att = group_components(type = "weak")) %>%
      as.data.table()

    name <- paste0("components_att_", links_to_remove)
    setnames(components2, "components_att", name)
    components2 <- components2[, .SD, .SDcols = c("cl_id", name)]
    components <- merge(components, components2, all.x = TRUE, all.y = TRUE, by= "cl_id")

  }

  columns_to_paste <- names(components)
  columns_to_paste <- columns_to_paste[columns_to_paste != "cl_id"]

  community_order <- components[,order_column_raw := do.call(paste0, .SD), .SDcols = columns_to_paste]
  community_order <- community_order[,.(cl_id,order_column_raw)][order(order_column_raw)]
  community_order[,minimize_crossing_order:=seq(1:nrow(community_order))] # give order as simple number
  setnames(community_order, c("cl_id"), c(intertemporal_cluster_column))

  alluv_dt_meta <- merge(alluv_dt, community_order[, .SD, .SDcols = c(intertemporal_cluster_column, "minimize_crossing_order")], by = intertemporal_cluster_column, all.x = TRUE)

  alluv_dt_meta[[intertemporal_cluster_column]] <- forcats::fct_reorder(alluv_dt_meta[[intertemporal_cluster_column]], alluv_dt_meta[["minimize_crossing_order"]], min, .desc = TRUE)

  return(alluv_dt_meta)
}
