networks_to_alluv <- function(list_graph = NA,
                              intertemporal_cluster_column = "intertemporal_name",
                              node_key = NA,
                              summary_cl_stats = FALSE){

  #' Create a data.frame suitable for alluvial graph projection
  #'
  #' This function creates a data.frame that can be easily plotted with ggalluvial from a list of networks.
  #'
  #' @param list_graph
  #' Your list with all networks
  #'
  #' @param intertemporal_cluster_column
  #' The column with the identifier of the inter-temporal cluster. If you have used
  #' [add_clusters()][networkflow::add_clusters()] and [merge_dynamic_clusters()][networkflow::merge_dynamic_clusters()],
  #' it is of the form `dynamic_cluster_{clustering_method}`.
  #'
  #' @param node_key
  #' The column with the unique identifier of each node.
  #'
  #' @param summary_cl_stats
  #' If set to TRUE, the data.frame will contain a list of variable that summarize cluster statistics of the alluvial. These variables
  #' can be particularly useful to filter smaller communities when plotting according to different variables:
  #'
  #' - `share_cluster_alluv` is the percentage share of a given cluster across all time windows;
  #' -  `share_cluster_window` is the percentage share of a given cluster in a given time window;
  #' - `share_clusert_max` is the highest value of `share_cluster_window` for a given cluster across all individual time windows;
  #' - `length_cluster` is the number of time windows a cluster exists.
  #'
  #'  #' @examples
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph

  . <- y_alluv <- Window <- share_cluster_alluv <- intertemporal_name <- tot_window_leiden <- tot_window <- share_cluster_window <- share_cluster_max <- head <- N <- nodes <- NULL

  networks <- lapply(list_graph,
                     function(tbl)(tbl %N>% as.data.table))
  networks <- lapply(networks,
                     function(dt)(dt[, .SD, .SDcols = c(node_key, intertemporal_cluster_column)]))

  alluv_dt <- rbindlist(networks, idcol = "Window")
#  alluv_dt[,y_alluv:=1/.N, Window]

  if(summary_cl_stats == TRUE){
    alluv_dt[,share_cluster_alluv:= round(.N/alluv_dt[,.N], 4) * 100, intertemporal_cluster_column,
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # share of cl in alluv
    alluv_dt[,tot_window_leiden := .N, .(Window,intertemporal_cluster_column),
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    alluv_dt[,tot_window := .N, .(Window)]
    alluv_dt[,share_cluster_window := round(tot_window_leiden/tot_window, 4) * 100] # share of cl in time window
    alluv_dt[,share_cluster_max := max(share_cluster_window), intertemporal_cluster_column,
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # max share of cl in all time windows
    alluv_dt[,tot_window_leiden := NULL]
    alluv_dt[,tot_window := NULL]

    n_years <- alluv_dt[, head(.SD, 1), .(Window, intertemporal_cluster_column),
                        env = list(intertemporal_cluster_column = intertemporal_cluster_column)][,.N, intertemporal_cluster_column, env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    n_years <- n_years %>% rename(length_cluster = N)
#    setkey(c(intertemporal_cluster_column))
    alluv_dt <- merge(alluv_dt,
                      n_years,
                      by = c(intertemporal_cluster_column),
                      all.x = TRUE) %>%  # length of cl
      .[order(Window)]
  }
  alluv_dt[, y_alluv :=1/.N, Window]
  return (alluv_dt)
}

