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
  #' The column with the identifier of the inter-temporal cluster. By default, "intertemporal_name", as it is
  #' the name of the column created with [intertemporal_cluster_naming()][networkflow::intertemporal_cluster_naming()].
  #'
  #' @param node_key
  #' The column with the unique identifier of each node.
  #'
  #' @param summary_cl_stats
  #' If set to TRUE, the data.frame will contain a list of variable that summarize cluster statistics of the alluvial. These variables
  #' can be particularly useful to filter smaller communities when plotting according to different variables:
  #'
  #' - share_cl_alluv is the share of a given cluster across all time windows;
  #' - share_cl_window is the share of a given cluster in a given time window;
  #' - share_cl_max is the highest value of share_cl_window for a given cluster across all individual time windows;
  #' - length_cl is the number of time windows a cluster exists.
  #'
  #'  #' @examples
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph

  . <- y_alluv <- Window <- share_cl_alluv <- intertemporal_name <- tot_window_leiden <- tot_window <- share_cl_window <- share_cl_max <- head <- N <- nodes <- NULL

  networks <- lapply(list_graph, function(tbl)(tbl %N>% as.data.table))
  networks <- lapply(networks, function(dt)(dt[, .SD, .SDcols = c(node_key, intertemporal_cluster_column)]))

  alluv_dt <- rbindlist(networks, idcol = "Window")
#  alluv_dt[,y_alluv:=1/.N, Window]

  if(summary_cl_stats == TRUE){
    alluv_dt[,share_cl_alluv:=.N/alluv_dt[,.N], intertemporal_cluster_column, env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # share of cl in alluv
    alluv_dt[,tot_window_leiden := .N, .(Window,intertemporal_cluster_column), env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    alluv_dt[,tot_window := .N, .(Window)]
    alluv_dt[,share_cl_window := tot_window_leiden/tot_window] # share of cl in time window
    alluv_dt[,share_cl_max := max(share_cl_window), intertemporal_cluster_column, env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # max share of cl in all time windows
    alluv_dt[,tot_window_leiden := NULL]
    alluv_dt[,tot_window := NULL]

    n_years <- alluv_dt[, head(.SD, 1), .(Window, intertemporal_cluster_column), env = list(intertemporal_cluster_column = intertemporal_cluster_column)][,.N, intertemporal_cluster_column, env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    n_years <- n_years %>% rename(length_cl = N)
    alluv_dt <- merge(alluv_dt, n_years, by = c(intertemporal_cluster_column), all.x = TRUE) %>%  # length of cl
      .[order(Window)]
  }

  return (alluv_dt)
}
