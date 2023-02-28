label_alluvial <- function(alluv_dt = NA,
                           window_column = "Window",
                           intertemporal_cluster_label_column = "intertemporal_name")
{
  #' Create a label column suitable for plotting labels on an alluvial graph
  #'
  #' @description
  #' Simple function to create a label column that make it easy to center label around particular clusters on the alluvial graph of intertemporal networks.
  #'
  #' @param alluv_dt
  #' Data.frame of the alluvial created using the networkflow::networks_to_alluv function
  #'
  #' @param window_column
  #' The column with your time windows.
  #'
  #' @param intertemporal_cluster_label_column
  #' The column with the identifier or label of the inter-temporal cluster. By default, "intertemporal_name", as it is
  #' the name of the column created with [intertemporal_cluster_naming()][networkflow::intertemporal_cluster_naming()].
  #'
  #' @export

  . <- Window <- head <- x <- intertemporal_name_label <- N <- NULL

  label <- data.table::copy(alluv_dt)
  label <- label[,.N,.(intertemporal_cluster_label_column, window_column),
                 env = list(intertemporal_cluster_label_column = intertemporal_cluster_label_column, window_column = window_column)]
  label <- label[,Window:=round(mean(as.numeric(window_column))),intertemporal_cluster_label_column,
                 env = list(intertemporal_cluster_label_column = intertemporal_cluster_label_column, window_column = window_column)]
  label <- label[, head(.SD, 1), .(intertemporal_cluster_label_column),
                 env = list(intertemporal_cluster_label_column = intertemporal_cluster_label_column)]
  label[,N:=NULL]
  label[,intertemporal_name_label:=intertemporal_cluster_label_column,
        env = list(intertemporal_cluster_label_column = intertemporal_cluster_label_column) ]

  alluv_dt <- merge(alluv_dt, label, by=c(intertemporal_cluster_label_column, window_column), all.x = TRUE)

  return(alluv_dt)

}
