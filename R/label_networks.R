label_networks <- function(graphs = NA,
                           x_coordinates_column = "x",
                           y_coordinates_column = "y",
                           intertemporal_cluster_label_column = "intertemporal_name")
{
  #' Create a label column suitable for plotting labels on an alluvial graph
  #'
  #' @description
  #' Simple function to create a label column that make it easy to center label around particular clusters on the alluvial graph of intertemporal networks.
  #'
  #' @param graphs
  #' A network or a list of networks.
  #'
  #' @param x_coordinates_column
  #' The column with the x coordinates of your nodes.
  #'
  #' @param y_coordinates_column
  #' The column with the y coordinates of your nodes.
  #'
  #' @param intertemporal_cluster_label_column
  #' The column with the identifier or label of the inter-temporal cluster. By default, "intertemporal_name", as it is
  #' the name of the column created with [intertemporal_cluster_naming()][networkflow::intertemporal_cluster_naming()].
  #' If you want your label to be something other than cluster names, you can also use this function for any kind of variables in your nodes table.
  #'
  #' @export
  #'
  . <- nodes <- label_x <- x <- label_y <- y <- head <- label_column <- intertemporal_name <- NULL

  network_level_label <- function(graphs_l = graphs,
                                  x_coordinates_column_l = x_coordinates_column,
                                  y_coordinates_column_l = y_coordinates_column,
                                  intertemporal_cluster_label_column_l = intertemporal_cluster_label_column){

    label_com <- graphs_l %N>%
      data.table::as.data.table()
    label_com <- label_com[, label_x := mean(x_coordinates_column_l), intertemporal_cluster_label_column_l,
                           env = list(intertemporal_cluster_label_column_l = intertemporal_cluster_label_column_l, x_coordinates_column_l = x_coordinates_column_l)]
    label_com <- label_com[, label_y := mean(y_coordinates_column_l), intertemporal_cluster_label_column_l,
                           env = list(intertemporal_cluster_label_column_l = intertemporal_cluster_label_column_l, y_coordinates_column_l = y_coordinates_column_l)]
    label_com <- label_com[, head(.SD, 1), intertemporal_cluster_label_column_l,
                           env = list(intertemporal_cluster_label_column_l = intertemporal_cluster_label_column_l)]

    label_com[, c("label_column") := intertemporal_cluster_label_column_l,
              env = list(intertemporal_cluster_label_column_l = intertemporal_cluster_label_column_l)]
    label_com <- label_com[, .SD, .SDcol = c(intertemporal_cluster_label_column_l,"label_column", "label_x", "label_y")]

    graphs_l <- graphs_l %N>%
      dplyr::left_join(label_com, by = intertemporal_cluster_label_column_l)
  }


  if(inherits(graphs, "list")){
    graphs <- lapply(graphs, network_level_label)

  } else{
    if(inherits(graphs, "tbl_graph")){
      graphs <- list(graphs)
      graphs <- lapply(graphs, network_level_label)
      graphs <- graphs[[1]]

    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }

  return(graphs)
}
