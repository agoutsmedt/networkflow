plot_networks <- function(graphs = NA,
                          networkflow_label_column = TRUE,
                          label_column = NA,
                          x_coordinates_column = "x",
                          y_coordinates_column = "y",
                          color_nodes_column = "color",
                          color_edges_column  = "color_edges",
                          weight_column = "weight",
                          node_size_column = "size",
                          time_variable = NA)
{
  #' Plot your network or list of networks after having followed the workflow of networkflow.
  #'
  #' @description
  #' An easy way to plot the your networks by using the columns created by the different function of networkflow.
  #'
  #' @param graphs
  #' A network or a list of networks.
  #'
  #' @param networkflow_label_column
  #' Do you want to use the label created with the [label_networks()][networkflow::label_networks()] function ? TRUE or FALSE.
  #'
  #' @param label_column
  #' If networkflow_label_column is TRUE, this parameters is not used. If you want to plot labels for individual nodes you can use this parameters to chose the column to use as label.
  #'
  #' @param x_coordinates_column
  #' The column with the x coordinates of your nodes.
  #'
  #' @param y_coordinates_column
  #' The column with the y coordinates of your nodes.
  #'
  #' @param color_nodes_column
  #' The column with the colors of your nodes.
  #'
  #' @param color_edges_column
  #' The column with the colors of your edges.
  #'
  #' @param weight_column
  #' The column with the weight of edges.
  #'
  #' @param node_size_column
  #' The column with the size of nodes.
  #'
  #' @param time_variable
  #' The column with the temporal variable of your individual nodes. Will only be used for the title of the plot.
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph
  #' @import dplyr
  #' @import magrittr
  #' @import ggplot2
  #' @importFrom ggrepel geom_label_repel

  . <- network_level_plot <- label_x <- label_y <- NULL

  x_coordinates_column_plot <- ensym(x_coordinates_column)
  y_coordinates_column_plot <- ensym(y_coordinates_column)
  color_nodes_column_plot <- ensym(color_nodes_column)
  color_edges_column_plot <- ensym(color_edges_column)
  weight_column_plot <- ensym(weight_column)
  node_size_column_plot <- ensym(node_size_column)
  label_column_plot <- ensym(label_column)



  list_ggplot <- list()

  if(inherits(graphs, "list")){
    graphs <- graphs
  } else {
    if(inherits(graphs, "tbl_graph")){
      graphs <- list(graphs)
      graphs <- lapply(graphs, network_level_plot)
    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }

  # We plot using a loop
  for (elements in as.character(names(graphs))) {
    graphs_l <- graphs[[as.character(elements)]]
    graphs_l_nodes <- graphs_l %N>% as.data.table()
    max_time <- max(graphs_l_nodes[[time_variable]])
    min_time <- min(graphs_l_nodes[[time_variable]])

    if(networkflow_label_column==TRUE){

      # the part where we add the necessary labels
      label_com <- graphs_l %N>% as.data.table()
      label_com <- label_com[,.N,.(label_column, label_x, label_y, color_nodes_column), env = list(color_nodes_column = color_nodes_column_plot)]

      list_ggplot[[paste0(elements)]] <- ggraph(graphs_l, "manual", x = !!x_coordinates_column_plot, y = !!y_coordinates_column_plot) +
        geom_edge_arc(aes(color = !!color_edges_column_plot, width = !!weight_column_plot), alpha = 0.5, strength = 0.2) +
        geom_node_point(aes(fill = !!color_nodes_column_plot, size = !!node_size_column_plot), pch=21) +
        scale_edge_width_continuous(range = c(0.5, 1)) +
        theme_graph() +
        ggrepel::geom_label_repel(data = label_com, aes(x = label_x, y = label_y, label = as.character(label_column), fill = !!color_nodes_column_plot)) +
        theme(legend.position = "none") +
        scale_fill_identity() +
        scale_edge_colour_identity() +
        labs(title = paste0(as.character(min_time),"-",as.character(max_time)))
    } else {
      list_ggplot[[paste0(elements)]] <- ggraph(graphs_l, "manual", x = !!x_coordinates_column_plot, y = !!y_coordinates_column_plot) +
        geom_edge_arc(aes(color = !!color_edges_column_plot, width = !!weight_column_plot), alpha = 0.5, strength = 0.2) +
        geom_node_point(aes(fill = !!color_nodes_column_plot, size = !!node_size_column_plot), pch=21) +
        scale_edge_width_continuous(range = c(0.5, 1)) +
        theme_graph() +
        geom_node_text(aes(label = !!label_column_plot, size = !!node_size_column_plot)) +
        theme(legend.position = "none") +
        scale_fill_identity() +
        scale_edge_colour_identity() +
        labs(title = paste0(as.character(min_time),"-",as.character(max_time)))
    }
  }

  if(inherits(graphs, "tbl_graph")){
    list_ggplot <- list_ggplot[[1]]}

  return(list_ggplot)
}
