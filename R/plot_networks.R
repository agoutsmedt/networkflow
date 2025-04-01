
  #' Plot your Networks
  #'
  #' @description
  #' An easy way to plot your networks by using the columns created by the different function of networkflow.
  #'
  #' @param graphs
  #' A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/) or a list of tibble
  #' graphs.
  #'
  #' @param x,y
  #' The columns of your tibble graph or list of tibble graphs
  #' with the x and y coordinates of your nodes (for instance, the coordinates
  #' produce by [layout_networks()][networkflow::layout_networks()]).
  #'
  #' @param cluster_label_column
  #' The values of the column are taken to be displayed as labels.
  #' By default, "cluster_label", as it is
  #' the default name of the column created with [name_clusters()][networkflow::name_clusters()].
  #' But you may also use the column produced with [add_clusters()][networkflow::add_clusters()].
  #'
  #' @param node_size_column
  #' The column with numeric values to be used for the size of nodes. By default, "node_size",
  #' as the column created by [build_dynamic_networks()][networkflow::build_dynamic_networks()]
  #' or [build_network()][networkflow::build_network()] when `compute_size` set to `TRUE` in
  #' these functions. If `NULL`, the size of every nodes is set to 1.
  #'
  #'
  #' @param color_column
  #' The column with the colors associated to the  categories of `cluster_label_column`.
  #' By default, "color", as the result of [color_networks()][networkflow::color_networks()].
  #'
  #' @param color_networks
  #' If no color has been set for the categories of `cluster_label_column`, you
  #' can attribute them colors by setting `color_networks` to `TRUE`.
  #' [color_networks()][networkflow::color_networks()] will be used.
  #'
  #' @param color
  #' If `color_networks`is `TRUE`, the parameter `color` will be used to color the categories of the
  #' `cluster_label_column`. It may be a vector of colors (in a character format)
  #' or a two columns data frame with the first column as
  #' the distinct observations of the `cluster_label_column` and a second column with the
  #' vector of colors you want to use. If `NULL` colors will be automatically chosen by
  #' [color_networks()][networkflow::color_networks()].
  #'
  #' @param print_plot_code
  #' Set to `TRUE` if you want the ggplot2 code to be printing. It is useful if you are not
  #' totally satisfied of the plot and want to manipulate the code yourself.
  #'
  #' @details
  #' If you have not used [prepare_label_networks()][networkflow::prepare_label_networks()],
  #' the function will be automatically used to produce coordinates to each category of
  #' `cluster_label_column`.
  #'
  #' @export

plot_networks <- function(graphs,
                          x = "x",
                          y = "y",
                          cluster_label_column = "cluster_label",
                          node_size_column = "node_size",
                          color_column = "color",
                          color_networks = FALSE,
                          color = NULL,
                          print_plot_code = FALSE)
{
  . <- network_level_plot <- label_x <- label_y <- NULL


  if(inherits(graphs, "list")){
    if(! x %in% colnames(graphs[[1]] %N>% as.data.frame())){
      cli::cli_abort("The tibble graphs don't have any layout coordinates")
    }
    if(! color_column %in% colnames(graphs[[1]] %N>% as.data.frame()) & color_networks == FALSE){
      cli::cli_abort("The tibble graphs don't have any color.
                     Please set {.emph color_networks} to {.val TRUE} or use {.fun color_networks}.")
    }
    if(! cluster_label_column %in% colnames(graphs[[1]] %N>% as.data.frame()) | is.null(cluster_label_column)){
      cli::cli_abort("The tibble graphs don't have any cluster label to be plotted.
                     Please provide a column to {.emph cluster_label_column}.")
    }
    if(color_networks){
      cli::cli_alert_info("{.emph color_networks} is {.val TRUE}. {.fun color_networks} is used to color the networks, with {.emph cluster_label_column} used as {.emph column_to_color}.")
      graphs <- color_networks(graphs = graphs,
                               column_to_color = cluster_label_column,
                               color = color,
                               unique_color_across_list = FALSE)
    }
    if(! "label_x" %in% colnames(graphs[[1]] %N>% as.data.frame())){
      cli::cli_alert_info("No coordinates to plot labels. {.fun prepare_label_networks} is used automatically.")
      graphs <- prepare_label_networks(graphs,
                                       x = x,
                                       y = y,
                                       cluster_label_column = cluster_label_column)
    }
    if(print_plot_code){
      to_plot_code <- plot_network(graph = graphs[[1]],
                                   x = x,
                                   y = y,
                                   title = window,
                                   print_plot_code = TRUE,
                                   color_column = color_column,
                                   cluster_label_column = cluster_label_column,
                                   node_size_column = node_size_column)
    }
    for(window in names(graphs)){
      graphs[[window]]$plot <- plot_network(graph = graphs[[window]],
                                          x = x,
                                          y = y,
                                          title = window,
                                          print_plot_code = FALSE,
                                          color_column = color_column,
                                          cluster_label_column = cluster_label_column,
                                          node_size_column = node_size_column)
    }
    cli::cli_alert_info("The plots are registered in as a list element named {.val plot}.
                   You may access the plot by chosing the corresponding tibble graph in the list of tibble graphs.
                   For instance, you may access the first plot by typing {.code your_list[[1]]$plot}.")
    return(graphs)
  } else {
    if(inherits(graphs, "tbl_graph")){
      if(! x %in% colnames(graphs %N>% as.data.frame())){
        cli::cli_abort("The tibble graph doesn't have any layout coordinates")
      }
      if(! color_column %in% colnames(graphs %N>% as.data.frame()) & color_networks == FALSE){
        cli::cli_abort("The tibble graph doesn't have any color.
                     Please set {.emph color_networks} to {.val TRUE} or use {.fun color_networks}.")
      }
      if(! cluster_label_column %in% colnames(graphs %N>% as.data.frame()) | is.null(cluster_label_column)){
        cli::cli_abort("The tibble graph doesn't have any cluster label to be plotted.
                     Please provide a column to {.emph cluster_label_column}.")
      }
      if(color_networks){
        cli::cli_alert_info("{.emph color_networks} is {.val TRUE}. {.fun color_networks} is used to color the networks, with {.emph cluster_label_column} used as {.emph column_to_color}.")
        graphs <- color_networks(graphs = graphs,
                                 column_to_color = cluster_label_column,
                                 color = color,
                                 unique_color_across_list = FALSE)
      }
      if(! "label_x" %in% colnames(graphs %N>% as.data.frame())){
        cli::cli_alert_info("No coordinates to plot labels. {.fun prepare_label_networks} is used automatically.")
        graphs <- prepare_label_networks(graphs,
                                         x = x,
                                         y = y,
                                         cluster_label_column = cluster_label_column)
      }
      plot_network(graph = graphs,x = x,
                   y = y,
                   title = "",
                   color_column = color_column,
                   cluster_label_column = cluster_label_column,
                   node_size_column = node_size_column,
                   print_plot_code = print_plot_code)
    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }
}

plot_network <- function(graph,
                         x = x,
                         y = y,
                         title,
                         color_column = color_column,
                         cluster_label_column = cluster_label_column,
                         node_size_column = node_size_column,
                         print_plot_code = print_plot_code){
  . <- weight <- label_x <- label_y <- NULL

  if(! "weight" %in% colnames(graph %E>% as.data.frame())){
    cli::cli_alert_info("No column `weight` found in edges data. All weight will equal 1.")
    graph <- graph %E>%
      dplyr::mutate(weight = 1)
  }
  if(! node_size_column %in% colnames(graph %N>% as.data.frame()) | is.null(node_size_column)){
    cli::cli_alert_info("No column `weight` found in edges data. All weight will equal 1.")
    graph <- graph %N>%
      dplyr::mutate(node_size = 1)
    node_size_column <- "node_size"
    node_size_column_plot <- rlang::ensym(node_size_column)
  } else {
    node_size_column_plot <- rlang::ensym(node_size_column)
    }

  labels <- graph %N>%
    data.table::as.data.table() %>%
    .[, .(color_column, cluster_label_column, label_x, label_y),
      env = list(color_column = color_column,
                 cluster_label_column = cluster_label_column)] %>%
    unique()

  x_coordinates_column_plot <- rlang::ensym(x)
  y_coordinates_column_plot <- rlang::ensym(y)
  color_column_plot <- rlang::ensym(color_column)
  label_column_plot <- rlang::ensym(cluster_label_column)

  plot_network <- rlang::expr(ggraph::ggraph(graph, layout = "manual", x = !!x_coordinates_column_plot, y = !!y_coordinates_column_plot) +
                                ggraph::geom_edge_arc(ggplot2::aes(color = !!color_column_plot, width = weight),
                                                      alpha = 0.5,
                                                      strength = 0.2) +
                                ggraph::geom_node_point(ggplot2::aes(fill = !!color_column_plot, size = !!node_size_column_plot), pch = 21) +
                                ggplot2::scale_color_identity() +
                                ggraph::scale_edge_width_continuous(range = c(0.5, 1)) +
                                ggraph::theme_graph() +
                                ggrepel::geom_label_repel(data = labels,
                                                          ggplot2::aes(x = label_x, y = label_y, label = !!label_column_plot, fill = !!color_column_plot)) +
                                ggplot2::scale_fill_identity() +
                                ggplot2::theme(legend.position = "none") +
                                ggraph::scale_edge_colour_identity() +
                                ggplot2::labs(title = title))

  if(print_plot_code){
    cli::cli_h1("{.pkg ggplo2} code used to produce the plot")
    print(plot_network)
  }

  eval(plot_network)

}
