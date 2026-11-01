#' Plot networks
#'
#' @description
#' `plot_networks()` draws a network from the columns created by the
#' previous steps of the pipeline: coordinates ([layout_networks()]),
#' colours ([color_networks()]) and cluster labels ([name_clusters()] and
#' [prepare_label_networks()]). Edges are drawn as light arcs whose width
#' follows their weight, nodes as points whose size follows
#' `node_size_column`, and each cluster gets a label at its centre of
#' gravity.
#'
#' It is meant as a quick, ready-made figure. For exploration, the
#' interactive app of [launch_network_app()] is more convenient; for a
#' figure to publish, `print_plot_code = TRUE` prints the ggraph code, which
#' can be copied and adapted.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param x,y Character scalars, the node columns with the coordinates
#'   (`"x"` and `"y"` by default).
#' @param cluster_label_column Character scalar, the node column with the
#'   labels to display: `"cluster_label"` ([name_clusters()]) by default, or
#'   a cluster column such as `cluster_leiden`.
#' @param node_size_column Character scalar, the numeric node column giving
#'   the size of nodes: `"node_size"` by default (see `compute_size` in
#'   [build_network()]). If `NULL` or absent, all nodes have the same size.
#' @param color_column Character scalar, the column with the colours of
#'   nodes and edges (`"color"` by default, from [color_networks()]).
#' @param color_networks Logical. If `TRUE`, [color_networks()] is run first,
#'   with `cluster_label_column` as the categories.
#' @param color With `color_networks = TRUE`, the colours passed to
#'   [color_networks()].
#' @param print_plot_code Logical. If `TRUE`, prints the ggplot2/ggraph code
#'   of the plot.
#'
#' @details
#' Missing pieces are filled in: labels are positioned with
#' [prepare_label_networks()] when `label_x` is absent, and edges without a
#' `weight` column get a weight of 1. Labels are drawn with ggrepel, which
#' must be installed.
#'
#' @return For a `tbl_graph`, a ggplot object. For a list of graphs, the
#'   list itself, in which each graph carries its plot in an element `plot`
#'   (`graphs[[1]]$plot`).
#'
#' @examples
#' \donttest{
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1
#' )
#' network <- add_clusters(
#'   network,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#' set.seed(1)
#' network <- layout_networks(network, node_id = "source_id", layout = "fr")
#' network <- color_networks(network, column_to_color = "cluster_leiden")
#' if (requireNamespace("ggrepel", quietly = TRUE)) {
#'   plot_networks(
#'     network,
#'     cluster_label_column = "cluster_leiden",
#'     node_size_column = NULL
#'   )
#' }
#' }
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
                          print_plot_code = FALSE) {
  single <- is_single_graph(graphs)
  graph_list <- if (single) list(graphs) else graphs
  first_nodes <- node_table(graph_list[[1]])
  if (is.null(cluster_label_column)) {
    cli::cli_abort("Give the column of the labels to display in {.arg cluster_label_column}.")
  }
  check_columns(first_nodes, c(x, y, cluster_label_column), "graphs")
  if (!color_column %in% names(first_nodes) && !color_networks) {
    cli::cli_abort(c(
      "The graphs have no {.field {color_column}} column.",
      "i" = "Run {.fn color_networks} first, or set {.code color_networks = TRUE}."
    ))
  }

  if (color_networks) {
    graph_list <- color_networks(graph_list, column_to_color = cluster_label_column,
                                 color = color, verbose = FALSE)
    color_column <- "color"
  }
  if (!"label_x" %in% names(first_nodes)) {
    graph_list <- prepare_label_networks(graph_list, x = x, y = y,
                                         cluster_label_column = cluster_label_column)
  }
  titles <- if (single) "" else names(graph_list)
  if (is.null(titles)) titles <- rep("", length(graph_list))

  if (print_plot_code) {
    plot_network(graph_list[[1]], x = x, y = y, title = titles[1],
                 color_column = color_column, cluster_label_column = cluster_label_column,
                 node_size_column = node_size_column, print_plot_code = TRUE)
  }
  plots <- lapply(seq_along(graph_list), function(i) {
    plot_network(graph_list[[i]], x = x, y = y, title = titles[i],
                 color_column = color_column, cluster_label_column = cluster_label_column,
                 node_size_column = node_size_column, print_plot_code = FALSE)
  })
  if (single) {
    return(plots[[1]])
  }
  for (i in seq_along(graph_list)) {
    graph_list[[i]]$plot <- plots[[i]]
  }
  cli::cli_inform(c("i" = "Each graph of the list carries its plot: {.code graphs[[1]]$plot}."))
  graph_list
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

  if (!"weight" %in% names(tidygraph::as_tibble(graph, active = "edges"))) {
    graph <- graph %E>% dplyr::mutate(weight = 1)
  }
  if (is.null(node_size_column) || !node_size_column %in% colnames(graph %N>% as.data.frame())) {
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

  if (print_plot_code) {
    cli::cli_h2("ggplot2 code of the plot")
    cli::cli_code(deparse(plot_network, width.cutoff = 80))
  }

  eval(plot_network)

}
