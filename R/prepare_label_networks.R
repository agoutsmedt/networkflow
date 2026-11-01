#' Position the labels of clusters in network and alluvial plots
#'
#' @description
#' `prepare_label_networks()` computes, for each category of
#' `cluster_label_column`, the mean coordinates of its nodes, where the label
#' of the cluster will be drawn by [plot_networks()]: `label_x` and
#' `label_y` are the centre of gravity of the cluster.
#'
#' `prepare_label_alluvial()` chooses the time window where the label of each
#' cluster of an alluvial will be drawn: the median window of the windows
#' where the cluster exists. The chosen row gets the label in a `label_x`
#' column, which [plot_alluvial()] uses.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s with coordinates
#'   (see [layout_networks()]).
#' @param alluv_dt A data frame created with [networks_to_alluv()].
#' @param x,y Character scalars, the node columns with the coordinates
#'   (`"x"` and `"y"` by default).
#' @param cluster_label_column Character scalar, the column with the labels:
#'   `"cluster_label"` ([name_clusters()]) by default, or a cluster column.
#' @param window_column Character scalar, the column of `alluv_dt` with the
#'   time windows (`"window"` by default, as created by
#'   [networks_to_alluv()]).
#'
#' @return `prepare_label_networks()`: the same object as `graphs` with node
#'   columns `label_x` and `label_y`. `prepare_label_alluvial()`: `alluv_dt`
#'   with a `label_x` column, holding the label on the rows of the chosen
#'   window and missing elsewhere.
#'
#' @examples
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
#' network <- prepare_label_networks(network, cluster_label_column = "cluster_leiden")
#' unique(tidygraph::as_tibble(network, active = "nodes")[c("cluster_leiden", "label_x", "label_y")])
#'
#' @export
prepare_label_networks <- function(graphs,
                                   x = "x",
                                   y = "y",
                                   cluster_label_column = "cluster_label") {
  single <- is_single_graph(graphs)
  if (single) graphs <- list(graphs)
  for (graph in graphs) check_columns(node_table(graph), c(x, y, cluster_label_column), "graphs")
  graphs <- lapply(graphs, network_level_label, x_coordinate = x, y_coordinate = y,
                   cluster_label_column = cluster_label_column)
  if (single) graphs <- graphs[[1]]
  graphs
}

# Centre of gravity of each cluster: the mean coordinates of its nodes.
network_level_label <- function(graph, x_coordinate, y_coordinate, cluster_label_column) {
  label_com <- node_table(graph) %>%
    dplyr::group_by(.data[[cluster_label_column]]) %>%
    dplyr::summarise(
      label_x = mean(.data[[x_coordinate]]),
      label_y = mean(.data[[y_coordinate]]),
      .groups = "drop"
    )
  graph %N>%
    dplyr::select(-dplyr::any_of(c("label_x", "label_y"))) %>%
    dplyr::left_join(label_com, by = cluster_label_column)
}

#' @rdname prepare_label_networks
#' @export
prepare_label_alluvial <- function(alluv_dt,
                                   window_column = "window",
                                   cluster_label_column = "cluster_label")
{

  . <- label_alluvial <- NULL

  label <- data.table::copy(alluv_dt)
  # We take the median window and we set the label here
  label <- label[, .N, .(cluster_label_column, window_column),
                 env = list(cluster_label_column = cluster_label_column,
                            window_column = window_column)] %>%
    .[, label_alluvial := round(stats::median(1:.N), 0) == 1:.N, .(cluster_label_column),
                 env = list(cluster_label_column = cluster_label_column,
                            window_column = window_column)] %>%
    .[label_alluvial == TRUE] %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(cluster_label_column, window_column)))) %>%
    dplyr::mutate(label_x = eval(rlang::ensym(cluster_label_column)))

  alluv_dt <- merge(alluv_dt,
                    label,
                    by=c(cluster_label_column, window_column),
                    all.x = TRUE)

  return(alluv_dt)
}

