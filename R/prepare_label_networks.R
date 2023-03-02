
#' Creating and Positioning Labels for Plotting Networks or Alluvial
#'
#' @description
#' `prepare_label_networks()` creates a label column that make it easy to center label around particular clusters on the alluvial graph of intertemporal networks.
#' `prepare_label_alluvial()`
#'
#' @param graphs
#' A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/) or a list of tibble
#' graphs.
#'
#' @param alluv_dt
#' A data.frame of an alluvial created with [networks_to_alluv()][networkflow::networks_to_alluv()].
#'
#' @param x,y
#' The columns of your tibble graph or list of tibble graphs
#' with the x and y coordinates of your nodes (for instance, the coordinates
#' produce by [layout_networks()][networkflow::layout_networks()]).
#'
#' @param cluster_label_column
#' The column with the cluster label you want to display. By default, "cluster_label", as it is
#' the default name of the column created with [name_clusters()][networkflow::name_clusters()].
#' You may also use the cluster identifier created by [add_clusters()][networkflow::add_clusters()]
#' for simple cluster, or by [merge_dynamic_clusters()][networkflow::merge_dynamic_clusters()] for
#' inter-temporal clusters.
#'
#' @param window_column
#' The column of the alluvial with your time window. By default, "window", as created by
#' [networks_to_alluv()][networkflow::networks_to_alluv()].
#'
#'
#' @export
#'

prepare_label_networks <- function(graphs,
                                   x = "x",
                                   y = "y",
                                   cluster_label_column = "cluster_label")
{
  . <- nodes <- label_x <- x <- label_y <- y <- label_column <- NULL

  if(inherits(graphs, "list")){
    graphs <- lapply(graphs,
                     network_level_label,
                     x_coordinate = x,
                     y_coordinate = y,
                     cluster_label_column = cluster_label_column)

  } else{
    if(inherits(graphs, "tbl_graph")){
      graphs <- network_level_label(graphs,
                                    cluster_label_column = cluster_label_column)
    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }
  return(graphs)
}

network_level_label <- function(graph,
                                x_coordinate = x,
                                y_coordinate = y,
                                cluster_label_column = cluster_label_column){
  x <- y <- NULL

  label_com <- graph %N>%
    data.table::as.data.table() %>%
    dplyr::group_by(dplyr::across({{cluster_label_column}})) %>%
    dplyr::summarise(label_x = mean(eval(rlang::ensym(x_coordinate))),
                     label_y = mean(eval(rlang::ensym(y_coordinate))))

  graph <- graph %N>%
    dplyr::left_join(label_com, by = cluster_label_column)
}

#' @rdname prepare_label_networks
#' @export
prepare_label_alluvial <- function(alluv_dt,
                                   window_column = "window",
                                   cluster_label_column = "cluster_label")
{

  . <- NULL

  label <- data.table::copy(alluv_dt)
  # We take the median window and we set the label here
  label <- label[, label_alluvial := round(mean(1:.N), 0) == (1:.N), .(cluster_label_column),
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

