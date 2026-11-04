#' Keep the main component(s) of networks
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A projected network is often made of one large connected component and of
#' small islands: a few articles that share references only with each other,
#' or isolated pairs. Community detection and layouts are hard to read on such
#' fragments, and many studies keep only the main component.
#' `filter_components()` numbers the (weakly) connected components by
#' decreasing size, keeps the `nb_components` largest, and warns when the
#' first component removed is not negligible.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s (e.g. the output of
#'   [build_dynamic_networks()]).
#' @param nb_components Integer, the number of components to keep, starting
#'   from the largest (1 by default).
#' @param threshold_alert Number between 0 and 1. If the largest removed
#'   component gathers more than this share of the nodes (0.05 by default), a
#'   warning is given: removing it would change the object of study.
#' @param keep_component_columns Logical. If `TRUE`, keeps the columns
#'   `components_att` (component rank, 1 being the largest) and
#'   `size_components` (share of the nodes in the component).
#'
#' @details
#' Dropping small components is a choice, not a technicality: the removed
#' documents may be a marginal literature, or a literature that cites
#' sources absent from the data. Look at what is removed before discarding
#' it, especially when the warning is triggered. For a list of graphs, the
#' filter is applied to each graph separately, so a node may be kept in one
#' window and removed in another.
#'
#' @return The same object as `graphs` (a `tbl_graph` or a list of
#'   `tbl_graph`s) restricted to the nodes of the kept components.
#'
#' @seealso [build_network()] and [build_dynamic_networks()], which can call
#'   this function with `filter_components = TRUE`.
#'
#' @examples
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   method = "similarity",
#'   edges_threshold = 2
#' )
#' main_component <- filter_components(network, nb_components = 1)
#' c(before = igraph::vcount(network), after = igraph::vcount(main_component))
#'
#' @export
filter_components <- function(graphs,
                              nb_components = 1,
                              threshold_alert = 0.05,
                              keep_component_columns = FALSE) {
  if (is_single_graph(graphs)) {
    return(network_level_filtering(graphs, nb_components, threshold_alert, keep_component_columns))
  }
  lapply(graphs, network_level_filtering,
         nb_components = nb_components,
         threshold_alert = threshold_alert,
         keep_component_columns = keep_component_columns)
}

network_level_filtering <- function(graph,
                                    nb_components,
                                    threshold_alert,
                                    keep_component_columns) {
  components_att <- size_components <- NULL

  # An empty network (a window without any edge) has no component.
  if (igraph::vcount(graph) == 0) {
    return(graph)
  }

  # Components numbered by decreasing size (1 is the largest), and the share
  # of the nodes of the graph in each.
  graph <- graph %N>%
    dplyr::mutate(components_att = tidygraph::group_components(type = "weak")) %>%
    dplyr::mutate(size_components = as.vector(table(components_att)[components_att]) / dplyr::n())

  nodes <- node_table(graph)
  if (max(nodes$components_att) > nb_components) {
    share_removed <- max(nodes$size_components[nodes$components_att == nb_components + 1])
    if (share_removed > threshold_alert) {
      window <- if ("time_window" %in% names(nodes)) unique(nodes$time_window)[1] else NULL
      cli::cli_warn(c(
        "A removed component gathers {round(100 * share_removed, 1)}% of the nodes{if (!is.null(window)) paste0(' (window ', window, ')')}.",
        "i" = "It is more than {.arg threshold_alert} ({100 * threshold_alert}%): check what is removed, or increase {.arg nb_components}."
      ))
    }
  }

  graph <- dplyr::filter(graph, components_att <= nb_components)
  if (!keep_component_columns) {
    graph <- dplyr::select(graph, -components_att, -size_components)
  }
  graph
}
