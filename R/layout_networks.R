#' Compute the coordinates of nodes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A network has no natural position on a page. Force-directed layouts place
#' nodes so that tied nodes are close and untied nodes are pushed apart, so
#' that clusters appear as dense regions: Kamada-Kawai (`"kk"`) draws graph
#' distances as geometric distances \insertCite{kamada1989}{networkflow},
#' Fruchterman-Reingold (`"fr"`) balances attraction along edges and
#' repulsion between all nodes \insertCite{fruchterman1991}{networkflow}.
#' `layout_networks()` computes such coordinates with
#' [ggraph::create_layout()], which gives access to the layouts of igraph and
#' graphlayouts, and stores them in the node table.
#'
#' For a list of networks, the coordinates of a node may jump from one window
#' to the next, which makes the plots hard to compare. With
#' `compute_dynamic_coordinates = TRUE`, the coordinates of window \eqn{t-1}
#' are used as the starting positions of the layout of window \eqn{t}, so
#' that the nodes present in both windows tend to stay in the same region.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param node_id Character scalar, the node column with a unique identifier,
#'   used to join the coordinates.
#' @param layout Character scalar, a layout accepted by
#'   [ggraph::create_layout()] (e.g. `"kk"`, `"fr"`, `"stress"`).
#' @param compute_dynamic_coordinates Logical. For a list of graphs, if
#'   `TRUE`, each layout starts from the coordinates of the previous window
#'   (see the details).
#' @param save_coordinates Logical. If `TRUE`, the coordinates are also
#'   stored in `x_{layout}` and `y_{layout}` (e.g. `x_kk`, `y_kk`), so that
#'   they are not lost when another layout is computed.
#' @param ... Other arguments of the layout (see [ggraph::create_layout()]
#'   and the igraph or graphlayouts function behind it).
#'
#' @details
#' Force-directed layouts are stochastic and sensitive to the starting
#' positions: set a seed with [set.seed()] before calling the function to
#' reproduce a figure. Distances in such plots are only roughly
#' interpretable: two clusters drawn side by side are more tied than two
#' clusters at opposite ends, but distances should not be measured.
#'
#' **Weights.** igraph uses the `weight` edge column automatically, but not
#' always in the same way: Fruchterman-Reingold (`"fr"`) reads weights as
#' attraction (heavier edges pull nodes closer), while Kamada-Kawai (`"kk"`)
#' reads them as distances, so that similarity weights push the most similar
#' nodes apart and weakly tied nodes to the margins. With similarity weights,
#' prefer `"fr"`, or pass `weights = NA` to ignore them.
#'
#' For dynamic coordinates, nodes absent from the previous window start at
#' (0, 0). The starting positions are passed to the layout as its `coords`
#' argument: this works only with layouts that accept it, such as `"fr"` and
#' `"kk"` of igraph.
#'
#' @return The same object as `graphs`, with node columns `x` and `y` (any
#'   existing `x` and `y` are replaced) and, if `save_coordinates = TRUE`,
#'   `x_{layout}` and `y_{layout}`.
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 2
#' )
#' set.seed(1)
#' network <- layout_networks(network, node_id = "source_id", layout = "fr")
#' head(tidygraph::as_tibble(network, active = "nodes")[c("source_id", "x", "y")])
#'
#' @export
layout_networks <- function(graphs,
                            node_id,
                            layout,
                            compute_dynamic_coordinates = FALSE,
                            save_coordinates = FALSE,
                            ...) {
  if (is_single_graph(graphs)) {
    check_columns(node_table(graphs), node_id, "graphs")
    return(join_coordinates(graphs, node_id = node_id, layout = layout,
                            save_coordinates = save_coordinates, ...))
  }
  for (graph in graphs) check_columns(node_table(graph), node_id, "graphs")
  if (compute_dynamic_coordinates && length(graphs) > 1) {
    return(compute_dynamic_coordinates(graphs, node_id = node_id, layout = layout,
                                       save_coordinates = save_coordinates, ...))
  }
  lapply(graphs, join_coordinates, node_id = node_id, layout = layout,
         save_coordinates = save_coordinates, ...)
}

compute_dynamic_coordinates <- function(graphs,
                                        node_id = node_id,
                                        layout = layout,
                                        save_coordinates = save_coordinates,
                                        ...)
{
  x <- y <- NULL
  graphs[[1]] <- join_coordinates(graphs[[1]],
                                  node_id = node_id,
                                  layout = layout,
                                  save_coordinates = save_coordinates,
                                  ...)

  for (i in 2:length(graphs)){
    past_coords <- graphs[[i-1]] %N>%
      dplyr::as_tibble() %>%
      dplyr::select(dplyr::all_of(node_id), x, y)

    graphs[[i]] <- graphs[[i]] %N>%
      dplyr::select(-dplyr::any_of(c("x", "y"))) %>%
      dplyr::left_join(past_coords, by = node_id)

    input_coords <- graphs[[i]] %N>%
      dplyr::as_tibble() %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(c("x","y")), ~tidyr::replace_na(., 0))) %>%
      dplyr::select(x, y) %>%
      as.matrix

    graphs[[i]] <- graphs[[i]] %N>%
      dplyr::select(-x, -y)

    coords <- ggraph::create_layout(graphs[[i]],
                                    layout = layout,
                                    coords = input_coords,
                                    ...) %>%
      dplyr::select(dplyr::all_of(node_id), x, y)

    graphs[[i]] <- graphs[[i]] %N>%
      dplyr::left_join(coords, by = node_id)

    if(save_coordinates == TRUE){
      graphs[[i]] <- graphs[[i]] %N>%
        dplyr::mutate("x_{layout}" := x,
                      "y_{layout}" := y)
    }
  }
  return(graphs)
}

join_coordinates <- function(graphs,
                             node_id,
                             layout,
                             save_coordinates = FALSE,
                             ...) {
  # Old coordinates are dropped first: ggraph would otherwise warn that it
  # overwrites them. create_layout() returns the nodes in the order of the
  # graph, so coordinates are assigned by position.
  graphs <- graphs %N>% dplyr::select(-dplyr::any_of(c("x", "y")))
  coords <- ggraph::create_layout(graphs, layout = layout, ...)
  graphs <- graphs %N>% dplyr::mutate(x = coords$x, y = coords$y)
  if (save_coordinates) {
    graphs <- graphs %N>%
      dplyr::mutate(!!paste0("x_", layout) := .data$x, !!paste0("y_", layout) := .data$y)
  }
  graphs
}

