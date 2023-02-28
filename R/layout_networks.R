#' Calculate the Coordinates of Nodes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function implements different layout algorithms from
#' [ggraph](https://ggraph.data-imaginist.com/index.html) and
#' [graphlayouts](http://graphlayouts.schochastics.net/index.html). It also implements
#' a dynamic process of computation of coordinates for list of tibble graphs, in order
#' for the coordinates to be consistent between graphs.
#'
#' @inheritParams add_clusters
#'
#' @param node_id
#' The column with the unique identifier of each node.
#'
#' @param layout
#' The type of layout to create. All strings accepted by the algorithm argument can
#' also be supplied directly into layout. See `ggraph::ggraph()` for more details on
#' layouts.
#'
#' @param compute_dynamic_coordinates
#' When you have a list of tibble graphs and that some nodes are in multiple graphs,
#' you may want that each node in several graphs has relatively similar coordinates. Set
#' `compute_dynamic_coordinates` to `TRUE` to take into account the coordinates of the nodes in
#' the `n-1` tibble graph, during the computation of the coordinates of the `n` tibble graph.
#'
#' @param save_coordinates
#' If you are running the function on the same object, the already existing columns `x` and `y`
#' will be erased and new values will be computed. Set `save_coordinates` to `TRUE` to
#' save the `x` and `y` values in columns with a name depending of the layout method used.
#'
#' @param ...
#' Additional arguments of the layout compatible with the function. See `ggraph::graph()`,
#' `ggraph::layout_tbl_graph_igraph()` and `graphlayouts::graphslayouts()` for more information.
#'
#' @details
#' The function allows for the use of different layouts implemented in
#' [ggraph](https://ggraph.data-imaginist.com/index.html) and
#' [graphlayouts](http://graphlayouts.schochastics.net/index.html). However, some
#' layout of `graphlayouts` are not working with this function. Also, layouts do not
#' all take as input pre-determined coordinates, meaning that the
#' `compute_dynamic_coordinates` argument does not work with all layout. Please check
#' if the layout used allows a parameter called `coord`.
#'
#' @return
#' The same tibble graph or list of tibble graphs with a column `x` and `y`, and also
#' additional column `x_{layout}` and `y_{layout}` if you have set
#' `save_coordinates` to `TRUE`.
#'
#' @examples
#' library(networkflow)
#'
#' nodes <- Nodes_stagflation |>
#' dplyr::rename(ID_Art = ItemID_Ref) |>
#' dplyr::filter(Type == "Stagflation")
#'
#' references <- Ref_stagflation |>
#' dplyr::rename(ID_Art = Citing_ItemID_Ref)
#'
#' temporal_networks <- build_dynamic_networks(nodes = nodes,
#' directed_edges = references,
#' source_id = "ID_Art",
#' target_id = "ItemID_Ref",
#' time_variable = "Year",
#' cooccurrence_method = "coupling_similarity",
#' time_window = 20,
#' edges_threshold = 1,
#' overlapping_window = TRUE,
#' filter_components = TRUE)
#'
#' temporal_networks <- layout_networks(temporal_networks,
#' node_id = "ID_Art",
#' layout = "fr",
#' compute_dynamic_coordinates = TRUE)
#'
#' temporal_networks [[1]]
#'
#' @export

layout_networks <- function(graphs,
                            node_id,
                            layout,
                            compute_dynamic_coordinates = FALSE,
                            save_coordinates = FALSE,
                            ...){
  if(inherits(graphs, "tbl_graph")){
    graphs <- join_coordinates(graphs,
                               node_id = node_id,
                               layout = layout,
                               save_coordinates = save_coordinates,
                               ...)

  } else if(inherits(graphs, "list")){
    if(compute_dynamic_coordinates == FALSE){
      graphs <- lapply(graphs,
                       function(tbl) join_coordinates(tbl,
                                                      node_id = node_id,
                                                      layout = layout,
                                                      save_coordinates = save_coordinates,
                                                      ...))
    } else {
      graphs <- compute_dynamic_coordinates(graphs,
                                            node_id = node_id,
                                            layout = layout,
                                            save_coordinates = save_coordinates,
                                            ...)
    }
  } else {
    cli::cli_abort(c("The data object you enter in the function is neither",
                     "*" = "a tibble graph (a {.emph tbl_graph} from {.pkg tidygraph});",
                     "*" = "a list of tibble graphs."))
  }
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
                             save_coordinates = save_coordinates,
                             ...){
  x <- y <- NULL
  coords <- ggraph::create_layout(graphs,
                                  layout = layout,
                                  ...) %>%
    dplyr::select(dplyr::all_of(node_id), x, y)

  graphs <- graphs %N>%
    dplyr::select(-dplyr::any_of(c("x", "y"))) %>%
    dplyr::left_join(coords, by = node_id)

  if(save_coordinates == TRUE){
    graphs <- graphs %N>%
      dplyr::mutate("x_{layout}" := x,
                    "y_{layout}" := y)
  }
  return(graphs)
}
