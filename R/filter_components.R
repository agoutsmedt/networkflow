filter_components <- function(graphs,
                              nb_components = 1,
                              threshold_alert = 0.05,
                              keep_component_columns = FALSE){
  #' Filtering Network Components
  #'
  #' @description
  #' `r lifecycle::badge("experimental")`
  #'
  #' This function is deprecated and will be replaced by `networkflow::extract_main_component()`.
  #' This function which i) creates a [tidygraph](https://tidygraph.data-imaginist.com/index.html)
  #' graph using [tbl_graph()][tidygraph::tbl_graph()];
  #' ii) keeps the main components of the graph, using [main_components()][tidygraph::group_components()]; and iii) warns
  #' the user if the first biggest component removed is too large.
  #'
  #' @inheritParams add_clusters
  #'
  #' @param nb_components
  #' By default, the function takes the main component of the graph (`nb_components = 1`).
  #' However it is possible to take as many components as you wish. The first component
  #' is the largest one, component 2 is the second one, etc.
  #'
  #' @param threshold_alert
  #' If the biggest component after the last one selected (by default, nb_component = 1)
  #' gathers more than x% (by default, 5%) of the total number of nodes,
  #' the function triggers a warning to inform the user that he has removed a big component of the network.
  #'
  #' @param keep_component_columns
  #' Set to `TRUE` if you want to store in the tibble graph the components number and
  #' the size of the components.
  #'
  #' @details
  #' The function will automatically rename the first column of nodes as "Id".
  #'
  #' @return The same tidygraph object or list of tidygraph objects with nodes
  #'
  #' @import magrittr
  #' @import tidygraph
  #' @import dplyr
  #' @export
  if(inherits(graphs, "list")){
    list <- TRUE
    graphs <- lapply(graphs, function(graph) network_level_filtering(graph,
                                                                     nb_components = nb_components,
                                                                     threshold_alert = threshold_alert,
                                                                     keep_component_columns = keep_component_columns,
                                                                     list = list))

  } else{
    if(inherits(graphs, "tbl_graph")){
      list <- FALSE
      graphs <- network_level_filtering(graphs,
                                        nb_components = nb_components,
                                        threshold_alert = threshold_alert,
                                        keep_component_columns = keep_component_columns,
                                        list = list)

    } else {
      cli::cli_abort("Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs.")
    }
  }
}


  network_level_filtering <- function(graph,
                                      nb_components = nb_components,
                                      threshold_alert = threshold_alert,
                                      keep_component_columns = keep_component_columns,
                                      list = list){
    components_att <- size_components <- . <- NULL

    # attributing a number to the different components (1 is the biggest components)
    graph <- graph %N>%
      dplyr::mutate(components_att = tidygraph::group_components(type = "weak"),
                    size_components = n()) %>%
      dplyr::group_by(components_att) %>%
      dplyr::mutate(size_components = n()/size_components) %>%
      ungroup()

    highest_component <- graph %N>%
      as.data.frame() %>%
      slice_max(order_by = components_att, n = 1, with_ties = FALSE) %>%
      .$components_att

    if(highest_component > nb_components){ #needed only if there is more components than the number of component selected
      share_component <- graph %N>%
        as.data.frame() %>%
        filter(components_att == nb_components + 1) %>%
        slice_max(order_by = size_components, n = 1, with_ties = FALSE) %>%
        .$size_components %>%
        round(4)
      if(share_component > threshold_alert){
        if(list == TRUE) cli::cli_h1("Component filtering for the {.val {graph %N>% as.data.frame() %>% .$time_window %>% unique()}} period")
        cli::cli_alert_warning("you have removed a component gathering {.val {share_component*100}}% of the nodes, more than the {.val {threshold_alert*100}}% threshold.")
      }
    }

    graph <- graph %>%
      dplyr::filter(components_att <= nb_components)

    if(keep_component_columns == FALSE) graph <- graph %>% dplyr::select(-components_att, -size_components)

    return(graph)
  }

