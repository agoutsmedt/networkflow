tbl_main_component  <- function(edges, nodes, nb_components = 1, threshold_alert = 0.05, ...){
  #' Creating Graph And Keeping Main Component With Tidygraph From Edges And Nodes
  #'
  #' A function which i) creates a [tidygraph](https://tidygraph.data-imaginist.com/index.html) graph using [tbl_graph()][tidygraph::tbl_graph()];
  #' ii) keeps the main components of the graph, using [main_components()][tidygraph::group_components()]; and iii) warns
  #' the user if the first biggest component removed is too large.
  #'
  #' @param edges
  #' A dataframe with a list of links between nodes, under the columns "from" and "to". The two columns should
  #' be in character format.
  #'
  #' @param nodes
  #' A dataframe with a list of nodes. The first column will be used as the identifying column.
  #' Be careful to avoid doublons in the first column. The first column should be in character format.
  #'
  #' @param ...
  #' Any parameter of the [tbl_graph()][tidygraph::tbl_graph()] function. Edges are directed by default.
  #' Coupling or co-citation network are *undirected* networks.
  #'
  #' @param nb_components
  #' By default, the function takes the main component of the graph (nb_components = 1). However it is possible to take as
  #' many components as you wish Component 1 is the largest one, component 2 is the second one, etc.
  #'
  #' @param threshold_alert
  #' If the biggest component after the last one selection (by default, nb_component = 1) gathers more than x% (by default, 5%) of the total number of nodes,
  #' the function triggers a warning to inform the user that he has removed a big component of the network.
  #'
  #' @details
  #' The function will automatically rename the first column of nodes as "Id".
  #'
  #' @return A tidygraph object with a list of nodes and a list of edges.
  #'
  #' @import magrittr
  #' @export

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  components_att <- NULL

  # creating the tidygraph object
  graph <- tidygraph::tbl_graph(nodes, edges, ...)

    # attributing a number to the different components (1 is the biggest components)
  graph <- graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(components_att = tidygraph::group_components(type = "weak")) %>%
    dplyr::rename_at( 1, ~"Id" ) # renamed the first column to a standard format

  # looking at the biggest component just after the last one we have kept
  threshold <- graph %>%
    dplyr::filter(components_att == nb_components+1) %>%
    dplyr::rename_at( 1, ~"Id" )

  # looking at the percentage of nodes in the biggest component just after the last one we have kept
  # trigger a warning if superior to the threshold_alert
  if(length(igraph::V(threshold)$Id)/length(igraph::V(graph)$Id) > threshold_alert)warning(paste0("Warning: you have removed a component gathering more than ",threshold_alert,"% of the nodes"))

  # keeping only the number of components we want
  graph <- graph %>%
    dplyr::filter(components_att<= nb_components) %>%
    dplyr::select(-components_att) # we remove the column as it won't be useful after that
}
