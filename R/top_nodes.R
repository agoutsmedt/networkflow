top_nodes <- function(graph, ordering_column, top_n = 20, top_n_per_com = 1, biggest_community = FALSE, community_threshold = 0.01){
  #' Displaying the Highest Cited Nodes
  #'
  #' A simple function for keeping a number n of nodes with the highest chosen statistics value
  #' per communities and a number n of nodes with the highest chosen statistics value within
  #' the whole network. This is used to display only the most important nodes on your graph visualisation.
  #'
  #' @param graph
  #' A tidygraph object.
  #'
  #' @param ordering_column
  #' The name of the column with numeric values you want to use to select the most important nodes of your network.
  #' For instance, the column gathering the number of citations of each article of your dataframe.
  #'
  #' @param top_n
  #' The number of nodes with the highest chosen statistics value. For instance the number of highest cited nodes
  #' you want to display on the graph.
  #'
  #' @param top_n_per_com
  #' The number of highest cited nodes per community to display.
  #'
  #' @param biggest_community
  #' If true, you have the possibility to remove the smallest community, depending of the `community_threshold`
  #' you have set.
  #'
  #' @param community_threshold
  #' If `biggest_community` is true, the function only selects the nodes that belong to communities which represent
  #' at least x% of the total number of nodes. By default, the parameter is set to 1%.
  #'
  #' @return A data.table with a number n of nodes with the highest chosen statistics value
  #' per communities and a number n of nodes with the highest chosen statistics value within
  #' the whole network. Informations like coordinates (x,y) and community identifier are kept
  #' for using the data.table in a visualisation.
  #'
  #' @export
  #' @import tidygraph
  #' @import magrittr
  #' @import dplyr
  #' @import data.table

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  nodes <- Com_ID <- Size_com <- NULL

  # Top nodes per community for the variable chosen
  top_variable_com <- graph %>%
    activate(nodes) %>%
    mutate(ordering_column = .data[[ordering_column]]) %>%
    as_tibble()

  # Keeping only the biggest communites if the parameter is TRUE
  if(biggest_community == TRUE){
    top_variable_com <- top_variable_com %>%
      filter(Size_com > community_threshold)
  }

  # Keeping the n top nodes per community
  top_variable_com <- top_variable_com %>%
    group_by(Com_ID) %>%
    slice_max(order_by = ordering_column, n = top_n_per_com) %>%
    as.data.table()

  # Top nodes in general for the chosen variable
  top_variable_general <- graph %>%
    activate(nodes) %>%
    mutate(ordering_column = .data[[ordering_column]]) %>%
    as_tibble()

  top_variable_general <- top_variable_general %>%
    slice_max(order_by = ordering_column, n = top_n) %>%
    as.data.table()

  # adding the two and removing the doublons
  top_variable_sum <- unique(rbind(top_variable_general, top_variable_com))
  top_variable_sum <- top_variable_sum[,-"ordering_column"]


  return(top_variable_sum)
}
