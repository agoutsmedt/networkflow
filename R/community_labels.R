community_labels <- function(graph, biggest_community = FALSE, community_threshold = 0.01, community_name_column = "Community_name", community_size_column = "Size_com"){
  #' Calculating the Coordinates for Projecting Community Labels
  #'
  #' A simple function to calculate the mean of coordinates x and y for each community. These coordinates
  #' are to be used to plot the name of the community on the graph visualisation.
  #'
  #' @param graph
  #' A tidygraph object.
  #'
  #' @param biggest_community
  #' If true, you have the possibility to remove the smallest community, depending of the `community_threshold`
  #' you have set.
  #'
  #' @param community_threshold
  #' If `biggest_community` is true, the function selects the nodes that belong to communities which represent
  #' at least x% of the total number of nodes. By default, the parameter is set to 1%.
  #'
  #' @param community_name_column
  #' Name of the column with the name of the community to be used as label
  #'
  #' @param community_size_column
  #' Name of the column with the total number of nodes or the share of nodes in each community.
  #'
  #' @details
  #' You have to run this function only after having calculating (x,y) coordinates for your nodes.
  #'
  #' @return A data.frame with the list of your communities names (only the biggest one if `biggest_community`
  #' set to `TRUE`) and a pair of coordinates to project these names on your visualisation. It also
  #' keeps the color of the community and its size.
  #'
  #' @export
  #' @import tidygraph
  #' @import magrittr
  #' @import dplyr

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  nodes <- x <- Size_com <- y <- color <- Community_name <- NULL

  # Top nodes per community for the variable chosen
  label_com <- graph %>%
    activate(nodes) %>%
    mutate(community_name_column = .data[[community_name_column]],
           community_size_column = .data[[community_size_column]])  %>%
    as_tibble()

  # Keeping only the biggest communites if the parameter is TRUE
  if(biggest_community == TRUE){
    label_com <- label_com %>%
      filter(Size_com > community_threshold)
  }

  # Keeping the n top nodes per community
  label_com <- label_com %>%
    group_by(Community_name) %>%
    mutate(x = mean(x), y = mean(y)) %>%
    select(Community_name,x,y,color,Size_com) %>%
    as_tibble() %>%
    unique()

  return(label_com)
}
