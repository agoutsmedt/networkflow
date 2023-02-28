community_names <- function(graph, ordering_column, naming = "Label", community_column = "Com_ID"){
  #' Automatically Attributing Names to Communities
  #'
  #' @description
  #'
  #' `r lifecycle::badge("deprecated")`
  #'
  #' A function to give to a community the name of its node with the highest chosen measure.
  #' It also gives the edges the name of their community. If the edge connects nodes from
  #' different community, name will be NA.
  #'
  #' @description The function takes into account the parameters chosen for the `leiden_improved()` function.
  #' If you have chosen 2 or 3 levels of resolution, it repeats the same process for the second and third resolution.
  #' In other words, for 3 levels of resolution, you will have the names of the communities for the first value of the
  #' Leiden resolution, but also the names for the second and third values.
  #'
  #'
  #' @param graph A tidygraph object.
  #' @param ordering_column Enter the name of the column you want to be used to choose the community name. For instance, if
  #' you choose `Degree`, the function takes the value of the `naming` column of the node with the highest
  #' degree in the community to name the community. You can use other measure
  #' than network centrality measures: for instance, if nodes are articles, you can use the number of citations of articles.
  #'
  #' @param naming Enter the name of the column you want to be used for naming the community. The function takes the node
  #' with the highest `centrality_measure` chosen, and use the node value in the `naming` column  to title the community.
  #' For instance, if nodes are individuals and if you have a column called `surname`, you can use this column.
  #'
  #' @param community_column
  #' The name of your community identifiers column.
  #'
  #' @details The attribute of nodes and edges with the names of the communities is called
  #' `Community_name`. It is formed by the identifier of the community (in the `Com_ID` column)
  #' and by the `naming` value.
  #'
  #' @details If you have entered a second and a third resolutions values in the
  #' `leiden_improved()` function, you will have two supplementary columns: `Community_2_name`
  #' and `Community_3_name`.
  #'
  #' @return The same graph object but with a column `Community_name`, as well as `Community_2_name`
  #' and `Community_3_name` if you have run the [leiden_workflow()] function for more than one resolution.
  #'
  #' @export

  lifecycle::deprecate_warn("0.1.0", "community_names()", "name_clusters()")

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  nodes <- Com_ID <- Label <- Community_name <- Community_2_name <- Community_3_name <- V <- Com_ID_2 <- Com_ID_3 <- NULL

  # Finding the nodes with the highest strength per community and building a df with community numbers
  # and the label of the node with the highest Strength.

  graph <- graph %N>%
    dplyr::rename(Label = {{naming}},
           Com_ID = {{community_column}}) %>%
    dplyr::mutate(ordering_column = eval(rlang::ensym(ordering_column)))

  Community_names <- graph %N>%
    dplyr::as_tibble() %>%
    dplyr::arrange(Com_ID, dplyr::desc(ordering_column)) %>%
    dplyr::mutate(Community_name = Label) %>%
    dplyr::select(Community_name, Com_ID) %>%
    dplyr::group_by(Com_ID) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(Community_name = paste0(Com_ID,"-",Community_name))

  # adding the name as an attribute to the nodes.
  graph <- graph %N>%
    dplyr::inner_join(Community_names, by = "Com_ID")

  # Reproducing the same operation for Com_ID_2 if it exists
  if(!is.null(igraph::V(graph)$Com_ID_2)){
    Community_names <- graph %N>%
      dplyr::as_tibble()  %>%
      dplyr::arrange(Com_ID, dplyr::desc(ordering_column)) %>%
      dplyr::mutate(Community_2_name = Label) %>%
      dplyr::select(Community_2_name, Com_ID_2) %>%
      dplyr::group_by(Com_ID_2) %>%
      dplyr::slice(1) %>%
      dplyr::mutate(Community_2_name = paste0(Com_ID_2,"-",Community_2_name))

    graph <- graph %N>%
      dplyr::inner_join(Community_names, by = "Com_ID_2")

 #   graph <- graph %>%
  #    activate(edges) %>%
   #   left_join(Community_names, by = "Com_ID_2")
  }

  # Reproducing the same operation for Com_ID_3 if it exists
  if(!is.null(igraph::V(graph)$Com_ID_3)){
    Community_names <- graph %N>%
      dplyr::as_tibble()  %>%
      dplyr::arrange(Com_ID, dplyr::desc(ordering_column)) %>%
      dplyr::mutate(Community_3_name = Label) %>%
      dplyr::select(Community_3_name, Com_ID_3) %>%
      dplyr::group_by(Com_ID_3) %>%
      dplyr::slice(1) %>%
      dplyr::mutate(Community_3_name = paste0(Com_ID_3,"-",Community_3_name))

    graph <- graph %N>%
      dplyr::inner_join(Community_names, by = "Com_ID_3")

  #  graph <- graph %>%
   #   activate(edges) %>%
    #  left_join(Community_names, by = "Com_ID_3")
  }

  graph <- graph %>%
    dplyr::select(-ordering_column)

  return(graph)
}

