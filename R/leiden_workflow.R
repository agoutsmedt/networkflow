# Running Leiden for two different resolutions and associating edges to communities
leiden_workflow <- function(graph, res_1 = 1, res_2 = NULL, res_3 = NULL, niter = 500){
  #' Add Leiden Communities to graph
  #'
  #' @description This function takes as input a tidygraph graph It then runs the Leiden detection community algorithm
  #' \insertCite{traag2019}{networkflow} which creates a partition. The `Leiden_workflow` functhion uses
  #' [find_partition()][leidenAlg::find_partition()] from the [leidenAlg](https://cran.r-project.org/web/packages/leidenAlg/index.html)
  #' package. The function then associates each node to its
  #' corresponding community number. It also creates a community attribute for edges: to each edge is associated
  #' a corresponding community number, if the two nodes connected by the edge belong to the same community. If nodes have
  #' a different community, the edge takes as attribute the total number of communities plus 1.
  #'
  #' @param graph A tidygraph object with a "weight" column for edges.
  #' @param res_1 The first resolution used for running the Leiden algorithm. 1 by default.
  #' @param res_2 The second resolution used for running the Leiden algorithm a second time.
  #' It adds a second community attribute to nodes and edges. By default, res_2 is null and
  #' the function just run the Leiden algorithm once (with a resolution equals to res_1).
  #' @param res_3 The third resolution used for running the Leiden algorithm a third time.
  #' @param niter Number of iterations to run the Leiden algorithm, in order to optimise
  #' the resulting partition. By default, `niter` equals 500 which warrants a quasi-optimal
  #' partitionning. Decrease n_iterations for exploratory work, in order to decrease computation
  #' time.
  #'
  #' @details The function could be run for 1, 2 or 3 different resolution values of the Leiden algorithm. It allows the
  #' user to compare different communities partitions. A low resolution means fewer communities. For instance, if
  #' the second resolution is smaller than the first one, we can observe how decreasing the resolution led some communities to disappear
  #' and to be merged with other communities. Sankey diagrams enable interesting analysis of the different partitions.
  #' @details The function also automatically calculates the percentage of total nodes that are gathered in each
  #' community, in the column `Size_com`. This calculation is only done for `Com_ID`, that is for the resolution equals to
  #' 1.
  #' @details To make plotting easier later, a zero is put before each one-digit community number (community 5 becomes 05).
  #' @details The community attribute of nodes and edges for the first resolution is called `Com_ID`.
  #' For the second and third resolution, it is called respectively `Com_ID_2` and `Com_ID_3`.
  #' @details Attributing a community number to edges enable to give edges the same color of the nodes
  #' they are connecting, if the two nodes have the same color, or a different color than any node, if
  #' the nodes belong to a different community.
  #'
  #' @return The same tidygraph graph as input, but with a new (or three) community number attribute(s) for each node and
  #' each edge.
  #'
  #' @references
  #' \insertAllCited{}
  #'
  #' @export
  #' @import magrittr
  #' @import tidygraph
  #' @import dplyr

  # Listing the variables not in the global environment to avoid a "note" saying "no visible binding for global variable ..." when using check()
  # See https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  components_att <- edges <- nodes <- Com_ID <- Size_com <- to <- from <- com_ID_from <- com_ID_to <- Com_ID_2 <- Com_ID_2_from <- Com_ID_2_to <- Com_ID_3 <- Com_ID_3_from <- Com_ID_3_to <-  NULL

  # run the leiden algorithm for the first resolution
  leiden <- leidenAlg::find_partition(graph, edge_weights = igraph::E(graph)$weight, resolution = res_1, niter = niter)

  # Add the resulting partition as an attribute of nodes
  # (to make plotting easier, put a 0 before one digit community)
  graph <- graph %>%
    activate(nodes) %>%
    mutate(Com_ID = sprintf("%02d", leiden+1)) %>%
    mutate(Com_ID = as.character(Com_ID))

  # calculate the size of the community
  graph <- graph %>%
    group_by(Com_ID) %>%
    mutate(Size_com = n()) %>%
    mutate(Size_com = Size_com / length(igraph::V(graph)$Com_ID)) %>%
    ungroup()

  # Add an attribute to edges, depending on the community of their nodes
  # (If communities are different between the two nodes, edges takes the total number of communities plus 1 as attribute)
  # (Another possibility in the future would be for edges with nodes from different communities to be the average of the
  # two communities number. It would allow the edges to take as color the mix of the two communities color)
  graph <- graph %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(com_ID_to = .N()$Com_ID[to], com_ID_from = .N()$Com_ID[from], Com_ID = ifelse(com_ID_from == com_ID_to, com_ID_from,sprintf("%02d",max(leiden)+2))) # .N() makes the node data available while manipulating edges


  # Doing the same for the second resolution
  if(is.null(res_2)){
    return(graph)
  }
  if(!is.null(res_2)){
    leiden_2 <- leidenAlg::find_partition(graph, edge_weights = igraph::E(graph)$weight, resolution = res_2, niter = niter)
    graph <- graph %>%
      activate(nodes) %>%
      mutate(Com_ID_2 = sprintf("%02d", leiden_2+1)) %>%
      mutate(Com_ID_2 = as.character(Com_ID_2))

    graph <- graph %>%
      activate(edges) %>%
      mutate(Com_ID_2_to = .N()$Com_ID_2[to], Com_ID_2_from = .N()$Com_ID_2[from], Com_ID_2 = ifelse(Com_ID_2_from == Com_ID_2_to, Com_ID_2_from,sprintf("%02d",max(leiden_2)+2)))
  }

  # Doing the same for the third resolution
  if(is.null(res_3)){
    return(graph)
  }
  if(!is.null(res_3)){
    leiden_3 <- leidenAlg::find_partition(graph, edge_weights = igraph::E(graph)$weight, resolution = res_3, niter = niter)
    graph <- graph %>%
      activate(nodes) %>%
      mutate(Com_ID_3 = sprintf("%02d", leiden_3+1)) %>%
      mutate(Com_ID_3 = as.character(Com_ID_3))

    graph <- graph %>%
      activate(edges) %>%
      mutate(Com_ID_3_to = .N()$Com_ID_3[to], Com_ID_3_from = .N()$Com_ID_3[from], Com_ID_3 = ifelse(Com_ID_3_from == Com_ID_3_to, Com_ID_3_from,sprintf("%02d",max(leiden_3)+2)))
  }
}
