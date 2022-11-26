add_clusters <- function(graphs,
                         clustering_method = c("leiden", "louvain", "fast_greedy", "infomap", "walktrap"),
                         objective_function = c("modularity", "CPM"), #leiden
                         resolution = 1, #leiden
                         n_iterations = 1000, #leiden
                         n_groups = NULL, #fast_greedy & walktrap
                         node_weights = NULL, #infomap & Leiden
                         trials = 10, #infomap
                         steps = 4, #walktrap
                         verbose = TRUE
                         ){
  #' Detect and Add Clusters to Graphs
  #'
  #' @description
  #' `r lifecycle::badge("experimental")`
  #'
  #' This function takes as input a tidygraph graph and then runs different
  #' cluster detection algorithm depending on the method chosen by the user (see @details for
  #' information on the different methods. The function
  #' associate each node to its corresponding cluster identifier. It also creates a cluster
  #' attribute for edges: to each edge is associated a corresponding cluster
  #' identifier if  the two nodes connected by the edge belong to the same
  #' cluster If nodes have a different cluster, the edge takes "00" as cluster attribute.
  #'
  #' @param graphs A tidygraph object or a list of tidygraph objects with a "weight" column
  #' for edges.
  #'
  #' @param clustering_method The different clustering algorithms implemented in the
  #' function (see details). The parameters of the function depend of the clustering method chosen.
  #'
  #' @param objective_function The objective function to maximize for the leiden algorithm.
  #' Whether to use the Constant Potts Model (CPM) or modularity. Must be either "CPM"
  #' or "modularity" (see `igraph::cluster_leiden()`). CPM is used by default.
  #'
  #' @param resolution The resolution parameter to use for leiden algorithm
  #' (see `igraph::cluster_leiden()`). Higher resolutions lead to more
  #' smaller communities, while lower resolutions lead to fewer larger communities.
  #'
  #' @param n_iterations the number of iterations to iterate the Leiden algorithm.
  #' Each iteration may improve the partition further (see `igraph::cluster_leiden()`).
  #'
  #' @param node_weights May be used both for the Leiden or infomap algorithms.
  #' For Leiden, if this is not provided, it will be automatically determined on the
  #' basis of the objective_function (see `igraph::cluster_leiden()`). For infomap,
  #' if it is not present, then all vertices are considered to have the same weight.
  #' A larger vertex weight means a larger probability that the random surfer jumps
  #' to that vertex (see `igraph::cluster_infomap()`).
  #'
  #' @param trials The number of attempts to partition the network
  #' (can be any integer value equal or larger than 1) for the infomap algorithm
  #' (see `igraph::cluster_infomap()`).
  #'
  #' @param n_groups May be used by the fast greedy or the walktrap algorithm.
  #' Integer scalar, the desired number of communities. If too low or two high,
  #' then an error message is given.
  #'
  #' @param steps The length of the random walks to perform for the walktrap algorithm
  #' (see `igraph::cluster_walktrap()`)
  #'
  #' @param verbose
  #' Set to `FALSE` if you don't want the function to display different sort of information.
  #'
  #' @details The function could be run indifferently on one tidigraph object or on a list
  #' of tidygraph object, as created by `build_dynamic_networks()`.
  #' @details The function implements five different algorithms. Four exists in
  #' [igraph](https://igraph.org/r/) and are used in this package through their implement
  #' in [tidygraph](https://tidygraph.data-imaginist.com/) (see
  #' [group_graph()][tidygraph::group_graph()]). The function also implements the
  #' Leiden algorithm \insertCite{traag2019}{networkflow} which is in `igraph` but not
  #' in `tidygraph` yet (see [cluster_leiden()][igraph::cluster_leiden()]).
  #' @details The newly created columns with the cluster identifier for nodes and edges
  #' are named depending of the method used. If you use the Leiden algorithm, the
  #' function will create a column called `cluster_leiden` for nodes, and three columns
  #' for the edges, called `cluster_leiden_from`, `cluster_leiden_to` and `cluster_leiden`.
  #' @details The function also
  #' automatically calculates the percentage of total nodes that are gathered in each
  #' cluster, in the column `size_com`.
  #' @details To make plotting easier later, a zero is put before one-digit cluster identifier
  #' (cluster 5 becomes "05"; cluster 10 becomes "10"). Attributing a cluster identifier to edges
  #' allow for giving edges the same color of the nodes they are connecting together if the two nodes have the same color,
  #' or a different color from both nodes, if the nodes belong to different clusters.
  #'
  #' @return The same tidygraph graph or tidygraph list as input, but with a new cluster
  #' column for nodes with a column with the size of these clusters,
  #' and three cluster columns for edges (see the details).
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
  #' time_window = 10,
  #' edges_threshold = 1,
  #' overlapping_window = TRUE,
  #' filter_components = TRUE)
  #'
  #' temporal_networks <- add_clusters(temporal_networks,
  #' objective_function = "modularity",
  #' clustering_method = "leiden")
  #'
  #' temporal_networks[[1]]
  #'
  #'
  #' @references
  #' \insertAllCited{}
  #'
  #' @export
  #' @import magrittr
  #' @import tidygraph
  #' @import dplyr
  #' @import igraph
  #' @import lifecycle
  #' @import cli
  #' @importFrom Rdpack reprompt
  #'

  if(length(clustering_method) > 1){
    cli::cli_abort(c("You did not choose any clustering method! You have the choice between: ",
                   "*" = "\"leiden\";",
                   "*" = "\"louvain\";",
                   "*" = "\"fast_greedy\";",
                   "*" = "\"infomap\";",
                   "*" = "\"walktrap\"."))
  }
  if(! clustering_method %in% c("leiden", "louvain", "fast_greedy", "infomap", "walktrap")){
    cli::cli_abort("The method you have chosen is not implemented within the function.")
  }
  if(length(objective_function) > 1 & clustering_method == "leiden"){
    cli::cli_abort(c("You did not choose any objective function for the Leiden algorithm. You have the choice between: ",
                   "*" = "\"CPM\";",
                   "*" = "\"modularity\"."))
  if(clustering_method %in% c("leiden", "louvain", "fast_greedy", "infomap", "walktrap")){
  cli::cli_alert_info("You are using the {.emph {.strong {clustering_method}}} clustering method.")
  }

  }
  if(inherits(graphs, "list")){
    list <- TRUE
    cluster_list_graph <- lapply(graphs, function(graph) detect_cluster(graph,
                                                                        clustering_method = clustering_method,
                                                                        objective_function = objective_function,
                                                                        resolution = resolution,
                                                                        n_iterations = n_iterations,
                                                                        n_groups = n_groups,
                                                                        node_weights = node_weights,
                                                                        trials = trials,
                                                                        steps = steps,
                                                                        list = list,
                                                                        verbose = verbose))
    return(cluster_list_graph)
  }
  if(inherits(graphs, "tbl_graph")){
    list <- FALSE
    cluster_graph <-   detect_cluster(graphs,
                                      clustering_method = clustering_method,
                                      objective_function = objective_function,
                                      resolution = resolution,
                                      n_iterations = n_iterations,
                                      n_groups = n_groups,
                                      node_weights = node_weights,
                                      trials = trials,
                                      steps = steps,
                                      list = list,
                                      verbose = verbose)
    return(cluster_graph)
  }
}

# function in the tidygraph style to import Leiden community detection
group_leiden <- function(graph = graph,
                         objective_function = objective_function,
                         weights = weights,
                         resolution = resolution,
                         n_iterations = n_iterations,
                         node_weights = node_weights){
  igraph::cluster_leiden(graph,
                         resolution_parameter = resolution,
                         objective_function = objective_function,
                         weights = weights,
                         n_iterations = n_iterations,
                         vertex_weights = node_weights) %>%
    igraph::membership()
}

# extracting the appropriate clustering function depending on the method chosen
extract_clustering_method <- function(clustering_method = clustering_method){
  . <- objective_function <- weights <- resolution <- n_iterations <- node_weights <- trials <- steps <- method <- NULL

  function_table <- tribble(
    ~ method, ~functions,
    "leiden", expr(group_leiden(graph, objective_function = objective_function, weights = weights, resolution = resolution, n_iterations = n_iterations, node_weights = node_weights)),
    "louvain", expr(group_louvain(weights = weights)),
    "fast_greedy", expr(group_fast_greedy(weights = weights, n_groups = n_groups)),
    "infomap", expr(group_infomap(weights = weights, node_weights = node_weights, trials = trials)),
    "walktrap", expr(group_walktrap(weights = weights, steps = steps, n_groups = n_groups)))
  fun <- function_table %>%
    filter(method == clustering_method) %>%
    .[["functions"]] %>%
    .[[1]]

  return(fun)
}

# function to detect the clusters on one graph
detect_cluster <- function(graph,
                           clustering_method = clustering_method,
                           objective_function = objective_function,
                           resolution = resolution,
                           n_iterations = n_iterations,
                           n_groups = n_groups,
                           node_weights = node_weights,
                           trials = trials,
                           steps = steps,
                           list = list,
                           verbose = verbose){
  . <- from <- to <- NULL

  weights <- igraph::E(graph)$weight
  if(clustering_method %in% c("infomap", "leiden") & !is.null(node_weights)){
    node_weights <- graph %N>% as.data.frame() %>%  .[[node_weights]]
  }
  fun <- extract_clustering_method(clustering_method)
  cluster_col <- paste0("cluster_", clustering_method)
  size_col <- paste0("size_cluster_", clustering_method)

  graph <- graph %N>%
    mutate({{ cluster_col }} := eval(fun),
           {{ cluster_col }} := sprintf("%02d", as.integer(.data[[cluster_col]])),
           {{ size_col }} := n()) %>%
    group_by(across({{ cluster_col }})) %>%
    mutate({{ size_col }} := n()/.data[[size_col]]) %>%
    ungroup() %E>%
    dplyr::mutate("{ cluster_col }_from" := .N()[[cluster_col]][from],
                  "{ cluster_col }_to" := .N()[[cluster_col]][to],
                  {{ cluster_col }} := if_else(.data[[paste0(cluster_col, "_from")]] == .data[[paste0(cluster_col, "_to")]],
                                               .data[[paste0(cluster_col, "_from")]],
                                               "00"))
  if(verbose == TRUE){
    nb_clusters <- graph %N>%
      as.data.frame() %>%
      .[[cluster_col]] %>%
      unique %>%
      length()

    max_size <- graph %N>%
      as.data.frame %>%
      .[[size_col]] %>%
      max() %>%
      round(3) * 100

    if(list == TRUE) cli::cli_h1("Cluster detection for the {.val {graph %N>% as.data.frame() %>% .$time_window %>% unique()}} period")
    cli::cli_alert_info("The {.emph {clustering_method}} method detected {.val {nb_clusters}} clusters. The biggest cluster represents {.val {max_size}%} of the network.")
  }
  return(graph)
}
