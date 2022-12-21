#' Automatically Attributing Names to Clusters
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The function gives a name to networks clusters.
#' It also gives the edges the name of their cluster. The clusters are named according
#' to the column chosen by the user (for instance, in the case nodes are articles, the
#' name may be the author and date of an article).
#'
#'
#' @param graphs
#' A tibble graph (from [tidygraph](https://tidygraph.data-imaginist.com/))
#' or a list of tibble graphs.
#'
#' @param method
#' The method for finding the names, among `tidygraph_functions`, `given_column`,
#' and `tf-idf` (see the details). The `tf-idf` method is chosen by default.
#'
#' @param name_merged_clusters
#' Set to `TRUE` if your clusters have been established for
#' all your tibble graphs and thus are unique. Typically, you have such clusters
#' after running `merge_dynamic_clusters()`.
#'
#' @param cluster_id
#' The column you want to name. Generally, the column with the identifier of the
#' clusters, whether the simple cluster identified with `add_clusters()` or the
#' merged clusters identified with `merge_dynamic_clusters()`.
#'
#' @param label_columns
#' The column you want to be used to name the clusters. If the nodes are article,
#' you can choose, for instance, the columns with the author of the article and the
#' date of publication.
#'
#' @param label_name
#' The name of the column with cluster names, that will be created by the function.
#' "label" by default.
#'
#' @param tidygraph_function
#' For the `tidygraph_functions` method (see the details), the centrality measure to be
#' chosen among the measures implemented in in `tidygraph`  (see `tidygraph::centrality()`).
#'
#' @param order_by
#' For the `given_column` method, the column within the nodes list of your
#' tibble graph(s) you want to be used to classify nodes and choose names. This must be
#' a numeric column. For instance, you can use the `node_size` column of your network
#' if you have set `compute_size` to `TRUE` in `build_network()` or
#' `build_dynamic_networks()`.
#'
#' @param text_columns
#' For the `tf-idf` method, the columns with the text you want to analyze.
#' If you give multiple columns, they will be united to extract the terms. This is
#' a parameter of `extract_tfidf()`.
#'
#' @param nb_terms_label
#' For the `tf-idf` method, the number of terms you want to be used to serve a the
#' name of a cluster. Terms will be separated by a comma. This is a parameter of
#' `extract_tfidf()`.
#'
#' @param ...
#' Additional arguments from `extract_tfidf`, outside of those referred above as well as
#' of `grouping_across_list` which is not relevant here.
#'
#' @details
#' The node to be used for naming the community is chosen depending on 3 methods:
#'
#' - the `tidygraph_functions` method: the name of a cluster comes from the node,
#' within the cluster, which has the highest centrality measure. The user can choose the
#' different centrality measure implemented  in `tidygraph` (see `tidygraph::centrality()` for details).
#'
#' - the `given_column` method: the user gives a column of the tibble graph(s), with numeric
#' values, that will be used to classify the nodes and choose the name of each cluster.
#'
#' - the `tf-idf` method: clusters are name according to the terms with the highest
#' tf-idf value for each cluster. The user furnishes one or several columns with text,
#' and the function extracts the term and calculate the tf-idf value of each term, depending
#' on all the clusters. This method uses `extract_tfidf()`.
#'
#' @details
#' Please note that, when `name_merged_clusters` is set to `FALSE`, the TF-IDF is computed
#' tibble graph by tibble graph. It means that it is more likely that clusters in different
#' tibble graphs will share the same name.
#'
#' @return The same tibble graph or list of tibble graphs with a new column with the
#' names of the clusters, for both nodes and edges. If you choose the `tidygraph_functions` method,
#' the function also returns for nodes a column with the centrality measure computed.
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
#' temporal_networks <- add_clusters(temporal_networks,
#' objective_function = "modularity",
#' clustering_method = "leiden")
#'
#' # You can name the clusters in each tibble graphs:
#'
#' temporal_networks_with_names <- name_clusters(graphs = temporal_networks,
#' method = "tidygraph_functions",
#' name_merged_clusters = FALSE,
#' cluster_id = "cluster_leiden",
#' label_columns = c("Author", "Year"),
#' tidygraph_function = centrality_pagerank())
#'
#' temporal_networks_with_names[[1]]
#'
#' # Or you can name the dynamic clusters:
#'
#' temporal_networks <- merge_dynamic_clusters(temporal_networks,
#' cluster_id = "cluster_leiden",
#' node_id = "ID_Art",
#' threshold_similarity = 0.51,
#' similarity_type = "partial")
#'
#' temporal_networks_with_names <- name_clusters(graphs = temporal_networks,
#' method = "tf-idf",
#' name_merged_clusters = TRUE,
#' cluster_id = "dynamic_cluster_leiden",
#' label_columns = c("Author", "Year"),
#' text_columns = "Title",
#' nb_terms_label = 5,
#' clean_word_method = "lemmatise")
#'
#' temporal_networks_with_names[[1]]
#'
#' @export
#' @import tidygraph
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import cli
#' @import lifecycle
#'

name_clusters <- function(graphs,
                          method = c("tidygraph_functions", "given_column", "tf-idf"),
                          name_merged_clusters = FALSE,
                          cluster_id,
                          label_columns,
                          label_name = "cluster_label",
                          tidygraph_function = NULL,
                          order_by = NULL,
                          text_columns = NULL,
                          nb_terms_label = 3,
                          ...){

  . <- max_value <- term <- NULL

  if(length(method) > 1){
    method <- "tf-idf"
    cli::cli_alert_info("No {.emph method} chosen. The {.val tf-idf} method has been chosen by default.")
  }

  if(inherits(graphs, "tbl_graph")){
    if(name_merged_clusters == TRUE){
      cli::cli_abort("You have set {.emph name_merged_clusters} to {.val TRUE}. Please use a list of tibble graphs and not just a unique tibble graph.")
    }
    graphs <- list(unique_graph = graphs)
    unique_graph <- TRUE
  } else {
    unique_graph <- FALSE
  }


  if(method == "tidygraph_functions"){
    if(name_merged_clusters == TRUE){
      cli::cli_abort("The {.val tidygraph_functions} method is inappropriate as you have set {.emph name_merged_clusters} to {.val TRUE}.
                 The {.val tidygraph_functions} method only works at the graph level as it depends on the structure of the corresponding network.")
    }
    fun <- enexpr(tidygraph_function)
    order_by <- as_label(fun) %>%
      stringr::str_extract("centrality_.+(?=\\()")

    graphs <- lapply(graphs, function(tbl) tbl %N>%
                       dplyr::mutate({{ order_by }} := eval(fun)))
  }

  if(name_merged_clusters == FALSE){
    for(i in 1:length(graphs)){
      if(method %in% c("tidygraph_functions", "given_column")){
        labels <- graphs[[i]] %N>%
          dplyr::as_tibble() %>%
          dplyr::group_by(across({{ cluster_id }})) %>%
          dplyr::slice_max(order_by = .data[[order_by]], n = 1) %>%
          tidyr::unite({{ label_name }}, all_of(label_columns), sep = "_") %>%
          dplyr::select(all_of(cluster_id), all_of(label_name))
      } else if(method == "tf-idf"){
        labels <- extract_tfidf(graphs[[i]],
                                grouping_columns = cluster_id,
                                text_columns = text_columns,
                                nb_terms = nb_terms_label,
                                ...) %>%
          .[, (label_name) := paste0(term, collapse = ", "), by = cluster_id,
            env = list(cluster_id = cluster_id)] %>%
          select(all_of(cluster_id), all_of(label_name)) %>%
          unique
      } else {
        cli::cli_abort(c("The {.emph method} chosen does not exist. Please choose between: ",
                         "*" = "\"tf-idf\"",
                         "*" = "\"tidygraph_functions\"",
                         "*" = "\"given_column\""))
      }
      graphs[[i]] <- graphs[[i]] %N>%
        left_join(labels, by = cluster_id) %E>%
        left_join(labels, by = cluster_id)
    }
    if(unique_graph == TRUE) graphs <- graphs[[1]]
  } else if (name_merged_clusters == TRUE){
    dt <- lapply(graphs, function(graph) graph %N>%
                   data.table::as.data.table()) %>%
      data.table::rbindlist(idcol = "list_names")

    if(method == "given_column"){
      labels <- dt %>%
        dplyr::group_by(across({{ cluster_id }})) %>%
        dplyr::slice_max(order_by = .data[[order_by]], n = 1) %>%
        tidyr::unite({{ label_name }}, all_of(label_columns), sep = "_") %>%
        dplyr::select(all_of(cluster_id), all_of(label_name))
    } else if(method == "tf-idf"){
      labels <- extract_tfidf(dt,
                              grouping_columns = cluster_id,
                              text_columns = text_columns,
                              nb_terms = nb_terms_label,
                              ...) %>%
        .[, (label_name) := paste0(term, collapse = ", "), by = cluster_id,
          env = list(cluster_id = cluster_id)] %>%
        select(all_of(cluster_id), all_of(label_name)) %>%
        unique
    }
    graphs <- lapply(graphs, function(tbl) tbl %N>%
                       left_join(labels, by = cluster_id))
  }
  return(graphs)
}