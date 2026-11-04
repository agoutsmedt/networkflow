#' Give readable names to clusters
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A cluster identifier ("01", "cl_12") says nothing about its content.
#' `name_clusters()` gives each cluster a provisional label that helps
#' reading plots and tables, with one of three methods: the words that
#' distinguish the cluster from the others (`"tf-idf"`), or the metadata of
#' its most central node (`"tidygraph_functions"`) or of its node with the
#' highest value of a given column (`"given_column"`). These labels are
#' starting points for interpretation, not the result of it: naming a
#' cluster requires reading its documents, the references they share, and
#' what distinguishes them from neighbouring clusters
#' \insertCite{ding2011}{networkflow}.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param method `"tf-idf"` (chosen when no method is given),
#'   `"tidygraph_functions"` or `"given_column"` (see the details).
#' @param name_merged_clusters Logical. `TRUE` when `cluster_id` identifies
#'   clusters across a list of graphs (the dynamic clusters of
#'   [merge_dynamic_clusters()]): each cluster then gets one name, computed on
#'   all its nodes in all windows. `FALSE` (default) names the clusters of
#'   each graph separately.
#' @param cluster_id Character scalar, the node column with the clusters to
#'   name.
#' @param label_columns For `"tidygraph_functions"` and `"given_column"`, the
#'   node column(s) used to build the label of the chosen node, joined by
#'   "_" (e.g. `c("source_author", "source_year")`).
#' @param label_name Character scalar, the name of the new column
#'   (`"cluster_label"` by default).
#' @param tidygraph_function For `"tidygraph_functions"`, a call to a
#'   centrality function of [`tidygraph`](https://tidygraph.data-imaginist.com/), such as
#'   `tidygraph::centrality_pagerank()` (see [tidygraph::centrality]).
#' @param order_by For `"given_column"`, the numeric node column used to
#'   choose the node (e.g. `"node_size"` from [build_network()] with
#'   `compute_size = TRUE`, i.e. the most cited document of the cluster).
#' @param text_columns For `"tf-idf"`, the node column(s) with text (titles,
#'   abstracts, keywords), passed to [extract_tfidf()].
#' @param nb_terms_label For `"tf-idf"`, the number of terms in each label (3
#'   by default), separated by commas.
#' @param ... For `"tf-idf"`, other arguments of [extract_tfidf()] (e.g.
#'   `clean_word_method`, `n_gram`, `ngrams_filter`).
#'
#' @details
#' **tf-idf.** Each cluster is treated as one document made of the texts of
#' its nodes, and each term \eqn{t} (word or n-gram) gets the weight
#' \deqn{\mathrm{tf}_{tc} \times \mathrm{idf}_t = \frac{n_{tc}}{\sum_u n_{uc}}
#' \times \log\frac{C}{\mathrm{df}_t}}
#' \insertCite{sparckjones1972,salton1988}{networkflow}, with \eqn{n_{tc}} the
#' number of occurrences of \eqn{t} in cluster \eqn{c}, \eqn{C} the number of
#' clusters and \eqn{\mathrm{df}_t} the number of clusters where \eqn{t}
#' appears. The label gathers the terms with the highest weight: frequent in
#' the cluster, rare in the others. With `name_merged_clusters = FALSE`, the
#' comparison is made within each graph, so clusters of different windows
#' may get the same label.
#'
#' **Most central node.** `"tidygraph_functions"` computes the centrality
#' given in `tidygraph_function` on each graph (it is added as a node
#' column) and labels each cluster with the node with the highest value.
#' The centrality is computed on the whole graph, not within the cluster.
#' This method needs a network structure and therefore cannot be used with
#' `name_merged_clusters = TRUE`.
#'
#' **Given column.** `"given_column"` labels each cluster with its node with
#' the highest `order_by` value: for an article network, typically its most
#' cited article.
#'
#' Nodes of an unnamed cluster get "no_name".
#'
#' @return The same object as `graphs`, with a new node column `label_name`,
#'   also joined to the edges through their cluster column.
#'
#' @seealso [extract_tfidf()] to see more terms than the labels show.
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
#'   method = "similarity",
#'   edges_threshold = 1
#' )
#' network <- add_clusters(
#'   network,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' # The words of the titles that distinguish each cluster
#' network <- name_clusters(
#'   network,
#'   method = "tf-idf",
#'   cluster_id = "cluster_leiden",
#'   text_columns = "source_title",
#'   nb_terms_label = 3,
#'   clean_word_method = "none"
#' )
#'
#' # The article with the highest PageRank of each cluster
#' network <- name_clusters(
#'   network,
#'   method = "tidygraph_functions",
#'   cluster_id = "cluster_leiden",
#'   label_columns = c("source_author", "source_year"),
#'   label_name = "central_article",
#'   tidygraph_function = tidygraph::centrality_pagerank()
#' )
#' unique(tidygraph::as_tibble(network, active = "nodes")[
#'   c("cluster_leiden", "cluster_label", "central_article")
#' ])
#'
#' @export
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
                          ...) {
  term <- NULL
  if (length(method) > 1) {
    method <- "tf-idf"
    cli::cli_inform(c("i" = "No {.arg method} given: using {.val tf-idf}."))
  }
  method <- rlang::arg_match(method, c("tidygraph_functions", "given_column", "tf-idf"))
  single <- is_single_graph(graphs)
  if (single && name_merged_clusters) {
    cli::cli_abort(c(
      "{.code name_merged_clusters = TRUE} needs a list of graphs.",
      "i" = "It names clusters defined across windows, such as those of {.fn merge_dynamic_clusters}."
    ))
  }
  if (single) graphs <- list(graphs)
  for (graph in graphs) check_columns(node_table(graph), cluster_id, "graphs")

  # The column used to choose the representative node of each cluster.
  if (method == "tidygraph_functions") {
    fun <- rlang::enquo(tidygraph_function)
    if (rlang::quo_is_null(fun)) {
      cli::cli_abort("With {.code method = \"tidygraph_functions\"}, give a centrality in {.arg tidygraph_function}, e.g. {.code tidygraph::centrality_pagerank()}.")
    }
    if (name_merged_clusters) {
      cli::cli_abort(c(
        "{.val tidygraph_functions} cannot name clusters across windows.",
        "i" = "A centrality depends on one network; use {.val given_column} or {.val tf-idf}."
      ))
    }
    order_by <- regmatches(rlang::as_label(fun), regexpr("centrality_[a-z_]+", rlang::as_label(fun)))
    if (length(order_by) == 0) order_by <- "centrality"
    graphs <- lapply(graphs, function(graph) graph %N>% dplyr::mutate(!!order_by := !!fun))
  }
  if (method == "given_column") {
    if (is.null(order_by)) {
      cli::cli_abort("With {.code method = \"given_column\"}, give the numeric column used to choose the node in {.arg order_by}.")
    }
    for (graph in graphs) check_columns(node_table(graph), order_by, "graphs")
  }
  if (method == "tf-idf" && is.null(text_columns)) {
    cli::cli_abort("With {.code method = \"tf-idf\"}, give the column(s) with text in {.arg text_columns}.")
  }

  # Label table (one row per cluster) computed on a node table.
  make_labels <- function(nodes) {
    if (method == "tf-idf") {
      terms <- extract_tfidf(nodes, grouping_columns = cluster_id, text_columns = text_columns,
                             nb_terms = nb_terms_label, ...)
      labels <- terms[, .(label = paste0(term, collapse = ", ")), by = cluster_id]
    } else {
      labels <- nodes %>%
        dplyr::group_by(.data[[cluster_id]]) %>%
        dplyr::slice_max(order_by = .data[[order_by]], n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        tidyr::unite("label", dplyr::all_of(label_columns), sep = "_") %>%
        dplyr::select(dplyr::all_of(c(cluster_id, "label")))
    }
    labels <- dplyr::as_tibble(labels)
    names(labels)[names(labels) == "label"] <- label_name
    labels
  }

  add_labels <- function(graph, labels) {
    graph <- graph %N>%
      dplyr::select(-dplyr::any_of(label_name)) %>%
      dplyr::left_join(labels, by = cluster_id) %>%
      dplyr::mutate(!!label_name := dplyr::coalesce(.data[[label_name]], "no_name"))
    edges <- tidygraph::as_tibble(graph, active = "edges")
    if (cluster_id %in% names(edges)) {
      graph <- graph %E>%
        dplyr::select(-dplyr::any_of(label_name)) %>%
        dplyr::left_join(labels, by = cluster_id) %>%
        tidygraph::activate("nodes")
    }
    graph
  }

  if (name_merged_clusters) {
    all_nodes <- dplyr::bind_rows(lapply(graphs, node_table))
    labels <- make_labels(all_nodes)
    graphs <- lapply(graphs, add_labels, labels = labels)
  } else {
    graphs <- lapply(graphs, function(graph) add_labels(graph, make_labels(node_table(graph))))
  }
  if (single) graphs <- graphs[[1]]
  graphs
}
