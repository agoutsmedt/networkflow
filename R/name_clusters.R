#' Give readable names to clusters
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A cluster identifier ("01", "cl_12") says nothing about its content.
#' `name_clusters()` gives each cluster a provisional label that helps
#' reading plots and tables, with one of four methods: the words that
#' distinguish the cluster from the others (`"tf-idf"`), the metadata of
#' its most central node (`"tidygraph_functions"`) or of its node with the
#' highest value of a given column (`"given_column"`), or a label written by
#' a language model from a description of the cluster (`"llm"`, optional,
#' through the [`ellmer`](https://ellmer.tidyverse.org) package: a model on
#' your computer or an online service). These labels are
#' starting points for interpretation, not the result of it: naming a
#' cluster requires reading its documents, the references they share, and
#' what distinguishes them from neighbouring clusters
#' \insertCite{ding2011}{networkflow}.
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param method `"tf-idf"` (chosen when no method is given),
#'   `"tidygraph_functions"`, `"given_column"` or `"llm"` (see the details).
#' @param name_dynamic_clusters Logical. `TRUE` when `cluster_id` identifies
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
#' @param order_by Character scalar, a numeric node column used to rank the
#'   nodes of each cluster, for two methods (ignored by the others):
#'   * `"given_column"`: the node with the highest value names the cluster,
#'     e.g. `"node_size"` from [build_network()] with `compute_size = TRUE`
#'     (the most cited document of the cluster);
#'   * `"llm"` (required): the `nb_nodes` nodes with the highest values are
#'     shown to the model, e.g. `"within_module_degree"` from
#'     [add_node_roles()] (the nodes most tied to their own cluster). For
#'     dynamic clusters, a node present in several windows is ranked by its
#'     highest value.
#' @param text_columns For `"tf-idf"`, the node column(s) with text (titles,
#'   abstracts, keywords), passed to [extract_tfidf()].
#' @param nb_terms_label For `"tf-idf"`, the number of terms in each label (3
#'   by default), separated by commas. A term that is part of a term already
#'   in the label, or contains one, is skipped ("business" after "business
#'   cycle").
#' @param prompt_columns For `"llm"` (required), character vector, the node
#'   column(s) written in the description of each cluster sent to the model: what the model
#'   knows about the nodes (e.g. `c("source_title", "source_year",
#'   "cluster_label")`). A column with one value per cluster (a label
#'   computed before, e.g. with `method = "tf-idf"`) is given once for the
#'   cluster.
#' @param nb_nodes For `"llm"` (required), whole number, the number of nodes
#'   listed for each cluster, highest `order_by` first. `0` lists none: the model then
#'   reads only the `context` and the columns with one value per cluster.
#' @param llm For `"llm"`, the model: a chat object of
#'   [`ellmer`](https://ellmer.tidyverse.org), configured as you would for any
#'   other use, e.g. `ellmer::chat_ollama(model = "mistral", params =
#'   ellmer::params(temperature = 0, seed = 123))` (see the details). Not
#'   needed with `preview = TRUE`.
#' @param instructions For `"llm"`, character scalar, the system prompt sent
#'   before the description of each cluster. `NULL` (default) uses the
#'   instructions given in the details. Change them to ask for longer
#'   labels, another language, another vocabulary... To start from the
#'   default, get it with `preview = TRUE` (column `instructions`), e.g.
#'   `instructions = paste(prompts$instructions[1], "Answer in French.")`.
#' @param context For `"llm"`, optional character scalar describing the
#'   network to the model: what the nodes are, the field, the period, the
#'   question (e.g. `"Economics articles on stagflation, 1970-2010, linked
#'   by their shared references"`), written at the top of each description.
#'   Strongly recommended: in our tests on three corpora and three local
#'   models, it raised the quality of the labels more than any other change
#'   to the description (see the details).
#' @param preview For `"llm"`, logical. If `TRUE`, returns what the model
#'   would receive, without calling it (no model needed): a tibble with one
#'   row per cluster, its identifier, the `instructions` (the system prompt)
#'   and the `description` of the cluster (the user message), plus a `graph`
#'   column for a list of graphs named separately. Print a
#'   description with `cat()`. `FALSE` (default) names the clusters.
#' @param name_merged_clusters `r lifecycle::badge("deprecated")` Former name
#'   of `name_dynamic_clusters`.
#' @param ... For `"tf-idf"`, other arguments of [extract_tfidf()] (e.g.
#'   `clean_word_method`, `n_gram`, `ngrams_filter`). Must be empty for the
#'   other methods (a misspelt argument is an error).
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
#' the cluster, rare in the others. With `name_dynamic_clusters = FALSE`, the
#' comparison is made within each graph, so clusters of different windows
#' may get the same label.
#'
#' **Most central node.** `"tidygraph_functions"` computes the centrality
#' given in `tidygraph_function` on each graph (it is added as a node
#' column) and labels each cluster with the node with the highest value.
#' The centrality is computed on the whole graph, not within the cluster.
#' This method needs a network structure and therefore cannot be used with
#' `name_dynamic_clusters = TRUE`.
#'
#' **Given column.** `"given_column"` labels each cluster with its node with
#' the highest `order_by` value: for an article network, typically its most
#' cited article.
#'
#' **Language model.** `"llm"` writes a description of each cluster and asks
#' the model given in `llm` for a label. The model is a chat object of the
#' [`ellmer`](https://ellmer.tidyverse.org) package (only needed for this
#' method), so any model ellmer reaches can be used:
#' * a model running on your computer, so that the content of the nodes
#'   stays on it: `ellmer::chat_ollama()` ([Ollama](https://ollama.com)),
#'   `chat_lmstudio()`, `chat_vllm()`, or `chat_openai_compatible()` for
#'   another local server (e.g. llama.cpp);
#' * an online service (`chat_openai()`, `chat_anthropic()`...): the
#'   descriptions of the clusters are then sent to it.
#'
#' Configure the chat object as for any other use of ellmer; for more stable
#' labels, set `params = ellmer::params(temperature = 0, seed = 123)` (this
#' does not guarantee the same labels on another computer, and some online
#' services ignore `seed`). Each cluster is sent alone
#' to a copy of `llm` (its earlier conversation is not sent), with
#' `instructions` as the system prompt, and the answer is constrained to one
#' label (`chat_structured()`). The label is cleaned: no quotes or final
#' period, words separated by spaces ("Asset_Pricing" becomes "Asset
#' Pricing"). The default instructions are:
#'
#' ```
#' `r llm_default_instructions`
#' ```
#'
#' **Writing the prompt.** We compared nine prompts, with three local models
#' (`gemma3:4b`, `gemma3:12b`, `qwen3:14b`) on 21 clusters of three corpora
#' (articles of economics), the labels being scored blind by another model
#' (accuracy and specificity, from 1 to 5; a small test, to be taken as an
#' indication). What mattered most, in order:
#' 1. The instructions. Short ones ("a label of 2 or 3 words") give generic
#'    labels ("Monetary policy analysis"); the default above, which asks for
#'    the specific topic, forbids empty words and gives examples, raised the
#'    scores by 0.5 to 0.8 points whatever the model.
#' 2. The `context`: one sentence on what the nodes are, the field and the
#'    period raised the scores by a further 0.25 to 0.5 points. It helps the
#'    model most where it cannot tell the cluster apart by itself.
#' 3. Little else: 10 nodes are enough (25 did not help, 5 was a bit worse);
#'    year and journal in `prompt_columns` changed little; and a column of
#'    key terms (tf-idf) did not help and produced more repeated labels.
#' A larger model did not systematically beat a small one once the prompt was
#' good. If you change `instructions`, keep the rules asking for a specific
#' topic and keep the `context`.
#'
#' With `ellmer::chat_ollama()`, a model that thinks before answering (e.g.
#' `qwen3`) can take minutes per cluster or time out, because ellmer reaches
#' Ollama through its OpenAI-compatible endpoint, where `think = FALSE` is
#' ignored: use `api_args = list(reasoning_effort = "none")`.
#'
#' The description of a cluster gathers, when available:
#' * the `context` you give;
#' * the number of nodes of the cluster (and of windows, for dynamic
#'   clusters);
#' * the columns of `prompt_columns` with one value per cluster (e.g. a
#'   tf-idf label computed before with `method = "tf-idf"`), given once;
#' * its `nb_nodes` main nodes (highest `order_by`), described by the other
#'   columns of `prompt_columns`.
#'
#' Everything comes from the node tables of `graphs`. The value of a node
#' longer than 300 characters (an abstract) is cut, and its line breaks are
#' removed; columns with one value per cluster are written in full. Use
#' `preview = TRUE` to read the descriptions before running the model.
#'
#' If the model fails on the first cluster, `name_clusters()` stops: the
#' problem is likely its configuration (try `llm$chat("Hello")`). A later
#' cluster whose label fails is labelled "no_name" with a warning. By
#' default, each answer may take up to 300 seconds, with up to 3 tries (the options
#' `ellmer_timeout_s` and `ellmer_max_tries` of ellmer): raise
#' `options(ellmer_timeout_s = )` for a slow model. A warning is given when
#' a description exceeds about 4000 tokens: some local servers cut longer
#' prompts without warning (Ollama has a context of 4096 tokens by default).
#'
#' Nodes of an unnamed cluster get "no_name".
#'
#' @return The same object as `graphs`, with a new node column `label_name`,
#'   also joined to the edges through their cluster column. With
#'   `method = "llm"` and `preview = TRUE`, a tibble of the messages that
#'   would be sent to the model (see `preview`).
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
#' # What a language model would read for each cluster (no model needed):
#' # titles, years and the tf-idf label, for the 10 nodes most tied to their
#' # own cluster
#' network <- add_node_roles(network, cluster_id = "cluster_leiden")
#' prompts <- name_clusters(
#'   network,
#'   method = "llm",
#'   cluster_id = "cluster_leiden",
#'   prompt_columns = c("source_title", "source_year", "cluster_label"),
#'   order_by = "within_module_degree",
#'   nb_nodes = 10,
#'   context = "Economics articles on stagflation, linked by their shared references.",
#'   preview = TRUE
#' )
#' cat(prompts$instructions[1])
#' cat(prompts$description[1])
#'
#' \dontrun{
#' # A label written by a model running on your computer, here with Ollama
#' # (install Ollama, then `ollama pull mistral` in a terminal)
#' network <- name_clusters(
#'   network,
#'   method = "llm",
#'   cluster_id = "cluster_leiden",
#'   prompt_columns = c("source_title", "source_year", "cluster_label"),
#'   order_by = "within_module_degree",
#'   nb_nodes = 10,
#'   label_name = "llm_label",
#'   llm = ellmer::chat_ollama(model = "mistral",
#'                             params = ellmer::params(temperature = 0, seed = 123)),
#'   context = "Economics articles on stagflation, linked by their shared references."
#' )
#' }
#'
#' @export
name_clusters <- function(graphs,
                          method = c("tidygraph_functions", "given_column", "tf-idf", "llm"),
                          name_dynamic_clusters = FALSE,
                          cluster_id,
                          label_columns,
                          label_name = "cluster_label",
                          tidygraph_function = NULL,
                          order_by = NULL,
                          text_columns = NULL,
                          nb_terms_label = 3,
                          prompt_columns = NULL,
                          nb_nodes = NULL,
                          llm = NULL,
                          instructions = NULL,
                          context = NULL,
                          preview = FALSE,
                          name_merged_clusters = NULL,
                          ...) {
  term <- tf_idf <- NULL
  # `name_merged_clusters` was renamed `name_dynamic_clusters` in 0.0.9.9001
  if (!is.null(name_merged_clusters)) {
    cli::cli_warn(c(
      "{.arg name_merged_clusters} is deprecated since networkflow 0.0.9.9001.",
      "i" = "Use {.arg name_dynamic_clusters} instead."
    ))
    name_dynamic_clusters <- name_merged_clusters
  }
  if (length(method) > 1) {
    method <- "tf-idf"
    cli::cli_inform(c("i" = "No {.arg method} given: using {.val tf-idf}."))
  }
  method <- rlang::arg_match(method, c("tidygraph_functions", "given_column", "tf-idf", "llm"))
  # `...` is only for extract_tfidf(): elsewhere, a misspelt argument
  # (`preveiw = TRUE`) would be ignored silently.
  if (method != "tf-idf") rlang::check_dots_empty()
  if (!rlang::is_bool(name_dynamic_clusters)) {
    cli::cli_abort("{.arg name_dynamic_clusters} must be {.code TRUE} or {.code FALSE}.")
  }
  single <- is_single_graph(graphs)
  if (single && name_dynamic_clusters) {
    cli::cli_abort(c(
      "{.code name_dynamic_clusters = TRUE} needs a list of graphs.",
      "i" = "It names clusters defined across windows, such as those of {.fn merge_dynamic_clusters}."
    ))
  }
  if (single) graphs <- list(graphs)
  for (graph in graphs) check_columns(node_table(graph), cluster_id, "graphs")
  if (name_dynamic_clusters) check_unique_clusters(graphs, cluster_id)

  # The column used to choose the representative node of each cluster.
  if (method == "tidygraph_functions") {
    fun <- rlang::enquo(tidygraph_function)
    if (rlang::quo_is_null(fun)) {
      cli::cli_abort("With {.code method = \"tidygraph_functions\"}, give a centrality in {.arg tidygraph_function}, e.g. {.code tidygraph::centrality_pagerank()}.")
    }
    if (name_dynamic_clusters) {
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
  if (method == "llm") {
    # All the arguments are checked before the first call to the model.
    if (is.null(prompt_columns)) {
      cli::cli_abort(c(
        "With {.code method = \"llm\"}, give the node column(s) written in the description of each cluster in {.arg prompt_columns}, e.g. titles and years.",
        if (!missing(label_columns)) c("i" = "{.arg label_columns} is used by the other methods: for {.val llm}, it is {.arg prompt_columns}.")
      ))
    }
    if (is.null(order_by)) {
      cli::cli_abort(c(
        "With {.code method = \"llm\"}, give in {.arg order_by} the numeric node column used to choose the nodes shown to the model.",
        "i" = "E.g. {.code order_by = \"within_module_degree\"}, after {.code add_node_roles()}: the nodes most tied to their own cluster."
      ))
    }
    if (is.null(nb_nodes)) {
      cli::cli_abort("With {.code method = \"llm\"}, give in {.arg nb_nodes} the number of nodes listed for each cluster, e.g. {.code nb_nodes = 10}.")
    }
    if (!(is.numeric(nb_nodes) && length(nb_nodes) == 1 && !is.na(nb_nodes) && nb_nodes >= 0 && nb_nodes == round(nb_nodes))) {
      cli::cli_abort("{.arg nb_nodes} must be a whole number, 0 or more, not {.obj_type_friendly {nb_nodes}}{if (is.numeric(nb_nodes) && length(nb_nodes) == 1) paste0(' (', nb_nodes, ')') else ''}.")
    }
    if (!rlang::is_bool(preview)) {
      cli::cli_abort("{.arg preview} must be {.code TRUE} or {.code FALSE}.")
    }
    if (!is.null(context) && !rlang::is_string(context) || rlang::is_string(context, "")) {
      cli::cli_abort("{.arg context} must be one character string describing the network (or {.code NULL}).")
    }
    if (is.null(context)) {
      cli::cli_inform(
        c("!" = "No {.arg context} given: the model only sees the cluster columns and the main nodes.",
          "i" = "A one-sentence {.arg context} (what the nodes are, the field, the period) usually improves the labels."),
        class = "networkflow_no_context"
      )
    }
    if (is.null(instructions)) instructions <- llm_default_instructions
    if (!rlang::is_string(instructions) || !nzchar(trimws(instructions))) {
      cli::cli_abort("{.arg instructions} must be one character string.")
    }
    if (length(order_by) != 1) cli::cli_abort("{.arg order_by} must be the name of one column.")
    prompt_columns <- unique(prompt_columns)
    for (graph in graphs) {
      nodes <- node_table(graph)
      check_columns(nodes, c(prompt_columns, order_by), "graphs")
      if (!is.numeric(nodes[[order_by]])) {
        cli::cli_abort("{.arg order_by} must be a numeric column: {.field {order_by}} is {.obj_type_friendly {nodes[[order_by]]}}.")
      }
      lists <- prompt_columns[vapply(prompt_columns, function(col) is.list(nodes[[col]]), logical(1))]
      if (length(lists) > 0) {
        cli::cli_abort(c(
          "{.arg prompt_columns} cannot contain list columns: {.field {lists}}.",
          "i" = "Join their values into one string per node first, e.g. with {.code vapply(x, paste, character(1), collapse = \", \")}."
        ))
      }
    }
    if (preview) {
      # What the model would receive, without calling it (no model needed).
      node_sets <- if (name_dynamic_clusters) {
        list(dplyr::bind_rows(lapply(graphs, node_table), .id = ".nf_window"))
      } else {
        lapply(graphs, node_table)
      }
      graph_names <- if (is.null(names(graphs))) as.character(seq_along(graphs)) else names(graphs)
      out <- dplyr::bind_rows(lapply(seq_along(node_sets), function(i) {
        prompts <- llm_cluster_prompts(node_sets[[i]], cluster_id, prompt_columns, order_by, nb_nodes, context = context)
        dplyr::tibble(graph = graph_names[i], !!cluster_id := names(prompts),
                      instructions = instructions, description = unname(prompts))
      }))
      if (single || name_dynamic_clusters) out$graph <- NULL
      return(out)
    }
    llm_check_chat(llm)
    llm_state <- new.env()
    llm_state$answered <- FALSE
    llm_state$call <- rlang::current_env()
  }

  # Label table (one row per cluster) computed on a node table.
  make_labels <- function(nodes) {
    if (method == "llm") {
      prompts <- llm_cluster_prompts(nodes, cluster_id, prompt_columns, order_by, nb_nodes, context = context)
      labels <- dplyr::tibble(!!cluster_id := names(prompts), label = unname(llm_labels(prompts, llm, instructions, llm_state)))
      labels <- labels[!is.na(labels$label), ]
      # The identifiers keep their type (numbers stay numbers) for the join.
      ids <- unique(nodes[[cluster_id]])
      labels[[cluster_id]] <- ids[match(labels[[cluster_id]], as.character(ids))]
    } else if (method == "tf-idf") {
      # More terms than needed are extracted, so that terms overlapping a better
      # one can be skipped ("business" after "business cycle").
      terms <- extract_tfidf(nodes, grouping_columns = cluster_id, text_columns = text_columns,
                             nb_terms = 4 * nb_terms_label, ...)
      terms <- terms[order(-tf_idf), .SD, by = cluster_id, env = list(cluster_id = cluster_id)]
      labels <- terms[, .(label = label_from_terms(term, nb_terms_label)), by = cluster_id,
                      env = list(cluster_id = cluster_id)]
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
    # The column of an empty window is logical (no value): give it the type of
    # the labels, so that the join works
    as_label_type <- function(x) if (is.logical(x)) labels[[cluster_id]][0][seq_along(x)] else x
    graph <- graph %N>%
      dplyr::mutate(!!cluster_id := as_label_type(.data[[cluster_id]])) %>%
      dplyr::select(-dplyr::any_of(label_name)) %>%
      dplyr::left_join(labels, by = cluster_id) %>%
      dplyr::mutate(!!label_name := dplyr::coalesce(.data[[label_name]], "no_name"))
    edges <- tidygraph::as_tibble(graph, active = "edges")
    if (cluster_id %in% names(edges)) {
      graph <- graph %E>%
        dplyr::mutate(!!cluster_id := as_label_type(.data[[cluster_id]])) %>%
        dplyr::select(-dplyr::any_of(label_name)) %>%
        dplyr::left_join(labels, by = cluster_id) %>%
        tidygraph::activate("nodes")
    }
    graph
  }

  if (name_dynamic_clusters) {
    all_nodes <- dplyr::bind_rows(lapply(graphs, node_table), .id = ".nf_window")
    labels <- make_labels(all_nodes)
    graphs <- lapply(graphs, add_labels, labels = labels)
  } else {
    graphs <- lapply(graphs, function(graph) add_labels(graph, make_labels(node_table(graph))))
  }
  if (single) graphs <- graphs[[1]]
  graphs
}

# With name_dynamic_clusters = TRUE, a same identifier is one cluster across
# windows. When every window numbers its own clusters (the clusters of
# add_clusters(), not those of merge_dynamic_clusters()), the identifiers
# are not unique: "01" of each window would be named as one cluster. Sign of
# it, in 3 windows or more: all the windows together have no more
# identifiers than the window with the most.
check_unique_clusters <- function(graphs, cluster_id) {
  ids <- lapply(graphs, function(graph) unique(stats::na.omit(node_table(graph)[[cluster_id]])))
  ids <- ids[lengths(ids) > 0]
  if (length(ids) < 3) return(invisible())
  total <- length(unique(unlist(ids)))
  if (total == max(lengths(ids))) {
    cli::cli_warn(c(
      "The clusters of {.field {cluster_id}} may not be unique across windows: the {length(ids)} windows share the same {total} identifier{?s}.",
      "i" = "With {.code name_dynamic_clusters = TRUE}, nodes of different windows with the same identifier are named as one cluster.",
      "i" = "Use the clusters of {.fn merge_dynamic_clusters} (e.g. {.val {paste0('dynamic_', cluster_id)}}), or {.code name_dynamic_clusters = FALSE} to name the clusters of each window."
    ))
  }
  invisible()
}

# The label of a cluster: its first `n` terms (by decreasing tf-idf), skipping
# a term that is part of a term already kept, or contains one ("business"
# and "real business" after "business cycle"... but not "cycle theory").
label_from_terms <- function(terms, n) {
  kept <- character(0)
  for (term in terms) {
    padded <- paste0(" ", term, " ")
    overlaps <- vapply(kept, function(k) {
      grepl(padded, paste0(" ", k, " "), fixed = TRUE) || grepl(paste0(" ", k, " "), padded, fixed = TRUE)
    }, logical(1))
    if (!any(overlaps)) kept <- c(kept, term)
    if (length(kept) == n) break
  }
  paste(kept, collapse = ", ")
}
