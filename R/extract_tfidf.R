#' Find the terms that distinguish groups of nodes (tf-idf)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Clusters are defined by the structure of the network, not by the content
#' of the nodes. To interpret them, it helps to know which words are
#' specific to each cluster. `extract_tfidf()` extracts the words and
#' n-grams of one or several text columns (e.g. titles, abstracts) and
#' returns, for each group (e.g. each cluster), the terms with the highest
#' tf-idf \insertCite{sparckjones1972,salton1988}{networkflow}:
#' \deqn{\mathrm{tf}_{td} = \frac{n_{td}}{\sum_u n_{ud}}, \qquad
#' \mathrm{idf}_t = \log\frac{D}{\mathrm{df}_t}, \qquad
#' \text{tf-idf}_{td} = \mathrm{tf}_{td} \times \mathrm{idf}_t,}
#' with \eqn{n_{td}} the count of term \eqn{t} in group \eqn{d}, \eqn{D} the
#' number of groups and \eqn{\mathrm{df}_t} the number of groups containing
#' \eqn{t}. A term present in every group has a tf-idf of 0.
#'
#' @param data A `tbl_graph`, a list of `tbl_graph`s (their node tables are
#'   stacked) or a data frame.
#' @param text_columns Character vector, the column(s) with text. Several
#'   columns are pasted together, separated by ". ".
#' @param grouping_columns Character vector, the column(s) defining the
#'   groups, the "documents" of the tf-idf (e.g. `"cluster_leiden"`).
#' @param grouping_across_list Logical. For a list of graphs whose group
#'   identifiers repeat across graphs (cluster "01" exists in every window),
#'   `TRUE` adds the name of the graph to the groups, so that each cluster of
#'   each window is a separate document. Not needed for identifiers that are
#'   already unique across the list, such as dynamic clusters.
#' @param n_gram Integer, the maximum length of n-grams (2 by default:
#'   words and pairs of words).
#' @param stopwords_type Character scalar, the stop word list of
#'   [tidytext::get_stopwords()] (`"smart"` by default).
#' @param stopwords_vector Optional character vector of stop words, used
#'   instead of `stopwords_type`.
#' @param clean_word_method `"lemmatize"`, `"stemming"` (with the textstem
#'   package) or `"none"` (chosen when no method is given).
#' @param ngrams_filter Integer, the minimum number of occurrences of a term
#'   in the whole corpus (5 by default).
#' @param nb_terms Integer, the number of terms returned per group (5 by
#'   default).
#'
#' @details
#' The texts are lower-cased and split into n-grams of 1 to `n_gram` words
#' by [tidytext::unnest_tokens()] \insertCite{silge2017}{networkflow}. Each
#' word is lemmatised or stemmed if requested; n-grams containing a stop
#' word or a number are dropped, as are terms rarer than `ngrams_filter` in
#' the corpus and terms appearing only once in a group, which would
#' otherwise reach the top of small groups by accident.
#'
#' The choice of the groups decides what "specific" means: computed on the
#' clusters of one network, tf-idf finds what distinguishes a cluster from
#' the other clusters of the same period; computed on the stacked windows
#' with `grouping_across_list = TRUE`, it also distinguishes a cluster from
#' its own past and future.
#'
#' Lemmatising and stemming are optional and need the textstem package. For
#' larger corpora or other weighting schemes, the text-mining functions of
#' the mintR package (<https://codeberg.org/agoutsmedt/mintR>) can be used
#' on the node table.
#'
#' @return A `data.table` with one row per group and term, for the
#'   `nb_terms` best terms of each group: the grouping columns, `term`, `n`
#'   (count of the term in the group), `tf`, `idf` and `tf_idf`.
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
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
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
#' extract_tfidf(
#'   network,
#'   text_columns = "source_title",
#'   grouping_columns = "cluster_leiden",
#'   n_gram = 2,
#'   clean_word_method = "none",
#'   nb_terms = 3
#' )
#'
#' @export
extract_tfidf <- function(data,
                          text_columns,
                          grouping_columns,
                          grouping_across_list = FALSE,
                          n_gram = 2L,
                          stopwords_type = "smart",
                          stopwords_vector = NULL,
                          clean_word_method = c("lemmatize", "stemming", "none"),
                          ngrams_filter = 5L,
                          nb_terms = 5L) {
  text <- n <- term <- total_term <- document <- tf_idf <- word <- NULL
  if (!is.null(stopwords_vector) && !is.character(stopwords_vector)) {
    cli::cli_abort("{.arg stopwords_vector} must be a character vector, not {.obj_type_friendly {stopwords_vector}}.")
  }
  if (length(clean_word_method) > 1) {
    clean_word_method <- "none"
    cli::cli_inform(c("i" = "No {.arg clean_word_method} given: words are neither lemmatised nor stemmed."))
  }
  clean_word_method <- rlang::arg_match(clean_word_method, c("lemmatize", "stemming", "none"))
  if (clean_word_method != "none") {
    rlang::check_installed("textstem", reason = "to lemmatise or stem words.")
  }

  # One table of nodes, whatever the input. as.data.table() on a data frame or
  # a node table returns a copy: the user's data are not modified.
  if (inherits(data, "tbl_graph")) {
    dt <- data.table::as.data.table(node_table(data))
  } else if (is.data.frame(data)) {
    dt <- data.table::as.data.table(data)
    if (data.table::is.data.table(data)) dt <- data.table::copy(dt)
  } else if (is.list(data) && all(vapply(data, inherits, logical(1), what = "tbl_graph"))) {
    dt <- data.table::rbindlist(lapply(data, function(graph) data.table::as.data.table(node_table(graph))),
                                idcol = "list_names", fill = TRUE)
    if (grouping_across_list) grouping_columns <- c("list_names", grouping_columns)
  } else {
    cli::cli_abort("{.arg data} must be a {.cls tbl_graph}, a list of {.cls tbl_graph}s or a data frame.")
  }
  check_columns(dt, c(text_columns, grouping_columns), "data")

  # Texts: several columns are pasted together; missing texts are empty.
  texts <- lapply(dt[, .SD, .SDcols = text_columns], function(x) ifelse(is.na(x), "", as.character(x)))
  dt[, text := do.call(paste, c(texts, sep = ". "))]
  dt <- dt[, .SD, .SDcols = c(grouping_columns, "text")]

  if (is.null(stopwords_vector)) {
    stopwords_vector <- tidytext::get_stopwords(source = stopwords_type)$word
  }

  # N-grams of 1 to n_gram words, one column per word position.
  columns <- paste0("word_", seq_len(n_gram))
  tokens <- data.table::as.data.table(
    tidytext::unnest_tokens(dt, word, text, token = "ngrams", n_min = 1, n = n_gram)
  )
  tokens <- data.table::copy(tokens[!is.na(word)])
  split_words <- data.table::tstrsplit(tokens$word, " ", fixed = TRUE)
  split_words <- c(split_words, rep(list(rep(NA_character_, nrow(tokens))), n_gram - length(split_words)))
  tokens[, (columns) := split_words[seq_len(n_gram)]]
  for (column in columns) {
    data.table::set(tokens, i = which(is.na(tokens[[column]])), j = column, value = "")
  }

  # Lemmatising or stemming once per distinct word, then mapping back.
  if (clean_word_method != "none") {
    clean <- switch(clean_word_method, lemmatize = textstem::lemmatize_words, stemming = textstem::stem_words)
    words <- unique(unlist(tokens[, .SD, .SDcols = columns], use.names = FALSE))
    cleaned <- clean(words)
    for (column in columns) {
      data.table::set(tokens, j = column, value = cleaned[match(tokens[[column]], words)])
    }
  }

  # Drop n-grams containing a stop word or a number.
  keep <- rep(TRUE, nrow(tokens))
  for (column in columns) {
    keep <- keep & !tokens[[column]] %in% stopwords_vector & !grepl("^\\d+$", tokens[[column]])
  }
  tokens <- tokens[keep]
  tokens[, term := trimws(do.call(paste, c(.SD, sep = " "))), .SDcols = columns]
  tokens <- tokens[, .SD, .SDcols = c(grouping_columns, "term")]

  # Terms rarer than ngrams_filter in the corpus are dropped, then tf-idf
  # with the groups as documents.
  tokens[, total_term := .N, by = term]
  tokens <- tokens[total_term >= ngrams_filter]
  tokens[, document := do.call(paste, c(.SD, sep = "_")), .SDcols = grouping_columns]
  counts <- tokens[, .(n = .N), by = c("document", grouping_columns, "term")]

  counts %>%
    tidytext::bind_tf_idf(term, document, n) %>%
    dplyr::filter(n > 1) %>%
    dplyr::group_by(document) %>%
    dplyr::slice_max(order_by = tf_idf, n = nb_terms, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-document) %>%
    data.table::as.data.table()
}
