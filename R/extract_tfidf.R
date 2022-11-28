#' Extracting TF-IDF Values for Ngrams
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function takes as input a tibble graph (from [tidygraph](https://tidygraph.data-imaginist.com/)),
#' a list of tibble graphs or a data frame, extract the ngrams from the text column(s) of
#' your choice, and calculates the Term-Frequency Inverse-Document-Frequency value of each
#' ngram for each grouping variables you have chosen.
#'
#' @param data
#' A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/), a list of tibble
#' graphs or a data frame.
#'
#' @param text_columns
#' The columns with the text you want to analyze. If you give multiple columns, they will
#' be united to extract the terms.
#'
#' @param grouping_columns
#' The column(s) you want to use to calculate the tf-idf. These columns will become your
#' "document" unit in the [bind_tf_idf()][tidytext::bind_tf_idf()] function. For instance,
#' if you run the function on a unique tibble graph, you may want to compute the tf-idf
#' depending on the clusters your nodes are belonging. You have to take care that the
#' identifier of the variable you are using to compute the tf-idf is unique for each group
#' (see the details for more information).
#'
#' @param grouping_across_list Set to `TRUE` if you want to compute tf-idf on the
#' whole list of tibble graphs and that you have no unique identifier for them (see
#' the details for more information).
#'
#' @param n_gram
#' The maximum n you want for tokenizing your ngrams (see [unnest_tokens()][tidytext::unnest_tokens()]
#' for more information). 2 by default, i.e. only unigrams and bigrams will be extracted.
#'
#' @param stopwords_type
#' The type of stopwords list you want to use to remove stopwords from your ngrams.
#' The "smart" list is chosen by default, but see other possilities with
#' [stopwords_getsources][stopwords::stopwords_getsources()].
#'
#' @param stopwords_vector
#' Use your own stopwords list, in a vector of strings format.
#'
#' @param clean_word_method
#' Choose the method to clean and standardized your ngrams. You can lemmatize or stem words through
#' the [textstem package](https://github.com/trinker/textstem).
#' Choose "none" if you don't want to apply any cleaning method.
#'
#' @param ngrams_filter
#' You can exclude from tf-idf computation the ngrams that does not appear a certain
#' number of time in the whole corpus.
#'
#' @param nb_terms
#' The functions extracts the `nb_terms` (5 by default) highest TF-IDF for each grouping
#' variables.
#'
#' @returns
#' A data.table with the terms (i.e. ngrams) appearing in each "document" (that is your
#' `grouping_columns`) with the number of time they appear per document (`n`), their
#' term frequency (`tf`), their inverse document frequency (`idf`), and their term-frequency inverse-document-frequency
#' (`tf_idf`). The terms are those with the highest `tf_idf` value for each value of the
#' grouping columns, depending on the `nb_words` value you set. For instance, if `nb_words`
#' is set to 5 (default valuet), and that you compute the TF-IDF on the cluster variable,
#' the function extracts the 5 terms with the highest TF-IDF value for each cluster.
#'
#' @details
#' This functions extract TF-IDF values for various types of input, from multiple text
#' columns and with grouping of multiple columns. The most simple case is to use this
#' function with a data frame or a unique tibble graph with an easily identifiable
#' grouping variable (like a cluster). But it also allows more complex uses in the case
#' of a list of tibble graphs.
#'
#' @details If you enter as an input a list of tibble graphs, the function extracts TF-IDF on the
#' binded graphs, and not graph after graph. If your want to extract TF-IDF for each
#' graphs separately, then use `lapply()` and apply `extract_tfidf()` for each graph: the
#' input will be a unique tibble graph, and the operation will be repeated for each tibble
#' graphs of your list.
#'
#' @details As the extraction of TF-IDF is made on the whole aggregated list, you have
#' to choose carefully your `grouping_columns`. Indeed, your grouping columns must
#' identify variables that are unique. For instance, in the case you have used
#' `add_clusters()`, each node in each of your graph is associated to a cluster. But the
#' identifier of the clusters ("01", "02", "03", etc.) are the same across tibble graphs.
#' It means that all the "01" clusters will be grouped together, and it is something
#' you don't want. In this case, set `grouping_across_list` to `TRUE`: the identifier
#' of the cluster will be merged with the name of the corresponding tibble_graph in
#' the list. However, you don't need to use this possibility if you have a unique
#' identifier across your tibble graphs. That is the case, for instance, if you have
#' use `merge_dynamic_clusters()`, you have a column of clusters merged across
#' your different tibble graphs. These new inter-networks clusters constitute a unique
#' identifier.
#'
#' @details TF-IDF are calculated from the number of occurrence of a term in each document.
#' The terms which occur only once are removed to avoid too rare terms to appear at
#' the top of your grouping variables.
#'
#' @examples
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
#' tfidf <- extract_tfidf(temporal_networks,
#' n_gram = 4,
#' text_columns = "Title",
#' grouping_columns = "cluster_leiden",
#' grouping_across_list = TRUE,
#' clean_word_method = "lemmatise")
#'
#' tfidf[[1]]
#'
#' @export
#' @import data.table
#' @import tidygraph
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tidytext
#' @import textstem
extract_tfidf <- function(data,
                          text_columns,
                          grouping_columns,
                          grouping_across_list = FALSE,
                          n_gram = 2L,
                          stopwords_type = "smart",
                          stopwords_vector = NULL,
                          clean_word_method = c("lemmatize", "stemming", "none"),
                          ngrams_filter = 5L,
                          nb_terms = 5L){
  row_id <- text <- . <- term <- total_term <- document <- tf_idf <- NULL

  if(! is.null(stopwords_vector) & ! is.character(stopwords_vector)){
    stop("The stopwords list is not a vector of strings.")
  }
  if(length(clean_word_method) > 1){
    clean_word_method <- "none"
    cli::cli_alert_info("No cleaning word method has been chosen. The ngrams won't be stemmed or lemmatized.")
  }
  if(inherits(data, "tbl_graph")){ # in case we have only one network
    dt <- data %N>%
      data.table::as.data.table()
  } else if (inherits(data, "list")){ # for a list of network
    dt <- lapply(data, function(tbl) tbl %N>%
                   data.table::as.data.table()) %>%
      data.table::rbindlist(idcol = "list_names")
    if(grouping_across_list == TRUE) grouping_columns <- c("list_names", grouping_columns) # Double paste0 in case grouping_columns already gather multiple columns
  } else if (inherits(data, "data.frame")){ # the third case is the one for which we have already a data.frame
    dt <- data.table::as.data.table(data)
  } else {
    cli::cli_abort(c("The data object you enter in the function is neither",
                     "*" = "a tibble graph (a {.emph tbl_graph} from {.pkg tidygraph});",
                     "*" = "a list of tibble graphs;",
                     "*" = "a data frame."))
  }

  if(length(text_columns) > 1){
    dt[, row_id := 1:.N]
    dt[, text := paste0(.SD, collapse = ". "), by = row_id, .SDcols = text_columns][, -"row_id"]
  } else {
    dt[, text := .SD, .SDcols = text_columns]
  }

  if(is.null(stopwords_vector)){
    stopwords_vector <- tidytext::get_stopwords(source = stopwords_type)$word
  }
  columns <- paste0("word_", 1:n_gram)

  term_list <- dt %>%
    tidytext::unnest_tokens(word, text, token = "ngrams", n_min = 1, n = n_gram) %>%
    .[, (columns) := data.table::tstrsplit(word, " ")] %>% # splitting ngrams in one column per word
    .[, (columns) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = columns] %>%  # replacing na value
    {if(clean_word_method == "lemmatize") .[, (columns) := lapply(.SD, textstem::lemmatize_words), .SDcols = columns] else .} %>%
    {if(clean_word_method == "stemming") .[, (columns) := lapply(.SD, textstem::stem_words), .SDcols = columns] else .} %>%
    dplyr::filter(dplyr::if_all(starts_with("word_"), ~ ! . %in% stopwords_vector),
                  dplyr::if_all(starts_with("word_"), ~ ! stringr::str_detect(., "^[:digit:]+$"))) %>%
    tidyr::unite(term, starts_with("word_"), sep = " ") %>%
    .[, term := stringr::str_trim(term, "both")] %>%
    dplyr::select(dplyr::all_of(grouping_columns), term) %>%
    .[, total_term := .N, by = term] %>%
    dplyr::filter(total_term >= ngrams_filter) %>%
    tidyr::unite(document, all_of(grouping_columns), sep = "_", remove = FALSE) %>%
    .[, n := .N, by = .(term, document)] %>%
    dplyr::select(document, all_of(grouping_columns), term, n) %>%
    unique %>%
    tidytext::bind_tf_idf(term, document, n) %>%
    filter(n > 1) %>%
    dplyr::group_by(document) %>%
    dplyr::slice_max(order_by = tf_idf, n = nb_terms, with_ties = FALSE) %>%
    data.table::as.data.table()
}
