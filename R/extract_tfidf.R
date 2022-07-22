extract_tfidf <- function(data,
                   text_columns,
                   grouping_columns,
                   n_gram = 2L,
                   stopwords = NULL,
                   stopwords_list = "smart",
                   clean_word_method = c("lemmatize", "stemming", "none"),
                   ngrams_filter = 5L){
  #' Extracting TF-IDF Values for Ngrams
  #'
  #' This function takes as input a tidygraph object, a list of tidygraph objects or a data frame,
  #' extract the ngrams use in the character columns of your choice, and calculates the Term-Frequency
  #' Inverse-Document-Frequency value of each ngram for each document.
  #'
  #' @param data
  #' A [tidygraph](https://tidygraph.data-imaginist.com/) object, a list of tidygraph objects or a simple table.
  #'
  #' @param text_columns
  #' The columns with the text you want to analyze. If you give multiple columns, they will
  #' be gathered to extract the terms.
  #'
  #' @param grouping_columns
  #' The columns you want to use to calculate the tf-idf. These columns will become your
  #' "document" unit in the [bind_tf_idf()][tidytext::bind_tf_idf()] function.
  #'
  #' @param n_gram
  #' The maximum n you want for tokenizing your ngrams (see [unnest_tokens()][tidytext::unnest_tokens()]
  #' for more information). 2 by default, i.e. only unigrams and bigrams will be extracted.
  #'
  #' @param stopwords
  #' Use your own stopwords list, in a character vector format
  #'
  #' @param stopwords_list
  #' The stopwords list you want to use to remove stopwords from your ngrams. The "smart" list by
  #' default, but see other possilities with [stopwords_getsources][stopwords::stopwords_getsources()].
  #'
  #' @param clean_word_method
  #' Choose the method to clean and standardized your ngrams. You can lemmatize or stem words through
  #' the [textstem package](https://github.com/trinker/textstem). Choose "none" if you don't want to apply any cleaning method.
  #'
  #' @param ngrams_filter
  #' You can exclude from tf-idf computation the ngrams that does not appear a certain number of time.
  #'
  #' @returns
  #' A data.table with the list of terms (i.e. ngrams) appearing in each "document" (that is your
  #' `grouping_columns`) with the number of time they appear per document (`n`), their
  #' term frequency (`tf`), their inverse document frequency (`idf`), and their term-frequency inverse-document-frequency
  #' (`tf_idf`).
  #'
  #' @export
  #' @import data.table
  #' @import tidygraph
  #' @import dplyr
  #' @import tidyr
  #' @import stringr
  #' @import tidytext
  #' @import textstem

  row_id <- text <- . <- term <- total_term <- document <- NULL

  if(! is.null(stopwords) & ! is.character(stopwords)){
    stop("The stopwords list is not a character vector.")
  }
  if(class(data)[1] == "tbl_graph"){ # in case we have only one network
    dt <- data %N>%
      data.table::as.data.table
  } else if (class(data)[1] == "list"){ # for a list of network
    dt <- lapply(data, function(tbl) tbl %N>%
                   data.table::as.data.table) %>%
      data.table::rbindlist(idcol = "Window")
  } else { # the third case is the one for which we have already a data.frame
    dt <- data.table::as.data.table(data)
  }

  if(length(text_columns) > 1){
    dt[, row_id := 1:.N]
    dt[, text := paste0(.SD, collapse = ". "), by = row_id, .SDcols = text_columns][, -"row_id"]
  } else {
    dt[, text := .SD, .SDcols = text_columns]
  }

  if(is.null(stopwords)){
    stopwords <- tidytext::get_stopwords(source = stopwords_list)$word
  }
  columns <- paste0("word_", 1:n_gram)

  term_list <- dt %>%
    tidytext::unnest_tokens(word, text, token = "ngrams", n_min = 1, n = n_gram) %>%
    .[, (columns) := data.table::tstrsplit(word, " ")] %>% # splitting ngrams in one column per word
    .[, (columns) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = columns] %>%  # replacing na value
    {if(clean_word_method == "lemmatize") .[, (columns) := lapply(.SD, textstem::lemmatize_words), .SDcols = columns] else .} %>%
    {if(clean_word_method == "stemming") .[, (columns) := lapply(.SD, textstem::stem_words), .SDcols = columns] else .} %>%
    dplyr::filter(dplyr::if_all(starts_with("word_"), ~ ! . %in% stopwords ),
                  dplyr::if_all(starts_with("word_"), ~ ! stringr::str_detect(., "^[:digit:]+$"))) %>%
    tidyr::unite(term, starts_with("word_"), sep = " ") %>%
    .[, term := stringr::str_trim(term, "both")] %>%
    dplyr::select(dplyr::all_of(grouping_columns), term) %>%
    .[, total_term := .N, by = term] %>%
    dplyr::filter(total_term >= ngrams_filter) %>%
    tidyr::unite(document, all_of(grouping_columns), sep = "_") %>%
    dplyr::select(document, term) %>%
    unique %>%
    .[, n := .N, by = .(term, document)] %>%
    tidytext::bind_tf_idf(term, document, n)

}
