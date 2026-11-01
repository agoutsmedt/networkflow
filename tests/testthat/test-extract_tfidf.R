toy_texts <- function() {
  data.frame(
    group = c("A", "A", "B", "B"),
    title = c("oil oil shock", "oil prices", "money rules", "money rules 1979")
  )
}

test_that("tf-idf values follow the formula", {
  terms <- extract_tfidf(toy_texts(), text_columns = "title", grouping_columns = "group",
                         n_gram = 1, ngrams_filter = 1, clean_word_method = "none", nb_terms = 5)
  # Group A has 5 words (oil x3, shock, prices); "oil" appears in 1 of 2 groups.
  oil <- terms[term == "oil"]
  expect_equal(oil$n, 3)
  expect_equal(oil$tf, 3 / 5)
  expect_equal(oil$idf, log(2 / 1))
  expect_equal(oil$tf_idf, 3 / 5 * log(2))
  # Terms appearing once in their group are dropped; numbers are dropped.
  expect_false(any(c("shock", "prices", "1979") %in% terms$term))
  expect_setequal(terms[group == "B"]$term, c("money", "rules"))
})

test_that("n-grams with a stop word are dropped, others are kept", {
  data <- data.frame(group = c("A", "A", "B"),
                     title = c("the oil shock", "the oil shock", "money"))
  terms <- extract_tfidf(data, "title", "group", n_gram = 2, ngrams_filter = 1,
                         clean_word_method = "none")
  expect_true("oil shock" %in% terms$term)
  expect_false(any(grepl("\\bthe\\b", terms$term)))
})

test_that("nb_terms limits the terms per group and missing texts are ignored", {
  data <- rbind(toy_texts(), data.frame(group = "B", title = NA))
  terms <- extract_tfidf(data, "title", "group", n_gram = 1, ngrams_filter = 1,
                         clean_word_method = "none", nb_terms = 1)
  expect_equal(nrow(terms[group == "B"]), 1)
  expect_false("na" %in% terms$term)
})

test_that("several text columns and graph inputs are accepted", {
  data <- transform(toy_texts(), subtitle = c("crisis", "crisis", "policy", "policy"))
  terms <- extract_tfidf(data, c("title", "subtitle"), "group", n_gram = 1,
                         ngrams_filter = 1, clean_word_method = "none")
  expect_true(all(c("crisis", "policy") %in% terms$term))

  graph <- add_clusters(toy_graph(), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  from_graph <- extract_tfidf(graph, "title", "cluster_leiden", n_gram = 1,
                              ngrams_filter = 1, clean_word_method = "none")
  expect_true(all(c("inflation", "oil") %in% from_graph$term))
  from_list <- extract_tfidf(list(w1 = graph, w2 = graph), "title", "cluster_leiden",
                             grouping_across_list = TRUE, n_gram = 1, ngrams_filter = 1,
                             clean_word_method = "none")
  expect_setequal(unique(from_list$list_names), c("w1", "w2"))
})

test_that("the input is not modified and no warning is raised", {
  data <- data.table::as.data.table(toy_texts())
  before <- data.table::copy(data)
  expect_no_warning(
    extract_tfidf(data, "title", "group", n_gram = 2, ngrams_filter = 1, clean_word_method = "none")
  )
  expect_identical(data, before)
})

test_that("lemmatisation merges inflected forms", {
  skip_if_not_installed("textstem")
  data <- data.frame(group = c("A", "A", "B", "B"),
                     title = c("prices rising", "price rose", "money", "money"))
  terms <- extract_tfidf(data, "title", "group", n_gram = 1, ngrams_filter = 1,
                         clean_word_method = "lemmatize")
  expect_true("price" %in% terms$term)
})

test_that("arguments are checked", {
  expect_error(extract_tfidf(toy_texts(), "title", "group", stopwords_vector = 1), "character")
  expect_error(extract_tfidf(1:3, "title", "group"), "data frame")
  expect_error(extract_tfidf(toy_texts(), "nope", "group", clean_word_method = "none"), "nope")
})

test_that("nb_terms is the maximum number of terms of each group", {
  graph <- add_clusters(stagflation_network(1), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  for (k in c(1, 3, 6)) {
    terms <- extract_tfidf(graph, "source_title", "cluster_leiden", nb_terms = k,
                           clean_word_method = "none")
    expect_lte(max(table(terms$cluster_leiden)), k)
    expect_equal(max(table(terms$cluster_leiden)), k)
  }
})
