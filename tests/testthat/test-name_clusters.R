clustered_toy <- function() {
  add_clusters(toy_graph(), clustering_method = "leiden",
               objective_function = "modularity", seed = 1, verbose = FALSE)
}

test_that("given_column labels each cluster with its node with the highest value", {
  graph <- tidygraph::mutate(clustered_toy(), score = c(1, 5, 2, 9, 3, 4))
  named <- name_clusters(graph, method = "given_column", cluster_id = "cluster_leiden",
                         label_columns = "id", order_by = "score")
  nodes <- node_table(named)
  expect_equal(unique(nodes$cluster_label[nodes$id %in% c("a", "b", "c")]), "b")
  expect_equal(unique(nodes$cluster_label[nodes$id %in% c("d", "e", "f")]), "d")
})

test_that("given_column works on dynamic clusters across a list", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "02", d = "02"),
    c(a = "01", b = "01", c = "02", d = "02")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial")
  merged[[1]] <- tidygraph::mutate(merged[[1]], score = c(1, 2, 3, 4))
  merged[[2]] <- tidygraph::mutate(merged[[2]], score = c(9, 0, 0, 5))
  named <- name_clusters(merged, method = "given_column", name_merged_clusters = TRUE,
                         cluster_id = "dynamic_cluster", label_columns = "id", order_by = "score")
  labels <- node_table(named[[1]])$cluster_label
  # a has the highest score of its cluster over both windows (9), d of its own.
  expect_equal(labels, c("a", "a", "d", "d"))
  expect_equal(node_table(named[[2]])$cluster_label, labels)
})

test_that("tidygraph_functions labels clusters with their most central node", {
  named <- name_clusters(clustered_toy(), method = "tidygraph_functions",
                         cluster_id = "cluster_leiden", label_columns = "id",
                         tidygraph_function = tidygraph::centrality_degree())
  nodes <- node_table(named)
  # c and d have the highest degree of their triangle (the bridge).
  expect_setequal(unique(nodes$cluster_label), c("c", "d"))
  expect_true("centrality_degree" %in% names(nodes))
})

test_that("tf-idf labels use the distinctive words of each cluster", {
  named <- name_clusters(clustered_toy(), method = "tf-idf", cluster_id = "cluster_leiden",
                         text_columns = "title", nb_terms_label = 1, ngrams_filter = 1,
                         n_gram = 1, clean_word_method = "none")
  nodes <- node_table(named)
  expect_equal(unique(nodes$cluster_label[nodes$id == "a"]), "inflation")
  expect_equal(unique(nodes$cluster_label[nodes$id == "d"]), "oil")
  edges <- tidygraph::as_tibble(named, active = "edges")
  expect_true("cluster_label" %in% names(edges))
})

test_that("clusters without label get 'no_name'", {
  named <- name_clusters(clustered_toy(), method = "tf-idf", cluster_id = "cluster_leiden",
                         text_columns = "title", ngrams_filter = 100, clean_word_method = "none")
  expect_equal(unique(node_table(named)$cluster_label), "no_name")
})

test_that("arguments are checked", {
  graph <- clustered_toy()
  expect_error(
    name_clusters(graph, method = "given_column", cluster_id = "cluster_leiden", label_columns = "id"),
    "order_by"
  )
  expect_error(
    name_clusters(graph, method = "tf-idf", name_merged_clusters = TRUE, cluster_id = "cluster_leiden",
                  text_columns = "title"),
    "list of graphs"
  )
  expect_error(
    name_clusters(list(graph, graph), method = "tidygraph_functions", name_merged_clusters = TRUE,
                  cluster_id = "cluster_leiden", label_columns = "id",
                  tidygraph_function = tidygraph::centrality_degree()),
    "across windows"
  )
  expect_message(
    name_clusters(graph, cluster_id = "cluster_leiden", text_columns = "title",
                  ngrams_filter = 1, clean_word_method = "none"),
    "tf-idf"
  )
})
