test_that("the largest component is kept", {
  graph <- toy_graph(with_pair = TRUE)
  # The pair is 2 of 8 nodes (25%): above the 5% alert.
  expect_warning(filtered <- filter_components(graph), "removed component gathers 25%")
  expect_equal(igraph::vcount(filtered), 6)
  expect_false(any(c("g", "h") %in% node_table(filtered)$id))
})

test_that("nb_components keeps several components, without warning", {
  graph <- toy_graph(with_pair = TRUE)
  expect_no_warning(filtered <- filter_components(graph, nb_components = 2))
  expect_equal(igraph::vcount(filtered), 8)
})

test_that("component columns are dropped unless requested", {
  graph <- toy_graph(with_pair = TRUE)
  kept <- suppressWarnings(filter_components(graph, keep_component_columns = TRUE))
  expect_true(all(c("components_att", "size_components") %in% names(node_table(kept))))
  expect_equal(unique(node_table(kept)$size_components), 6 / 8)
  dropped <- suppressWarnings(filter_components(graph))
  expect_false(any(c("components_att", "size_components") %in% names(node_table(dropped))))
})

test_that("a list of graphs is filtered graph by graph, and the result is visible", {
  graphs <- list(w1 = toy_graph(with_pair = TRUE), w2 = toy_graph())
  result <- suppressWarnings(withVisible(filter_components(graphs)))
  expect_true(result$visible)
  expect_named(result$value, c("w1", "w2"))
  expect_equal(vapply(result$value, igraph::vcount, numeric(1)), c(w1 = 6, w2 = 6))
})

test_that("a wrong input gives an informative error", {
  expect_error(filter_components(data.frame(x = 1)), "tbl_graph")
})

test_that("an empty network is returned as it is", {
  empty <- tidygraph::tbl_graph(nodes = data.frame(id = character(0)), edges = data.frame(from = integer(0), to = integer(0)))
  expect_no_error(result <- filter_components(empty))
  expect_equal(igraph::vcount(result), 0)
})
