test_that("coordinates are added and can be saved under the layout's name", {
  withr::local_seed(1)
  graph <- layout_networks(toy_graph(), node_id = "id", layout = "fr", save_coordinates = TRUE)
  nodes <- node_table(graph)
  expect_true(all(c("x", "y", "x_fr", "y_fr") %in% names(nodes)))
  expect_false(anyNA(nodes$x))
  expect_equal(nodes$x, nodes$x_fr)
})

test_that("the result is returned visibly and coordinates are replaced", {
  withr::local_seed(1)
  graph <- tidygraph::mutate(toy_graph(), x = 0, y = 0)
  result <- withVisible(layout_networks(graph, node_id = "id", layout = "kk"))
  expect_true(result$visible)
  expect_false(all(node_table(result$value)$x == 0))
  expect_equal(sum(names(node_table(result$value)) == "x"), 1)
})

test_that("a seed reproduces the layout", {
  run <- function() {
    withr::with_seed(3, node_table(layout_networks(toy_graph(), node_id = "id", layout = "fr"))$x)
  }
  expect_equal(run(), run())
})

test_that("dynamic coordinates start from the previous window", {
  withr::local_seed(1)
  graphs <- list(w1 = toy_graph(), w2 = toy_graph())
  result <- layout_networks(graphs, node_id = "id", layout = "fr", compute_dynamic_coordinates = TRUE)
  expect_named(result, c("w1", "w2"))
  expect_false(anyNA(node_table(result$w2)$x))
})

test_that("inputs are checked", {
  expect_error(layout_networks(toy_graph(), node_id = "nope", layout = "fr"), "nope")
  expect_error(layout_networks(list(1, 2), node_id = "id", layout = "fr"), "tbl_graph")
})
