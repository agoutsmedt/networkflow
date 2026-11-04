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

test_that("forceatlas2 and umap give one pair of coordinates per node", {
  skip_if_not_installed("uwot")
  graph <- toy_graph(with_pair = TRUE)
  for (layout in c("forceatlas2", "umap")) {
    withr::local_seed(1)
    laid_out <- suppressMessages(layout_networks(graph, node_id = "id", layout = layout))
    nodes <- node_table(laid_out)
    expect_true(all(c("x", "y") %in% names(nodes)), info = layout)
    expect_equal(nrow(nodes), 8)
    expect_false(anyNA(nodes$x), info = layout)
  }
})

test_that("forceatlas2 draws the nodes of a triangle closer than the nodes of two triangles", {
  withr::local_seed(2)
  nodes <- node_table(layout_networks(toy_graph(), node_id = "id", layout = "forceatlas2"))
  position <- as.matrix(nodes[c("x", "y")])
  d <- as.matrix(dist(position))
  inside <- mean(d[1:3, 1:3][upper.tri(d[1:3, 1:3])])
  between <- mean(d[1:3, 4:6])
  expect_lt(inside, between)
})

test_that("umap keeps the components apart and the layouts take starting coordinates", {
  skip_if_not_installed("uwot")
  graphs <- list(w1 = toy_graph(), w2 = toy_graph(with_pair = TRUE))
  withr::local_seed(3)
  laid_out <- suppressMessages(
    layout_networks(graphs, node_id = "id", layout = "forceatlas2", compute_dynamic_coordinates = TRUE,
                    iterations = 50)
  )
  expect_equal(nrow(node_table(laid_out[[2]])), 8)
  expect_false(anyNA(node_table(laid_out[[2]])$x))
  expect_error(layout_networks(toy_graph(), node_id = "id", layout = "umap", weights = c(1, 2)), "weights")
})

test_that("the forces along the edges are summed at the right nodes", {
  expect_equal(tabulate_edges(c(3, 1, 3), c(1, 2, 5), 4), c(2, 0, 6, 0))
})

test_that("the linlog mode of forceatlas2 also separates the triangles", {
  withr::local_seed(2)
  nodes <- node_table(layout_networks(toy_graph(), node_id = "id", layout = "forceatlas2", linlog = TRUE))
  d <- as.matrix(dist(as.matrix(nodes[c("x", "y")])))
  expect_lt(mean(d[1:3, 1:3][upper.tri(d[1:3, 1:3])]), mean(d[1:3, 4:6]))
})

test_that("an unknown layout gives a clear error", {
  expect_error(layout_networks(toy_graph(), node_id = "id", layout = "nope"), "not a known layout")
})
