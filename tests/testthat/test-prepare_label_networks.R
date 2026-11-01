test_that("labels are placed at the centre of gravity of each cluster", {
  graph <- tidygraph::mutate(toy_graph(),
                             cluster = c("A", "A", "A", "B", "B", "B"),
                             x = c(0, 1, 2, 10, 11, 12), y = c(0, 0, 3, 5, 5, 5))
  labelled <- node_table(prepare_label_networks(graph, cluster_label_column = "cluster"))
  expect_equal(unique(labelled$label_x[labelled$cluster == "A"]), 1)
  expect_equal(unique(labelled$label_y[labelled$cluster == "A"]), 1)
  expect_equal(unique(labelled$label_x[labelled$cluster == "B"]), 11)
})

test_that("custom coordinate columns are used, for one graph or a list", {
  graph <- tidygraph::mutate(toy_graph(), cluster = "A", x = 0, y = 0, x_fr = 1:6, y_fr = 2)
  one <- node_table(prepare_label_networks(graph, x = "x_fr", y = "y_fr", cluster_label_column = "cluster"))
  expect_equal(unique(one$label_x), 3.5)
  several <- prepare_label_networks(list(graph, graph), x = "x_fr", y = "y_fr", cluster_label_column = "cluster")
  expect_equal(unique(node_table(several[[2]])$label_x), 3.5)
  expect_error(prepare_label_networks(graph, x = "nope", cluster_label_column = "cluster"), "nope")
})

test_that("prepare_label_alluvial puts one label per cluster, in its median window", {
  alluv <- data.table::data.table(
    cluster_label = rep(c("A", "B"), c(3, 1)),
    window = c("1", "2", "3", "1"),
    id = c("n1", "n1", "n1", "n2")
  )
  labelled <- prepare_label_alluvial(alluv)
  expect_equal(labelled[!is.na(label_x), window], c("2", "1"))
})
