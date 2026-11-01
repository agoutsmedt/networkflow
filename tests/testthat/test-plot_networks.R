ready_toy <- function() {
  graph <- add_clusters(toy_graph(), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  graph <- withr::with_seed(1, layout_networks(graph, node_id = "id", layout = "fr"))
  color_networks(graph, "cluster_leiden", verbose = FALSE)
}

test_that("a single graph gives a ggplot with one label per cluster", {
  plot <- plot_networks(ready_toy(), cluster_label_column = "cluster_leiden", node_size_column = NULL)
  expect_s3_class(plot, "ggplot")
  built <- ggplot2::ggplot_build(plot)
  label_layer <- vapply(plot$layers, function(l) inherits(l$geom, "GeomLabelRepel"), logical(1))
  expect_equal(nrow(built$data[[which(label_layer)]]), 2)
})

test_that("a list of graphs gives a named list of plots", {
  graphs <- list(w1 = ready_toy(), w2 = ready_toy())
  result <- plot_networks(graphs, cluster_label_column = "cluster_leiden", node_size_column = NULL)
  expect_named(result, c("w1", "w2"))
  expect_s3_class(result$w1, "ggplot")
  expect_equal(result$w2$labels$title, "w2")
})

test_that("colours can be computed on the fly and code printed", {
  graph <- withr::with_seed(1, layout_networks(
    add_clusters(toy_graph(), clustering_method = "louvain", verbose = FALSE),
    node_id = "id", layout = "fr"
  ))
  expect_error(plot_networks(graph, cluster_label_column = "cluster_louvain"), "color_networks")
  expect_output(
    plot_networks(graph, cluster_label_column = "cluster_louvain", node_size_column = NULL,
                  color_networks = TRUE, print_plot_code = TRUE),
    NA
  )
  expect_message(
    plot_networks(graph, cluster_label_column = "cluster_louvain", node_size_column = NULL,
                  color_networks = TRUE, print_plot_code = TRUE),
    "ggraph"
  )
})

test_that("missing columns are reported", {
  expect_error(plot_networks(ready_toy(), x = "nope", cluster_label_column = "cluster_leiden"), "nope")
  expect_error(plot_networks(ready_toy(), cluster_label_column = NULL), "cluster_label_column")
})
