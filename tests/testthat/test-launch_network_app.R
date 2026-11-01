# The server is never started: the pieces of the app are tested directly, and
# the server logic with shiny::testServer().

app_toy_settings <- function(graph = NULL, ...) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinycssloaders")
  graph <- graph %||% add_clusters(toy_graph(), clustering_method = "leiden",
                                   objective_function = "modularity", seed = 1, verbose = FALSE)
  suppressMessages(app_settings(
    graph, cluster_id = "cluster_leiden", cluster_information = c("id", "title"),
    cluster_tooltip = NULL, node_id = "id", node_tooltip = NULL, node_size = NULL,
    color = NULL, layout = "fr", ...
  ))
}

test_that("settings add layout, colours and constant sizes", {
  settings <- app_toy_settings()
  expect_true(settings$single)
  nodes <- node_table(settings$graphs[[1]])
  expect_true(all(c("x", "y", "color", "size") %in% names(nodes)))
  expect_equal(unique(nodes$size), 1)
})

test_that("the arguments are checked", {
  skip_if_not_installed("shiny")
  expect_error(
    launch_network_app(data.frame(x = 1), cluster_id = "c", cluster_information = "x", node_id = "id"),
    "tbl_graph"
  )
  expect_error(
    suppressMessages(app_settings(toy_graph(), "id", "title", NULL, "id", NULL, NULL, NULL, NULL)),
    "layout coordinates"
  )
  expect_error(
    suppressMessages(app_settings(toy_graph(), "id", "nope", NULL, "id", NULL, NULL, NULL, "fr")),
    "nope"
  )
  expect_error(
    suppressMessages(app_settings(toy_graph(), "id", character(0), NULL, "id", NULL, NULL, NULL, "fr")),
    "cluster_information"
  )
})

test_that("the nodes of a selected cluster are filtered", {
  nodes <- data.frame(cluster = c("01", "01", "02"), id = c("a", "b", "c"))
  expect_equal(app_cluster_nodes(nodes, "cluster", "02")$id, "c")
  expect_equal(nrow(app_cluster_nodes(nodes, "cluster", NULL)), 3)
  expect_equal(nrow(app_cluster_nodes(nodes, "cluster", character(0))), 3)
})

test_that("the plot is a ggplot with one label per cluster", {
  settings <- app_toy_settings()
  plot <- app_plot(settings$graphs[[1]], settings, show_edges = TRUE)
  expect_s3_class(plot, "ggplot")
  labels <- plot$layers[[2]]$data
  expect_equal(nrow(labels), 2)
})

test_that("the server selects a window and the nodes of a cluster", {
  graphs <- list(w1 = toy_graph(), w2 = toy_graph(with_pair = TRUE))
  graphs <- add_clusters(graphs, clustering_method = "leiden",
                         objective_function = "modularity", seed = 1, verbose = FALSE)
  settings <- app_toy_settings(graphs)
  expect_false(settings$single)
  # ggiraph warns "Killing locked device" when its renderer runs inside testServer.
  suppressWarnings(shiny::testServer(app_server(settings), {
    session$setInputs(time_window = "w1", network_plot_selected = NULL)
    expect_equal(nrow(nodes_shown()), 6)
    session$setInputs(time_window = "w2")
    expect_equal(nrow(nodes_shown()), 8)
    first_cluster <- node_table(graph_selected())$cluster_leiden[1]
    session$setInputs(network_plot_selected = first_cluster)
    expect_equal(nrow(nodes_shown()), sum(node_table(graph_selected())$cluster_leiden == first_cluster))
  }))
})
