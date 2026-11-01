clustered_toy_colors <- function() {
  add_clusters(toy_graph(), clustering_method = "leiden",
               objective_function = "modularity", seed = 1, verbose = FALSE)
}

test_that("each cluster gets one colour, and within-cluster edges its colour", {
  graph <- color_networks(clustered_toy_colors(), "cluster_leiden", verbose = FALSE)
  nodes <- node_table(graph)
  expect_equal(nrow(unique(nodes[c("cluster_leiden", "color")])), 2)
  edges <- tidygraph::as_tibble(graph, active = "edges")
  within <- edges$cluster_leiden != "00"
  from_colors <- nodes$color[edges$from[within]]
  expect_equal(grDevices::col2rgb(edges$color[within]), grDevices::col2rgb(from_colors))
})

test_that("a vector of colours goes to the largest categories first and is recycled", {
  graph <- tidygraph::mutate(toy_graph(), group = c("big", "big", "big", "big", "small", "small"))
  colored <- color_networks(graph, "group", color = c("red", "blue"), verbose = FALSE)
  nodes <- node_table(colored)
  expect_equal(unique(nodes$color[nodes$group == "big"]), "red")
  expect_message(color_networks(graph, "group", color = "red"), "recycled")
})

test_that("a data frame of colours is used and missing categories are reported", {
  graph <- tidygraph::mutate(toy_graph(), group = c("x", "x", "x", "y", "y", "y"))
  palette <- data.frame(group = c("x", "y"), color = c("#000000", "#FFFFFF"))
  colored <- color_networks(graph, "group", color = palette, verbose = FALSE)
  expect_equal(node_table(colored)$color, rep(c("#000000", "#FFFFFF"), each = 3))
  expect_warning(color_networks(graph, "group", color = palette[1, ]), "no colour")
})

test_that("unique_color_across_list separates the same value in two graphs", {
  graphs <- list(w1 = clustered_toy_colors(), w2 = clustered_toy_colors())
  same <- color_networks(graphs, "cluster_leiden", verbose = FALSE)
  expect_equal(node_table(same$w1)$color, node_table(same$w2)$color)
  different <- color_networks(graphs, "cluster_leiden", unique_color_across_list = TRUE, verbose = FALSE)
  expect_false(any(node_table(different$w1)$color %in% node_table(different$w2)$color))
})

test_that("the default palette warns when colours must be recycled", {
  graph <- tidygraph::mutate(toy_graph(), group = letters[1:6])
  expect_no_warning(color_networks(graph, "group", verbose = FALSE))
  big <- tidygraph::play_gnp(20, 0.2) %>% tidygraph::mutate(group = as.character(seq_len(20)))
  expect_warning(color_networks(big, "group", verbose = FALSE), "recycled")
})

test_that("color_alluvial adds one colour per category", {
  alluv <- data.frame(cluster = c("a", "a", "b"), window = c("1", "2", "1"))
  colored <- color_alluvial(alluv, "cluster", color = c("red", "blue"), verbose = FALSE)
  expect_equal(colored$color, c("red", "red", "blue"))
  expect_error(color_alluvial(1, "cluster"), "data frame")
})
