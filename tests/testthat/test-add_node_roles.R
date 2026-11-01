test_that("participation coefficients and z-scores follow Guimera and Amaral", {
  graph <- tidygraph::mutate(toy_graph(), cluster = c("A", "A", "A", "B", "B", "B"))
  roles <- node_table(add_node_roles(graph, module_col = "cluster"))

  # c: weight 2 towards A, 0.1 towards B.
  k_c <- 2.1
  expect_equal(roles$participation_coeff[roles$id == "c"], 1 - (2 / k_c)^2 - (0.1 / k_c)^2)
  expect_equal(roles$participation_coeff[roles$id == "a"], 0)
  # All members of A have the same within-module degree (2): z = 0.
  expect_equal(roles$within_module_degree[roles$id %in% c("a", "b", "c")], c(2, 2, 2))
  expect_equal(roles$within_module_z, rep(0, 6))
  expect_equal(roles$role_ga[roles$id == "a"], "R1 ultra-peripheral")
})

test_that("a node strongly tied to its cluster is a hub", {
  # A star of 8 leaves around h, the leaves being also tied in a ring.
  leaves <- paste0("l", 1:8)
  edges <- rbind(
    data.frame(from = "h", to = leaves),
    data.frame(from = leaves, to = c(leaves[-1], leaves[1]))
  )
  graph <- tidygraph::tbl_graph(nodes = data.frame(id = c("h", leaves), cluster = "A"),
                                edges = transform(edges, weight = 1), directed = FALSE)
  roles <- node_table(add_node_roles(graph, module_col = "cluster", z_threshold = 2.5))
  expect_equal(roles$role_ga[roles$id == "h"], "R5 provincial hub")
  expect_gt(roles$within_module_z[roles$id == "h"], 2.5)
})

test_that("lists are supported and missing columns are reported", {
  graph <- tidygraph::mutate(toy_graph(), cluster = c("A", "A", "A", "B", "B", "B"))
  result <- add_node_roles(list(w1 = graph, w2 = graph), module_col = "cluster")
  expect_named(result, c("w1", "w2"))
  expect_error(add_node_roles(graph, module_col = "nope"), "nope")
  expect_error(add_node_roles(graph, module_col = "cluster", weight_col = "nope"), "nope")
})
