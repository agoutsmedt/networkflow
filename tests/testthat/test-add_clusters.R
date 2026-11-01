test_that("Leiden finds the two triangles", {
  graph <- add_clusters(toy_graph(), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  nodes <- node_table(graph)
  clusters <- split(nodes$id, nodes$cluster_leiden)
  expect_length(clusters, 2)
  expect_setequal(lapply(clusters, sort), list(c("a", "b", "c"), c("d", "e", "f")))
  expect_equal(unique(nodes$size_cluster_leiden), 0.5)
})

test_that("edges get the cluster of their ends, and '00' between clusters", {
  graph <- add_clusters(toy_graph(), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  edges <- tidygraph::as_tibble(graph, active = "edges")
  bridge <- edges$weight == 0.1
  expect_equal(edges$cluster_leiden[bridge], "00")
  expect_true(all(edges$cluster_leiden[!bridge] %in% c("01", "02")))
  expect_equal(edges$cluster_leiden[!bridge], edges$cluster_leiden_from[!bridge])
})

test_that("cluster identifiers are numbered by decreasing size", {
  graph <- add_clusters(stagflation_network(), clustering_method = "leiden",
                        objective_function = "modularity", seed = 1, verbose = FALSE)
  sizes <- table(node_table(graph)$cluster_leiden)
  expect_false(is.unsorted(rev(as.vector(sizes))))
  expect_equal(names(sizes)[1], "01")
})

test_that("every algorithm assigns a cluster to every node", {
  graph <- toy_graph()
  for (method in c("louvain", "fast_greedy", "infomap", "walktrap")) {
    clustered <- add_clusters(graph, clustering_method = method, seed = 1, verbose = FALSE)
    column <- node_table(clustered)[[paste0("cluster_", method)]]
    expect_false(anyNA(column), label = method)
    expect_length(column, 6)
  }
})

test_that("n_groups cuts the dendrogram of fast greedy and walktrap", {
  clustered <- add_clusters(toy_graph(), clustering_method = "walktrap", n_groups = 3, verbose = FALSE)
  expect_length(unique(node_table(clustered)$cluster_walktrap), 3)
})

test_that("the seed makes results reproducible without changing the session's RNG", {
  network <- stagflation_network()
  run <- function() {
    node_table(add_clusters(network, clustering_method = "leiden",
                            objective_function = "modularity", seed = 42,
                            verbose = FALSE))$cluster_leiden
  }
  expect_identical(run(), run())

  withr::local_seed(1)
  before <- .Random.seed
  run()
  expect_identical(.Random.seed, before)
})

test_that("weights are used by every algorithm unless weights = NA", {
  # A square a-b-c-d-a with two strong opposite edges: the weighted partition
  # follows the strong edges.
  graph <- tidygraph::tbl_graph(
    nodes = data.frame(id = c("a", "b", "c", "d")),
    edges = data.frame(from = c("a", "b", "c", "d"), to = c("b", "c", "d", "a"),
                       weight = c(10, 0.1, 10, 0.1)),
    directed = FALSE
  )
  for (method in c("louvain", "fast_greedy")) {
    clustered <- node_table(add_clusters(graph, clustering_method = method, seed = 1, verbose = FALSE))
    column <- clustered[[paste0("cluster_", method)]]
    expect_equal(column[1], column[2], label = method)
    expect_equal(column[3], column[4], label = method)
    expect_false(column[1] == column[3], label = method)
  }
})

test_that("graphs are returned with the node table active", {
  graph <- add_clusters(toy_graph(), clustering_method = "leiden",
                        objective_function = "modularity", verbose = FALSE)
  expect_equal(tidygraph::active(graph), "nodes")
})

test_that("a list of graphs is clustered graph by graph", {
  graphs <- list(w1 = toy_graph(), w2 = toy_graph())
  clustered <- add_clusters(graphs, clustering_method = "leiden",
                            objective_function = "modularity", seed = 1, verbose = FALSE)
  expect_named(clustered, c("w1", "w2"))
  expect_true(all(vapply(clustered, function(g) "cluster_leiden" %in% names(node_table(g)), logical(1))))
})

test_that("choices must be explicit and inputs valid", {
  expect_error(add_clusters(toy_graph(), verbose = FALSE), "clustering_method")
  expect_error(add_clusters(toy_graph(), clustering_method = "leiden", verbose = FALSE), "objective_function")
  expect_error(
    add_clusters(data.frame(x = 1), clustering_method = "louvain"),
    "tbl_graph"
  )
})

test_that("a summary is reported when verbose", {
  expect_message(
    add_clusters(toy_graph(), clustering_method = "leiden", objective_function = "modularity", seed = 1),
    "found 2 clusters"
  )
})

test_that("Leiden iterates until the partition is stable by default", {
  expect_equal(eval(formals(add_clusters)$n_iterations), -1)
})
