test_that("identical windows give identical dynamic clusters", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "01", d = "02", e = "02", f = "02"),
    c(a = "02", b = "02", c = "02", d = "01", e = "01", f = "01")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete")
  first <- node_table(merged[[1]])
  second <- node_table(merged[[2]])
  expect_equal(second$dynamic_cluster[match(first$id, second$id)], first$dynamic_cluster)
  expect_length(unique(c(first$dynamic_cluster, second$dynamic_cluster)), 2)
})

test_that("a cluster that loses its majority gets a new identifier", {
  # Cluster 01 {a, b, c, d}: in window 2, only a and b stay together.
  windows <- toy_windows(
    c(a = "01", b = "01", c = "01", d = "01"),
    c(a = "01", b = "01", c = "02", d = "03")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete")
  first <- node_table(merged[[1]])
  second <- node_table(merged[[2]])
  # 2/4 nodes of the old cluster go to the new 01: not more than 0.5.
  expect_false(any(second$dynamic_cluster %in% first$dynamic_cluster))
  expect_length(unique(second$dynamic_cluster), 3)
})

test_that("partial similarity ignores newcomers, complete does not", {
  # Cluster {a, b} persists in window 2 but is joined by three new nodes.
  windows <- toy_windows(
    c(a = "01", b = "01"),
    c(a = "01", b = "01", x = "01", y = "01", z = "01")
  )
  complete <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete")
  partial <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial")
  same_name <- function(merged) {
    unique(node_table(merged[[1]])$dynamic_cluster) == unique(node_table(merged[[2]])$dynamic_cluster)
  }
  # complete: 2 of the 5 members of the new cluster come from the old one.
  expect_false(same_name(complete))
  expect_true(same_name(partial))
})

test_that("dynamic clusters are added to the edges", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "02", d = "02"),
    c(a = "01", b = "01", c = "02", d = "02")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial")
  edges <- tidygraph::as_tibble(merged[[1]], active = "edges")
  expect_true(all(c("dynamic_cluster_from", "dynamic_cluster_to") %in% names(edges)))
  expect_equal(tidygraph::active(merged[[1]]), "nodes")
})

test_that("the function can be run again, and works on unnamed lists", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "02", d = "02"),
    c(a = "01", b = "01", c = "02", d = "02")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial")
  expect_message(
    again <- merge_dynamic_clusters(merged, "cluster", "id", similarity_type = "complete"),
    "replaced"
  )
  expect_equal(sum(names(node_table(again[[1]])) == "dynamic_cluster"), 1)
  expect_null(names(again))
})

test_that("windows without shared nodes trigger a warning", {
  windows <- toy_windows(c(a = "01", b = "01"), c(c = "01", d = "01"))
  expect_warning(
    merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial"),
    "share no node"
  )
})

test_that("arguments are checked", {
  windows <- toy_windows(c(a = "01", b = "01"), c(a = "01", b = "01"))
  expect_snapshot(error = TRUE, merge_dynamic_clusters(windows, "cluster", "id"))
  expect_error(
    merge_dynamic_clusters(windows, "cluster", "id", threshold_similarity = 0.5, similarity_type = "partial"),
    "0.5"
  )
  expect_error(
    merge_dynamic_clusters(windows[1], "cluster", "id", similarity_type = "partial"),
    "at least two"
  )
  expect_error(
    merge_dynamic_clusters(windows, "no_column", "id", similarity_type = "partial"),
    "no_column"
  )
})

test_that("on the stagflation data, every node gets a dynamic cluster", {
  networks <- build_dynamic_networks(
    subset(Nodes_stagflation, source_type == "Stagflation"), Ref_stagflation,
    "source_id", "target_id", time_variable = "source_year", time_window = 10,
    window_step = 1, cooccurrence_method = "coupling_angle",
    edges_threshold = 2, verbose = FALSE
  )[1:5]
  networks <- add_clusters(networks, clustering_method = "leiden",
                           objective_function = "modularity", seed = 1, verbose = FALSE)
  merged <- merge_dynamic_clusters(networks, "cluster_leiden", "source_id", similarity_type = "partial")
  all_nodes <- dplyr::bind_rows(lapply(merged, node_table))
  expect_false(anyNA(all_nodes$dynamic_cluster_leiden))
  # Fewer dynamic clusters than window clusters: some continuities are found.
  n_window <- sum(vapply(merged, function(g) length(unique(node_table(g)$cluster_leiden)), integer(1)))
  expect_lt(length(unique(all_nodes$dynamic_cluster_leiden)), n_window)
})
