test_that("identical windows give identical dynamic clusters", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "01", d = "02", e = "02", f = "02"),
    c(a = "02", b = "02", c = "02", d = "01", e = "01", f = "01")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete", method = "majority", threshold_similarity = 0.5001)
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
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete", method = "majority", threshold_similarity = 0.5001)
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
  complete <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "complete", method = "majority", threshold_similarity = 0.5001)
  partial <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001)
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
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001)
  edges <- tidygraph::as_tibble(merged[[1]], active = "edges")
  expect_true(all(c("dynamic_cluster_from", "dynamic_cluster_to") %in% names(edges)))
  expect_equal(tidygraph::active(merged[[1]]), "nodes")
})

test_that("the function can be run again, and works on unnamed lists", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "02", d = "02"),
    c(a = "01", b = "01", c = "02", d = "02")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001)
  expect_message(
    again <- merge_dynamic_clusters(merged, "cluster", "id", similarity_type = "complete", method = "majority", threshold_similarity = 0.5001),
    "replaced"
  )
  expect_equal(sum(names(node_table(again[[1]])) == "dynamic_cluster"), 1)
  expect_null(names(again))
})

test_that("windows without shared nodes trigger a warning", {
  windows <- toy_windows(c(a = "01", b = "01"), c(c = "01", d = "01"))
  expect_warning(
    merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001),
    "share no node"
  )
})

test_that("arguments are checked", {
  windows <- toy_windows(c(a = "01", b = "01"), c(a = "01", b = "01"))
  expect_snapshot(error = TRUE, merge_dynamic_clusters(windows, "cluster", "id", method = "majority", threshold_similarity = 0.5001))
  expect_error(
    merge_dynamic_clusters(windows, "cluster", "id", method = "majority", threshold_similarity = 0.5, similarity_type = "partial"),
    "0.5"
  )
  expect_error(
    merge_dynamic_clusters(windows[1], "cluster", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001),
    "at least two"
  )
  expect_error(
    merge_dynamic_clusters(windows, "no_column", "id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001),
    "no_column"
  )
})

test_that("on the stagflation data, every node gets a dynamic cluster", {
  networks <- build_dynamic_networks(
    subset(Nodes_stagflation, source_type == "Stagflation"), Ref_stagflation,
    "source_id", "target_id", time_variable = "source_year", time_window = 10,
    window_step = 1, method = "angle",
    edges_threshold = 2, verbose = FALSE
  )[1:5]
  networks <- add_clusters(networks, clustering_method = "leiden",
                           objective_function = "modularity", seed = 1, verbose = FALSE)
  merged <- merge_dynamic_clusters(networks, "cluster_leiden", "source_id", similarity_type = "partial", method = "majority", threshold_similarity = 0.5001)
  all_nodes <- dplyr::bind_rows(lapply(merged, node_table))
  expect_false(anyNA(all_nodes$dynamic_cluster_leiden))
  # Fewer dynamic clusters than window clusters: some continuities are found.
  n_window <- sum(vapply(merged, function(g) length(unique(node_table(g)$cluster_leiden)), integer(1)))
  expect_lt(length(unique(all_nodes$dynamic_cluster_leiden)), n_window)
})

# ---- Jaccard matching ------------------------------------------------------

# Window 1: {a, b, c} and {d, e}; window 2: {a, b, x, y} and {c, d, e}.
# Jaccard (complete): {a,b,c} vs {a,b,x,y} = 2/5 = 0.4, {d,e} vs {c,d,e} = 2/3,
# {a,b,c} vs {c,d,e} = 1/5. Partial (nodes present in both windows only):
# {a,b,c} vs {a,b} = 2/3.
jaccard_windows <- function() {
  toy_windows(
    c(a = "01", b = "01", c = "01", d = "02", e = "02"),
    c(a = "01", b = "01", x = "01", y = "01", c = "02", d = "02", e = "02")
  )
}
dynamic_of <- function(merged, window, id) {
  nodes <- node_table(merged[[window]])
  nodes$dynamic_cluster[nodes$id == id]
}

test_that("the Jaccard matching links the clusters by decreasing Jaccard index", {
  low <- merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                threshold_similarity = 0.3, similarity_type = "complete", verbose = FALSE)
  # both pairs reach 0.3: each past identifier passes to its best match
  expect_equal(dynamic_of(low, 2, "a"), dynamic_of(low, 1, "a"))
  expect_equal(dynamic_of(low, 2, "d"), dynamic_of(low, 1, "d"))
  high <- merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                 threshold_similarity = 0.5, similarity_type = "complete", verbose = FALSE)
  expect_equal(dynamic_of(high, 2, "d"), dynamic_of(high, 1, "d"))
  expect_false(dynamic_of(high, 2, "a") == dynamic_of(high, 1, "a"))
})

test_that("the partial Jaccard index ignores the nodes that are in one window only", {
  partial <- merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                    threshold_similarity = 0.6, similarity_type = "partial", verbose = FALSE)
  expect_equal(dynamic_of(partial, 2, "a"), dynamic_of(partial, 1, "a"))
  complete <- merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                     threshold_similarity = 0.6, similarity_type = "complete", verbose = FALSE)
  expect_false(dynamic_of(complete, 2, "a") == dynamic_of(complete, 1, "a"))
})

test_that("an identifier passes to one cluster only, even when the clusters split", {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "01", d = "01"),
    c(a = "01", b = "01", c = "02", d = "02")
  )
  merged <- merge_dynamic_clusters(windows, "cluster", "id", method = "jaccard",
                                   threshold_similarity = 0.3, similarity_type = "complete", verbose = FALSE)
  new <- unique(node_table(merged[[2]])$dynamic_cluster)
  expect_length(new, 2)
  # the double majority, which needs more than half of each cluster, does not link them
  expect_equal(sum(new %in% node_table(merged[[1]])$dynamic_cluster), 1)
})

test_that("the thresholds are checked for each method", {
  expect_error(merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "majority",
                                      threshold_similarity = 0.3, similarity_type = "partial"), "0.5")
  expect_error(merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                      threshold_similarity = 0, similarity_type = "partial"), "jaccard")
  expect_error(merge_dynamic_clusters(jaccard_windows(), "cluster", "id", similarity_type = "partial",
                                      threshold_similarity = 0.6), "method")
  expect_message(merge_dynamic_clusters(jaccard_windows(), "cluster", "id", method = "jaccard",
                                        threshold_similarity = 0.6, similarity_type = "partial"), "Jaccard")
})

test_that("merge_diagnostics works with the Jaccard matching", {
  d <- merge_diagnostics(jaccard_windows(), "cluster", "id", method = "jaccard",
                         thresholds = c(0.3, 0.5), similarity_types = "complete", verbose = FALSE)
  expect_equal(d$method, c("jaccard", "jaccard"))
  expect_gt(d$n_dynamic_clusters[2], d$n_dynamic_clusters[1])
  expect_s3_class(plot_merge_diagnostics(d), "ggplot")
  expect_error(merge_diagnostics(jaccard_windows(), "cluster", "id", method = "jaccard", thresholds = 1.5), "jaccard")
})
