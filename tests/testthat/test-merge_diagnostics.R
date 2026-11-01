# Window 1: clusters {a, b, c, d} and {e, f}; window 2: {a, b, c} and
# {d, e, f}. With the complete similarity:
# - cluster 01 -> 01: 3 of the 4 nodes of the old cluster go to the new one
#   (0.75) and all 3 nodes of the new one come from the old one (1);
# - cluster 02 -> 02: both old nodes go to the new one (1) but only 2 of the
#   3 nodes of the new one come from it (0.667).
# So both are matched below 0.667, only the first below 0.75, none above.
two_windows <- function() {
  toy_windows(
    c(a = "01", b = "01", c = "01", d = "01", e = "02", f = "02"),
    c(a = "01", b = "01", c = "01", d = "02", e = "02", f = "02")
  )
}

test_that("diagnostics follow a hand-computed example", {
  d <- merge_diagnostics(two_windows(), "cluster", "id", thresholds = c(0.51, 0.7, 0.8),
                         similarity_types = "complete", verbose = FALSE)
  expect_equal(d$n_window_clusters, rep(4, 3))
  expect_equal(d$n_dynamic_clusters, c(2, 3, 4))
  expect_equal(d$share_continued, c(1, 0.5, 0))
  expect_equal(d$median_lifespan, c(2, 1, 1))
  expect_equal(d$n_persistent, c(0, 0, 0))
  expect_equal(d$share_single_window, c(0, 2 / 3, 1))
  # nodes: 6 per window; at 0.51 both clusters last 2 windows
  persistent <- merge_diagnostics(two_windows(), "cluster", "id", thresholds = 0.51,
                                  similarity_types = "complete", min_length = 2, verbose = FALSE)
  expect_equal(persistent$n_persistent, 2)
  expect_equal(persistent$share_nodes_persistent, 1)
})

test_that("diagnostics agree with merge_dynamic_clusters", {
  windows <- two_windows()
  for (threshold in c(0.51, 0.7)) {
    merged <- merge_dynamic_clusters(windows, "cluster", "id", threshold_similarity = threshold,
                                     similarity_type = "partial")
    n_dynamic <- length(unique(unlist(lapply(merged, function(g) node_table(g)$dynamic_cluster))))
    d <- merge_diagnostics(windows, "cluster", "id", thresholds = threshold,
                           similarity_types = "partial", verbose = FALSE)
    expect_equal(d$n_dynamic_clusters, n_dynamic)
  }
})

test_that("the grid has one row per threshold and type, and lifespans are stored", {
  d <- merge_diagnostics(two_windows(), "cluster", "id", thresholds = c(0.6, 0.9), verbose = FALSE)
  expect_equal(nrow(d), 4)
  expect_setequal(d$similarity_type, c("complete", "partial"))
  lifespans <- attr(d, "lifespans")
  expect_setequal(names(lifespans), c("similarity_type", "threshold", "dynamic_cluster", "lifespan", "n_nodes"))
  expect_equal(sum(lifespans$n_nodes[lifespans$threshold == 0.6 & lifespans$similarity_type == "complete"]), 12)
})

test_that("arguments are checked and unmatched windows are reported", {
  windows <- two_windows()
  expect_error(merge_diagnostics(windows[1], "cluster", "id"), "at least two")
  expect_error(merge_diagnostics(windows, "cluster", "id", thresholds = 0.4), "0.5")
  expect_error(merge_diagnostics(windows, "nope", "id"), "nope")
  expect_error(merge_diagnostics(windows, "cluster", "id", similarity_types = "other"), "similarity_types")
  disjoint <- toy_windows(c(a = "01", b = "01"), c(c = "01", d = "01"))
  expect_warning(merge_diagnostics(disjoint, "cluster", "id", thresholds = 0.6, verbose = FALSE), "share no node")
})

test_that("both views of the plot are ggplots", {
  d <- merge_diagnostics(two_windows(), "cluster", "id", thresholds = c(0.51, 0.7, 0.8), verbose = FALSE)
  summary_plot <- plot_merge_diagnostics(d)
  expect_s3_class(summary_plot, "ggplot")
  expect_equal(nlevels(summary_plot$data$metric), 4)
  expect_s3_class(plot_merge_diagnostics(d, view = "lifespans"), "ggplot")
  expect_s3_class(plot_merge_diagnostics(d, metrics = c("n_dynamic_clusters", "largest_share")), "ggplot")
  expect_error(plot_merge_diagnostics(data.frame(x = 1)), "merge_diagnostics")
})
