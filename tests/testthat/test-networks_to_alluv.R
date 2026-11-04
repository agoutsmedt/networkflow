toy_alluvial <- function() {
  windows <- toy_windows(
    c(a = "01", b = "01", c = "02", d = "02"),
    c(a = "01", b = "01", c = "02", d = "02", e = "02"),
    c(b = "01", c = "01", d = "02", e = "02")
  )
  names(windows) <- c("1", "2", "3")
  merge_dynamic_clusters(windows, "cluster", "id", similarity_type = "partial")
}

test_that("the alluvial table has one row per node and window, and heights sum to 1", {
  alluv <- suppressMessages(networks_to_alluv(toy_alluvial(), "dynamic_cluster", "id"))
  expect_equal(nrow(alluv), 4 + 5 + 4)
  heights <- alluv[, .(total = sum(y_alluv)), by = window]
  expect_equal(heights$total, rep(1, 3))
})

test_that("summary statistics describe the clusters", {
  alluv <- suppressMessages(networks_to_alluv(toy_alluvial(), "dynamic_cluster", "id"))
  stats <- unique(alluv[, .(dynamic_cluster, length_cluster, share_cluster_max)])
  expect_true(all(stats$length_cluster >= 1 & stats$length_cluster <= 3))
  expect_true(all(stats$share_cluster_max <= 100))
  expect_equal(sum(unique(alluv[, .(dynamic_cluster, share_cluster_alluv)])$share_cluster_alluv), 100)
})

test_that("minimize_crossing_alluvial ranks every cluster", {
  alluv <- suppressMessages(networks_to_alluv(toy_alluvial(), "dynamic_cluster", "id"))
  ordered <- minimize_crossing_alluvial(alluv, "dynamic_cluster", "id")
  ranks <- unique(ordered[, .(dynamic_cluster, minimize_crossing_order)])
  expect_false(anyNA(ranks$minimize_crossing_order))
  expect_equal(sort(ranks$minimize_crossing_order), seq_len(nrow(ranks)))
})

test_that("plot_alluvial returns a ggplot built on the table", {
  alluv <- suppressMessages(networks_to_alluv(toy_alluvial(), "dynamic_cluster", "id"))
  plot <- plot_alluvial(alluv, "dynamic_cluster", "id", color_alluvial = TRUE,
                        minimize_crossing = TRUE)
  expect_s3_class(plot, "ggplot")
  expect_true("color" %in% names(plot$data))
  expect_error(plot_alluvial(alluv, "nope", "id"), "nope")
})

test_that("plot_alluvial accepts the list of graphs and filters the clusters", {
  graphs <- toy_alluvial()
  from_graphs <- suppressMessages(plot_alluvial(graphs, "dynamic_cluster", "id"))
  from_table <- suppressMessages(plot_alluvial(
    networks_to_alluv(graphs, "dynamic_cluster", "id"), "dynamic_cluster", "id"
  ))
  expect_s3_class(from_graphs, "ggplot")
  expect_equal(nrow(from_graphs$data), nrow(from_table$data))
  # a cluster present in a single window is dropped with min_windows_cluster = 2
  filtered <- suppressMessages(plot_alluvial(graphs, "dynamic_cluster", "id", min_windows_cluster = 3))
  expect_lt(nrow(filtered$data), nrow(from_graphs$data))
  expect_error(plot_alluvial(graphs, "dynamic_cluster", "id", min_windows_cluster = 3, min_share_cluster = 200),
               NA)
  expect_error(plot_alluvial(1:3, "dynamic_cluster", "id"), "alluv_dt")
})
