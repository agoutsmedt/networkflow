test_that("coupling weights match the formulas on a hand-computed example", {
  build <- function(measure) {
    build_network(toy_documents(), toy_citations(), "doc", "ref",
                  projection_method = "structured", cooccurrence_method = measure)
  }
  # Coupling angle: shared / sqrt(L_i L_k)
  angle <- build("coupling_angle")
  expect_equal(edge_weight_between(angle, "d1", "d2"), 2 / sqrt(3 * 2))
  expect_equal(edge_weight_between(angle, "d1", "d3"), 1 / sqrt(3 * 2))
  expect_equal(edge_weight_between(angle, "d3", "d4"), 1 / sqrt(2 * 2))
  expect_equal(igraph::ecount(angle), 3)

  # Coupling strength: sum of log(N / f_j) over shared references / (L_i L_k),
  # with N = 4 documents and f_j = 2 for every shared reference.
  strength <- build("coupling_strength")
  expect_equal(edge_weight_between(strength, "d1", "d2"), 2 * log(4 / 2) / (3 * 2))
  expect_equal(edge_weight_between(strength, "d3", "d4"), log(4 / 2) / (2 * 2))
})

test_that("the threshold applies to the number of shared targets", {
  network <- build_network(toy_documents(), toy_citations(), "doc", "ref",
                           projection_method = "structured",
                           cooccurrence_method = "coupling_angle", edges_threshold = 2)
  expect_equal(igraph::ecount(network), 1)
  # Nodes without edges are dropped by default...
  expect_setequal(tidygraph::as_tibble(network, active = "nodes")$doc, c("d1", "d2"))
  # ...and kept with keep_singleton = TRUE.
  with_singletons <- build_network(toy_documents(), toy_citations(), "doc", "ref",
                                   projection_method = "structured",
                                   cooccurrence_method = "coupling_angle",
                                   edges_threshold = 2, keep_singleton = TRUE)
  expect_equal(igraph::vcount(with_singletons), 4)
})

test_that("time windows are successive or sliding", {
  build <- function(overlapping) {
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref",
                           time_variable = "year", time_window = 2,
                           overlapping_window = overlapping,
                           cooccurrence_method = "coupling_angle",
                           keep_singleton = TRUE, verbose = FALSE)
  }
  successive <- build(FALSE)
  expect_named(successive, c("2000-2001", "2002-2003"))
  expect_setequal(tidygraph::as_tibble(successive[[1]], active = "nodes")$doc, c("d1", "d2"))
  expect_equal(unique(tidygraph::as_tibble(successive[[2]], active = "nodes")$time_window), "2002-2003")

  sliding <- build(TRUE)
  expect_named(sliding, c("2000-2001", "2001-2002", "2002-2003"))
})

test_that("a window that does not divide the period triggers a warning", {
  expect_warning(
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref",
                           time_variable = "year", time_window = 3,
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "last window is shorter"
  )
})

test_that("the input tables are not modified", {
  nodes <- data.table::as.data.table(toy_documents())
  citations <- data.table::as.data.table(toy_citations())
  nodes_before <- data.table::copy(nodes)
  citations_before <- data.table::copy(citations)
  build_dynamic_networks(nodes, citations, "doc", "ref", time_variable = "year",
                         time_window = 2, cooccurrence_method = "coupling_angle",
                         compute_size = FALSE, verbose = FALSE)
  expect_identical(nodes, nodes_before)
  expect_identical(citations, citations_before)
})

test_that("compute_size counts the citations received within the window", {
  # Documents cite each other: d2 and d3 cite d1, d3 cites d2.
  documents <- data.frame(doc = c("d1", "d2", "d3"), ref_id = c("d1", "d2", "d3"))
  citations <- data.frame(doc = c("d2", "d2", "d3", "d3", "d3"),
                          ref_id = c("d1", "x", "d1", "d2", "x"))
  network <- build_network(documents, citations, "doc", "ref_id",
                           projection_method = "structured",
                           cooccurrence_method = "coupling_angle",
                           compute_size = TRUE, keep_singleton = TRUE)
  sizes <- tidygraph::as_tibble(network, active = "nodes")
  expect_equal(sizes$node_size[match(c("d1", "d2", "d3"), sizes$doc)], c(2, 1, 0))
})

test_that("explicit choices are required and errors are informative", {
  expect_error(
    build_network(toy_documents(), toy_citations(), "doc", "ref", projection_method = "structured"),
    "cooccurrence_method"
  )
  expect_error(build_network(toy_documents(), toy_citations(), "doc", "ref"), "projection_method")
  expect_error(
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref", time_window = 2,
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "time_variable"
  )
  expect_error(
    build_network(toy_documents(), toy_citations(), "doc", "missing_column",
                  projection_method = "structured", cooccurrence_method = "coupling_angle"),
    "missing_column"
  )
  expect_snapshot(
    error = TRUE,
    build_network(toy_documents(), toy_citations(), "doc", "ref",
                  projection_method = "structured", cooccurrence_method = "cosine")
  )
})

test_that("duplicated node identifiers are reported", {
  documents <- rbind(toy_documents(), toy_documents()[1, ])
  expect_warning(
    build_network(documents, toy_citations(), "doc", "ref",
                  projection_method = "structured", cooccurrence_method = "coupling_angle"),
    "more than once"
  )
})

test_that("the statistical backbone works, with or without component filtering", {
  skip_if_not_installed("backbone")
  nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
  backbone <- build_network(nodes, Ref_stagflation, "source_id", "target_id",
                            projection_method = "statistical", model = "sdsm", alpha = 0.05)
  expect_s3_class(backbone, "tbl_graph")
  expect_gt(igraph::ecount(backbone), 0)
  filtered <- build_network(nodes, Ref_stagflation, "source_id", "target_id",
                            projection_method = "statistical", model = "sdsm", alpha = 0.05,
                            filter_components = TRUE)
  expect_equal(igraph::components(filtered)$no, 1)
  expect_error(
    build_network(nodes, Ref_stagflation, "source_id", "target_id",
                  projection_method = "statistical", model = "sdsm"),
    "alpha"
  )
})

test_that("verbose messages can be turned off", {
  expect_no_message(
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref",
                           time_variable = "year", time_window = 2,
                           cooccurrence_method = "coupling_angle", verbose = FALSE)
  )
  expect_message(
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref",
                           cooccurrence_method = "coupling_angle"),
    "projection_method"
  )
})
