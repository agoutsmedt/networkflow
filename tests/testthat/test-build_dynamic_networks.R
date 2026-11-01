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
                           time_variable = "year", time_window = 3, keep_singleton = TRUE,
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

# ---- Beyond bibliographic coupling ----------------------------------------

# Cited documents of toy_citations(): r1..r5. Co-citation = documents cited
# together: r1-r2 by d1 and d2 (2), r1-r3 and r2-r3 by d1 (1), r3-r4 by d3
# (1), r4-r5 by d4 (1). r1 and r2 are each cited by 2 documents.
toy_cited <- function() data.frame(ref = paste0("r", 1:5), cited_year = 1990L)

test_that("co-citation links the cited documents cited together", {
  network <- build_network(toy_cited(), toy_citations(), "doc", "ref",
                           projection_method = "structured", cooccurrence_method = "coupling_angle",
                           projection_on = "target")
  expect_setequal(node_table(network)$ref, paste0("r", 1:5))
  expect_equal(igraph::ecount(network), 5)
  # shared citing documents / sqrt(citations of each): r1-r2 -> 2 / sqrt(2 * 2)
  expect_equal(edge_weight_between(network, "r1", "r2"), 1)
  expect_equal(edge_weight_between(network, "r1", "r3"), 1 / sqrt(2 * 2))
  expect_equal(edge_weight_between(network, "r4", "r5"), 1 / sqrt(2 * 1))
  # the threshold counts shared citing documents
  strict <- build_network(toy_cited(), toy_citations(), "doc", "ref",
                          projection_method = "structured", cooccurrence_method = "coupling_angle",
                          edges_threshold = 2, projection_on = "target")
  expect_equal(igraph::ecount(strict), 1)
})

test_that("with projection_on = 'target', compute_size counts the citing documents", {
  network <- build_network(toy_cited(), toy_citations(), "doc", "ref",
                           projection_method = "structured", cooccurrence_method = "coupling_angle",
                           projection_on = "target", compute_size = TRUE)
  sizes <- node_table(network)
  expect_equal(sizes$node_size[match(paste0("r", 1:5), sizes$ref)], c(2, 2, 2, 2, 1))
})

test_that("windows can follow a time variable carried by the links", {
  citations <- transform(toy_citations(), citing_year = c(2000, 2000, 2000, 2001, 2001, 2002, 2002, 2003, 2003))
  windows <- build_dynamic_networks(
    toy_cited(), citations, "doc", "ref", projection_on = "target",
    time_in = "edges", time_variable = "citing_year", time_window = 2,
    cooccurrence_method = "coupling_angle", keep_singleton = TRUE, compute_size = TRUE,
    verbose = FALSE
  )
  expect_named(windows, c("2000-2001", "2002-2003"))
  # 2000-2001: d1 and d2 cite r1, r2, r3 (d1) and r1, r2 (d2)
  first <- node_table(windows[[1]])
  expect_setequal(first$ref, c("r1", "r2", "r3"))
  expect_equal(first$node_size[match(c("r1", "r2", "r3"), first$ref)], c(2, 2, 1))
  expect_setequal(node_table(windows[[2]])$ref, c("r3", "r4", "r5"))
  # nodes carry the window
  expect_equal(unique(first$time_window), "2000-2001")
})

test_that("links with missing time or unknown nodes are dropped with a warning", {
  citations <- transform(toy_citations(), citing_year = c(2000, 2000, NA, 2001, 2001, 2002, 2002, 2003, 2003))
  expect_warning(
    build_dynamic_networks(toy_cited(), citations, "doc", "ref", projection_on = "target",
                           time_in = "edges", time_variable = "citing_year", time_window = 2,
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "no valid"
  )
  expect_warning(
    build_dynamic_networks(toy_cited()[1:3, ], transform(toy_citations(), citing_year = 2000),
                           "doc", "ref", projection_on = "target", time_in = "edges",
                           time_variable = "citing_year", time_window = 1,
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "absent from `nodes`"
  )
})

test_that("nodes with a missing time are dropped with a warning", {
  documents <- toy_documents()
  documents$year[1] <- NA
  expect_warning(
    build_dynamic_networks(documents, toy_citations(), "doc", "ref", time_variable = "year",
                           time_window = 2, overlapping_window = TRUE, keep_singleton = TRUE,
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "1 node has no valid"
  )
  expect_error(
    build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref", time_variable = "nope",
                           cooccurrence_method = "coupling_angle", verbose = FALSE),
    "nope"
  )
})

# Authors: A signs d1 and d2, B signs d3, C signs d4. L(A) = 5 citations,
# L(B) = 2, L(C) = 2; A and B share r3, B and C share r4.
toy_authorship <- function() {
  citations <- toy_citations()
  citations$author <- c("A", "A", "A", "A", "A", "B", "B", "C", "C")
  citations
}

test_that("entities are coupled through the documents they sign", {
  network <- build_network(
    data.frame(author = c("A", "B", "C")), toy_authorship(), "author", "ref",
    document_id = "doc", projection_method = "structured", cooccurrence_method = "coupling_angle"
  )
  expect_equal(igraph::ecount(network), 2)
  expect_equal(edge_weight_between(network, "A", "B"), 1 / sqrt(5 * 2))
  expect_equal(edge_weight_between(network, "B", "C"), 1 / sqrt(2 * 2))
})

test_that("a reference cited by several documents of an entity counts more", {
  # A cites r1 through d1 and d2; B cites r1 once: min(2, 1) = 1 shared citation
  # but A's total (L = 4) grows with each document.
  citations <- data.frame(doc = c("d1", "d2", "d3"), ref = "r1", author = c("A", "A", "B"))
  one <- build_network(data.frame(author = c("A", "B")), citations, "author", "ref", document_id = "doc",
                       projection_method = "structured", cooccurrence_method = "coupling_angle")
  expect_equal(edge_weight_between(one, "A", "B"), 1 / sqrt(2 * 1))
})

test_that("entity coupling cannot be combined with other projections", {
  expect_error(
    build_network(data.frame(author = "A"), toy_authorship(), "author", "ref", document_id = "doc",
                  projection_method = "structured", cooccurrence_method = "coupling_similarity"),
    "coupling_angle"
  )
  expect_error(
    build_network(data.frame(author = "A"), toy_authorship(), "author", "ref", document_id = "doc",
                  projection_method = "structured", cooccurrence_method = "coupling_angle",
                  projection_on = "target"),
    "document_id"
  )
  expect_error(
    build_network(data.frame(author = "A"), toy_authorship(), "author", "ref", document_id = "nope",
                  projection_method = "structured", cooccurrence_method = "coupling_angle"),
    "nope"
  )
})

test_that("the generalised arguments do not change the usual results", {
  usual <- build_network(toy_documents(), toy_citations(), "doc", "ref",
                         projection_method = "structured", cooccurrence_method = "coupling_angle")
  explicit <- build_network(toy_documents(), toy_citations(), "doc", "ref",
                            projection_method = "structured", cooccurrence_method = "coupling_angle",
                            projection_on = "source")
  expect_equal(tidygraph::as_tibble(usual, active = "edges"), tidygraph::as_tibble(explicit, active = "edges"))
})

test_that("co-citation and entity coupling run on the stagflation data", {
  nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
  citations <- merge(
    transform(Ref_stagflation[, c("source_id", "target_id")],
              source_id = as.character(source_id), target_id = as.character(target_id)),
    nodes[, c("source_id", "source_year")], by = "source_id"
  )
  cited <- Ref_stagflation[!duplicated(Ref_stagflation$target_id), c("target_id", "target_author")]
  cited$target_id <- as.character(cited$target_id)
  cocitation <- build_network(cited, citations, "source_id", "target_id", projection_on = "target",
                              projection_method = "structured", cooccurrence_method = "coupling_angle",
                              edges_threshold = 3)
  expect_gt(igraph::ecount(cocitation), 0)
  windows <- build_dynamic_networks(
    cited, citations, "source_id", "target_id", projection_on = "target", time_in = "edges",
    time_variable = "source_year", time_window = 13, cooccurrence_method = "coupling_angle",
    edges_threshold = 2, verbose = FALSE
  )
  expect_named(windows, c("1975-1987", "1988-2000", "2001-2013"))
})

test_that("a window without any shared target gives an empty network and a warning", {
  warnings <- testthat::capture_warnings(
    windows <- build_dynamic_networks(toy_documents(), toy_citations(), "doc", "ref",
                                      time_variable = "year", time_window = 1,
                                      cooccurrence_method = "coupling_angle", verbose = FALSE)
  )
  expect_match(warnings, "is empty", all = FALSE)
  expect_true(all(vapply(windows, igraph::vcount, numeric(1)) == 0))
  # ...and the following steps accept empty windows
  expect_no_error(filter_components(windows))
})
