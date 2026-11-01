# Small fixtures built by hand, whose expected results can be computed on
# paper. They are functions (not objects) so that each test builds its own
# data (self-sufficient tests).

# Four documents and their references:
#   d1 = {r1, r2, r3}, d2 = {r1, r2}, d3 = {r3, r4}, d4 = {r4, r5}
# Shared references: d1-d2: 2 (r1, r2); d1-d3: 1 (r3); d3-d4: 1 (r4).
# r5 is cited once and cannot create a tie.
toy_citations <- function() {
  data.frame(
    doc = c("d1", "d1", "d1", "d2", "d2", "d3", "d3", "d4", "d4"),
    ref = c("r1", "r2", "r3", "r1", "r2", "r3", "r4", "r4", "r5")
  )
}

toy_documents <- function() {
  data.frame(
    doc = c("d1", "d2", "d3", "d4"),
    year = c(2000L, 2001L, 2002L, 2003L),
    title = c("inflation expectations", "inflation expectations policy",
              "oil shock supply", "oil supply prices")
  )
}

# Two triangles {a, b, c} and {d, e, f} joined by a weak edge c-d, plus a
# separate pair {g, h}. Leiden with modularity must find the two triangles.
toy_graph <- function(with_pair = FALSE) {
  nodes <- data.frame(
    id = c("a", "b", "c", "d", "e", "f"),
    year = 2000L,
    title = c("inflation rules", "inflation rules policy", "inflation policy",
              "oil supply shock", "oil supply", "oil shock prices")
  )
  edges <- data.frame(
    from = c("a", "a", "b", "d", "d", "e", "c"),
    to = c("b", "c", "c", "e", "f", "f", "d"),
    weight = c(1, 1, 1, 1, 1, 1, 0.1)
  )
  if (with_pair) {
    nodes <- rbind(nodes, data.frame(id = c("g", "h"), year = 2000L, title = c("x", "y")))
    edges <- rbind(edges, data.frame(from = "g", to = "h", weight = 1))
  }
  tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "id")
}

# Nodes and clusters of each window, for merge_dynamic_clusters(): a list of
# graphs whose node table has `id` and `cluster`. Edges are irrelevant to the
# matching rule; a chain links the nodes of each cluster.
toy_windows <- function(...) {
  windows <- list(...)
  lapply(windows, function(clusters) {
    nodes <- data.frame(id = names(clusters), cluster = unname(clusters))
    edges <- do.call(rbind, lapply(split(nodes$id, nodes$cluster), function(ids) {
      if (length(ids) < 2) return(NULL)
      data.frame(from = ids[-length(ids)], to = ids[-1])
    }))
    tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "id")
  })
}

# The stagflation network of the examples, with a threshold of 2.
stagflation_network <- function(threshold = 2) {
  build_network(
    nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
    directed_edges = Ref_stagflation,
    source_id = "source_id",
    target_id = "target_id",
    projection_method = "structured",
    cooccurrence_method = "coupling_angle",
    edges_threshold = threshold
  )
}

edge_weight_between <- function(graph, from, to) {
  ids <- tidygraph::as_tibble(graph, active = "nodes")[[1]]
  edges <- tidygraph::as_tibble(graph, active = "edges")
  pair <- (ids[edges$from] == from & ids[edges$to] == to) |
    (ids[edges$from] == to & ids[edges$to] == from)
  edges$weight[pair]
}
