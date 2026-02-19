# Build a single network

Convenience wrapper around
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
for a single network.

## Usage

``` r
build_network(
  nodes,
  directed_edges,
  source_id,
  target_id,
  projection_method,
  cooccurrence_method = c("coupling_angle", "coupling_strength", "coupling_similarity"),
  edges_threshold = 1,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...
)
```

## Arguments

- nodes:

  Table of nodes and their metadata. One row per node. For example, a
  table of articles with identifiers, authors, publication year, etc.

- directed_edges:

  Table of bipartite links between `source_id` nodes and `target_id`
  entities (e.g., article -\> reference, author -\> paper).

- source_id:

  Quoted name of the source-side node identifier.

- target_id:

  Quoted name of the target-side identifier linked to each source node.

- projection_method:

  Method used to build the single network. Must be one of `"structured"`
  or `"statistical"`.

- cooccurrence_method:

  Cooccurrence method used by the structured workflow.

- edges_threshold:

  Threshold used to filter weak edges in structured mode.

- compute_size:

  Logical. If `TRUE`, computes the number of incoming edges per node
  (e.g., citation count).

- keep_singleton:

  Logical. If `FALSE`, removes nodes with no edges in the final network.

- filter_components:

  Logical. If `TRUE`, keeps only the main component(s) using
  [`networkflow::filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

- ...:

  Additional arguments passed to
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).
