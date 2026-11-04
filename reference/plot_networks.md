# Plot networks

**\[experimental\]**

`plot_networks()` draws a network from the columns created by the
previous steps of the pipeline: coordinates
([`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)),
colours
([`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md))
and cluster labels
([`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
and
[`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)).
Edges are drawn as light arcs whose width follows their weight, nodes as
points whose size follows `node_size_column`, and each cluster gets a
label at its centre of gravity.

It is meant as a quick, ready-made figure. For a figure to publish,
`print_plot_code = TRUE` prints the
[`ggraph`](https://ggraph.data-imaginist.com/) code, which can be copied
and adapted.

## Usage

``` r
plot_networks(
  graphs,
  x = "x",
  y = "y",
  cluster_label_column = "cluster_label",
  node_size_column = "node_size",
  color_column = "color",
  color_networks = FALSE,
  color = NULL,
  max_arcs = 20000,
  print_plot_code = FALSE
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- x, y:

  Character scalars, the node columns with the coordinates (`"x"` and
  `"y"` by default).

- cluster_label_column:

  Character scalar, the node column with the labels to display:
  `"cluster_label"`
  ([`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md))
  by default, or a cluster column such as `cluster_leiden`.

- node_size_column:

  Character scalar, the numeric node column giving the size of nodes:
  `"node_size"` by default (see `compute_size` in
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)).
  If `NULL` or absent, all nodes have the same size.

- color_column:

  Character scalar, the column with the colours of nodes and edges
  (`"color"` by default, from
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)).

- color_networks:

  Logical. If `TRUE`,
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  is run first, with `cluster_label_column` as the categories.

- color:

  With `color_networks = TRUE`, the colours passed to
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- max_arcs:

  Whole number. Edges are drawn as arcs whose width follows their
  weight, unless a graph has more than `max_arcs` edges (20,000 by
  default): they are then drawn as straight lines of constant width,
  which is much faster (on a network of 140,000 edges, about 2 seconds
  and 1 GB of memory to draw, against 100 seconds and 11 GB with arcs).
  Set it to `Inf` to always draw arcs.

- print_plot_code:

  Logical. If `TRUE`, prints the
  [`ggplot2`](https://ggplot2.tidyverse.org/)/`ggraph` code of the plot.

## Value

For a `tbl_graph`, a ggplot object. For a list of graphs, a named list
of ggplot objects, one per graph (e.g. per time window).

## Details

Missing pieces are filled in: labels are positioned with
[`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
when `label_x` is absent, and edges without a `weight` column get a
weight of 1. Labels are drawn with
[`ggrepel`](https://ggrepel.slowkow.com/), which must be installed.

## Examples

``` r
# \donttest{
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 1
)
#> ℹ Coupling (`biblionetwork::biblio_coupling()`) with "similarity": ties
#>   weighted by the idf-weighted cosine of the two lists of targets, after Shen
#>   et al. (2019), between source_id entities sharing target_id entities.
#> ℹ Two source_id are linked when they share at least 1 target_id
#>   (`edges_threshold`).
#> ℹ Nodes without any edge are removed (`keep_singleton = FALSE`).
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)
set.seed(1)
network <- layout_networks(network, node_id = "source_id", layout = "fr")
network <- color_networks(network, column_to_color = "cluster_leiden")
if (requireNamespace("ggrepel", quietly = TRUE)) {
  plot_networks(
    network,
    cluster_label_column = "cluster_leiden",
    node_size_column = NULL
  )
}

# }
```
