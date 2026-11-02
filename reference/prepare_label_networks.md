# Position the labels of clusters in network and alluvial plots

`prepare_label_networks()` computes, for each category of
`cluster_label_column`, the mean coordinates of its nodes, where the
label of the cluster will be drawn by
[`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md):
`label_x` and `label_y` are the centre of gravity of the cluster.

`prepare_label_alluvial()` chooses the time window where the label of
each cluster of an alluvial will be drawn: the median window of the
windows where the cluster exists. The chosen row gets the label in a
`label_x` column, which
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
uses.

## Usage

``` r
prepare_label_networks(
  graphs,
  x = "x",
  y = "y",
  cluster_label_column = "cluster_label"
)

prepare_label_alluvial(
  alluv_dt,
  window_column = "window",
  cluster_label_column = "cluster_label"
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s with coordinates (see
  [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)).

- x, y:

  Character scalars, the node columns with the coordinates (`"x"` and
  `"y"` by default).

- cluster_label_column:

  Character scalar, the column with the labels: `"cluster_label"`
  ([`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md))
  by default, or a cluster column.

- alluv_dt:

  A data frame created with
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- window_column:

  Character scalar, the column of `alluv_dt` with the time windows
  (`"window"` by default, as created by
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)).

## Value

`prepare_label_networks()`: the same object as `graphs` with node
columns `label_x` and `label_y`. `prepare_label_alluvial()`: `alluv_dt`
with a `label_x` column, holding the label on the rows of the chosen
window and missing elsewhere.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1
)
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)
set.seed(1)
network <- layout_networks(network, node_id = "source_id", layout = "fr")
network <- prepare_label_networks(network, cluster_label_column = "cluster_leiden")
unique(tidygraph::as_tibble(network, active = "nodes")[c("cluster_leiden", "label_x", "label_y")])
#> # A tibble: 4 × 3
#>   cluster_leiden label_x label_y
#>   <chr>            <dbl>   <dbl>
#> 1 02                2.79   -1.13
#> 2 01               -6.64    1.38
#> 3 04                1.39    2.49
#> 4 03                2.39    4.06
```
