# Plot the evolution of dynamic clusters as an alluvial

`plot_alluvial()` draws the table created by
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)
with ggalluvial: one column per time window, one block per dynamic
cluster, with a height proportional to its share of the window, and
flows that follow the nodes from one window to the next. It can colour
the clusters
([`color_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)),
reorder them to limit crossings
([`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md))
and position their labels
([`prepare_label_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md))
on the way.

## Usage

``` r
plot_alluvial(
  alluv_dt,
  intertemporal_cluster_column,
  node_id,
  window_column = "window",
  color_column = "color",
  color_alluvial = FALSE,
  color = NULL,
  minimize_crossing = FALSE,
  prepare_label = FALSE,
  cluster_label_column = "cluster_label",
  print_plot_code = FALSE
)
```

## Arguments

- alluv_dt:

  A data frame created with
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- intertemporal_cluster_column:

  Character scalar, the column with the dynamic clusters (e.g.
  `"dynamic_cluster_leiden"`).

- node_id:

  Character scalar, the column with the node identifiers.

- window_column:

  Character scalar, the column with the time windows (`"window"` by
  default).

- color_column:

  Character scalar, the column with the colours of the clusters
  (`"color"` by default). If absent or `NULL`, ggplot2 chooses the
  colours.

- color_alluvial:

  Logical. If `TRUE`, runs
  [`color_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  first.

- color:

  With `color_alluvial = TRUE`, the colours passed to
  [`color_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- minimize_crossing:

  Logical. If `TRUE`, runs
  [`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
  first. Clusters are also reordered when `alluv_dt` already has a
  `minimize_crossing_order` column.

- prepare_label:

  Logical. If `TRUE`, runs
  [`prepare_label_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  first, which adds a `label_x` column with one label per cluster.

- cluster_label_column:

  Character scalar, the column with the labels used by
  [`prepare_label_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  (`"cluster_label"` by default).

- print_plot_code:

  Logical. If `TRUE`, prints the ggplot2 code of the plot.

## Value

A ggplot object.

## Details

Labels are drawn with ggrepel, which must be installed, when `alluv_dt`
has a `label_x` column. Small clusters clutter alluvials: filtering the
table on `share_cluster_max` or `length_cluster` (see
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md))
before plotting keeps the main ones.

The alluvial method was initiated by Alexandre Truc.

## Examples

``` r
# \donttest{
networks <- build_dynamic_networks(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 10,
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  verbose = FALSE
)
#> Warning: The last window is shorter than the others.
#> ℹ The data end in "2013", but the last window ends in "2014".
networks <- add_clusters(
  networks,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)
networks <- merge_dynamic_clusters(
  networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  threshold_similarity = 0.51,
  similarity_type = "partial"
)
#> Warning: 3 pairs of consecutive windows share no node: their clusters cannot be matched.
#> ℹ Clusters are matched through the nodes they share. In a network of documents
#>   (coupling), each node has one date: use sliding windows (`window_step = 1` in
#>   `build_dynamic_networks()`). If windows already slide, the nodes may have
#>   changed completely, or the threshold may leave a window without edges.
alluv_dt <- networks_to_alluv(
  networks,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id"
)
#> ℹ No color column in the graphs: no colour kept (see `color_alluvial()`).
#> ℹ No cluster_label column in the graphs: no label kept (see `name_clusters()`).
plot_alluvial(
  alluv_dt,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id",
  color_alluvial = TRUE,
  minimize_crossing = TRUE
)
#> Warning: no non-missing arguments to max; returning -Inf

# }
```
