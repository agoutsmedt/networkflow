# Turn a list of networks into the table of an alluvial plot

**\[experimental\]**

An alluvial plot shows the evolution of dynamic clusters: each time
window is a column, each cluster a block whose height is its share of
the nodes of the window, and the flows between blocks follow the nodes
that stay in the network from one window to the next (Rosvall and
Bergstrom 2010) . `networks_to_alluv()` stacks the node tables of a list
of networks into the long table that such a plot needs (one row per node
and window) and adds summary statistics on the clusters.

## Usage

``` r
networks_to_alluv(
  graphs,
  intertemporal_cluster_column,
  node_id,
  summary_cluster_stats = TRUE,
  keep_color = TRUE,
  color_column = "color",
  keep_cluster_label = TRUE,
  cluster_label_column = "cluster_label"
)
```

## Arguments

- graphs:

  A list of `tbl_graph`s with dynamic clusters (see
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)).

- intertemporal_cluster_column:

  Character scalar, the node column with the dynamic clusters (e.g.
  `"dynamic_cluster_leiden"`).

- node_id:

  Character scalar, the node column with a stable identifier; it is the
  "alluvium" that the flows follow.

- summary_cluster_stats:

  Logical. If `TRUE` (default), adds the statistics described in the
  return value, useful to keep only the main clusters in the plot.

- keep_color:

  Logical. If `TRUE` (default), keeps `color_column`.

- color_column:

  Character scalar, the colour column (`"color"`, from
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)).

- keep_cluster_label:

  Logical. If `TRUE` (default), keeps `cluster_label_column`.

- cluster_label_column:

  Character scalar, the label column (`"cluster_label"`, from
  [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)).

## Value

A `data.table` with one row per node and window: `window` (the name of
the graph in the list), `node_id`, the dynamic cluster, the colour and
label columns if kept, `y_alluv` (\\1/n_t\\, \\n_t\\ being the number of
nodes of the window, so that each window has a total height of 1), and,
with `summary_cluster_stats = TRUE`:

- `share_cluster_alluv`: the share of all rows that belong to the
  cluster, in per cent;

- `share_cluster_window`: the share of the window's nodes in the
  cluster, in per cent;

- `share_cluster_max`: the maximum of `share_cluster_window` over the
  windows;

- `length_cluster`: the number of windows where the cluster exists.

## Details

The alluvial method (following clusters over time with
`networks_to_alluv()`,
[`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
and
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md))
was initiated by Alexandre Truc.

## References

Rosvall M, Bergstrom CT (2010). “Mapping Change in Large Networks.”
*PLoS ONE*, **5**(1), e8694.
[doi:10.1371/journal.pone.0008694](https://doi.org/10.1371/journal.pone.0008694)
.

## See also

[`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md),
[`prepare_label_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md),
[`color_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
and
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md).

## Examples

``` r
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
alluv_dt[1:5]
#>    dynamic_cluster_leiden    window  source_id share_cluster_alluv
#>                    <char>    <char>     <char>               <num>
#> 1:                   cl_1 1975-1984    1172921               17.36
#> 2:                   cl_1 1975-1984    2901822               17.36
#> 3:                   cl_1 1975-1984 1111111128               17.36
#> 4:                   cl_1 1975-1984    1184127               17.36
#> 5:                   cl_1 1975-1984    7815498               17.36
#>    share_cluster_window share_cluster_max length_cluster  y_alluv
#>                   <num>             <num>          <int>    <num>
#> 1:                39.06             39.06              1 0.015625
#> 2:                39.06             39.06              1 0.015625
#> 3:                39.06             39.06              1 0.015625
#> 4:                39.06             39.06              1 0.015625
#> 5:                39.06             39.06              1 0.015625
```
