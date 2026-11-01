# Order the clusters of an alluvial to reduce crossing flows

**\[experimental\]**

In an alluvial plot, flows between clusters drawn far apart cross many
other flows and make the plot unreadable. `minimize_crossing_alluvial()`
computes an order of the clusters that places clusters exchanging many
nodes next to each other. It is a heuristic, not an optimal solution to
the crossing problem.

The function builds a network of clusters: two clusters are tied when
nodes move from one to the other between windows, with a cosine weight
\\n\_{ab} / \sqrt{\bar{s}\_a \bar{s}\_b}\\, \\n\_{ab}\\ being the number
of moving nodes and \\\bar{s}\_a\\ the mean size of cluster \\a\\ over
its windows. It then removes the ties one by one, from the weakest to
the strongest, and records at each step the connected component of each
cluster. Clusters are ordered by this sequence of components: clusters
that stay together until the strongest ties are removed end up side by
side.

## Usage

``` r
minimize_crossing_alluvial(
  alluv_dt,
  intertemporal_cluster_column = NULL,
  node_id = NULL,
  window_column = "window"
)
```

## Arguments

- alluv_dt:

  A data frame created with
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- intertemporal_cluster_column:

  Character scalar, the column with the dynamic clusters.

- node_id:

  Character scalar, the column with the node identifiers.

- window_column:

  Character scalar, the column with the time windows (`"window"` by
  default).

## Value

`alluv_dt` with a new column `minimize_crossing_order`, the rank of each
cluster, that
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
uses to order the blocks.

## Details

The alluvial method (following clusters over time with
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
`minimize_crossing_alluvial()` and
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md))
was initiated by Alexandre Truc.

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
#> ℹ The data end in 2013, but the last window ends in 2014.
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
#>   (coupling), each node has one date: use sliding windows (`overlapping_window
#>   = TRUE` in `build_dynamic_networks()`). If windows already slide, the nodes
#>   may have changed completely, or the threshold may leave a window without
#>   edges.
alluv_dt <- networks_to_alluv(
  networks,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id"
)
#> ℹ No color column in the graphs: no colour kept (see `color_alluvial()`).
#> ℹ No cluster_label column in the graphs: no label kept (see `name_clusters()`).

alluv_dt <- minimize_crossing_alluvial(
  alluv_dt,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id"
)
#> Warning: no non-missing arguments to max; returning -Inf
unique(alluv_dt[, c("dynamic_cluster_leiden", "minimize_crossing_order")])
#> Key: <dynamic_cluster_leiden>
#>     dynamic_cluster_leiden minimize_crossing_order
#>                     <char>                   <int>
#>  1:                   cl_1                       1
#>  2:                  cl_10                      12
#>  3:                  cl_11                       3
#>  4:                  cl_12                       4
#>  5:                  cl_13                       5
#>  6:                   cl_2                       6
#>  7:                   cl_3                       7
#>  8:                   cl_4                       8
#>  9:                   cl_5                       9
#> 10:                   cl_6                      10
#> 11:                   cl_7                      11
#> 12:                   cl_8                      13
#> 13:                   cl_9                       2
```
