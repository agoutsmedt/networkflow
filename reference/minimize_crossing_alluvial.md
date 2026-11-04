# Order the clusters of an alluvial to reduce crossing flows

**\[experimental\]**

In an alluvial plot, flows between clusters drawn far apart cross many
other flows and make the plot unreadable. `minimize_crossing_alluvial()`
computes an order of the clusters that places clusters exchanging many
nodes next to each other. It is a heuristic, not an optimal solution to
the crossing problem.

The order is built in two steps. First, the function builds a network of
clusters: two clusters are tied when nodes move from one to the other
between windows, with a cosine weight \\n\_{ab} / \sqrt{\bar{s}\_a
\bar{s}\_b}\\, \\n\_{ab}\\ being the number of moving nodes and
\\\bar{s}\_a\\ the mean size of cluster \\a\\ over its windows. It
removes the ties one by one, from the weakest to the strongest, and
records at each step the connected component of each cluster. Clusters
are ordered by this sequence of components: clusters that stay together
until the strongest ties are removed end up side by side (this is the
order of the leaves of a single-linkage hierarchical clustering of the
clusters, computed with
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)). This groups
clusters that exchange nodes, but it does not count the crossings.

Second (`refine = TRUE`, the default), the order is improved against the
crossings themselves. For each pair of consecutive windows, the flows
are the numbers of nodes going from a cluster of the first window to a
cluster of the second; two flows cross when the order of their origins
and the order of their destinations are opposite, and the pair is
weighted by the product of the two flows. Each cluster in turn is moved
to the position that minimises the total, until no move helps. This is a
local search, which can end in a local optimum, and which costs more
with many clusters and flows. For large alluvials (more than 100
clusters, or more than two million pairs of flows), the barycentre
heuristic is used instead: each cluster is placed at the mean position
of the clusters it exchanges nodes with, as long as this reduces the
crossings. Filtering the small clusters first (see
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md))
gives faster and more readable alluvials.

## Usage

``` r
minimize_crossing_alluvial(
  alluv_dt,
  intertemporal_cluster_column = NULL,
  node_id = NULL,
  window_column = "window",
  refine = TRUE,
  max_passes = 10,
  verbose = TRUE
)
```

## Arguments

- alluv_dt:

  A data frame created with
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
  whose rows follow the chronological order of the windows (the order of
  the levels of `window_column` if it is a factor, as in
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
  or of their appearance).

- intertemporal_cluster_column:

  Character scalar, the column with the dynamic clusters.

- node_id:

  Character scalar, the column with the node identifiers.

- window_column:

  Character scalar, the column with the time windows (`"window"` by
  default).

- refine:

  Logical. If `TRUE` (default), refines the order against the crossings
  (see the description); if `FALSE`, keeps the order of the components.

- max_passes:

  Whole number, the maximum number of passes of the refinement over all
  the clusters.

- verbose:

  Logical. If `TRUE` (default), reports the weighted number of crossings
  before and after the refinement.

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
  method = "similarity",
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
  method = "majority", threshold_similarity = 0.51,
  similarity_type = "partial"
)
#> ℹ Linking clusters across windows with the "majority" rule: a cluster inherits
#>   the identifier of a past cluster when more than 0.51 of the nodes of each go
#>   to / come from the other (double majority). The comparison counts the nodes
#>   present in both windows only (`similarity_type` = "partial").
#> Warning: 3 pairs of consecutive windows share no node: their clusters cannot be matched.
#> ℹ Clusters are matched through the nodes they share. In a network of documents
#>   (coupling), each node has one date: the windows must overlap (`window_step`
#>   smaller than `time_window` in `build_dynamic_networks()`). If windows already
#>   overlap, the nodes may have changed completely, or the threshold may leave a
#>   window without edges.
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
#> ℹ Weighted crossings of the flows between windows: 0 with the order of the
#>   links, 0 after the refinement.
unique(alluv_dt[, c("dynamic_cluster_leiden", "minimize_crossing_order")])
#> Key: <dynamic_cluster_leiden>
#>     dynamic_cluster_leiden minimize_crossing_order
#>                     <char>                   <int>
#>  1:                   cl_1                       1
#>  2:                  cl_10                       9
#>  3:                  cl_11                      10
#>  4:                  cl_12                      12
#>  5:                  cl_13                      13
#>  6:                  cl_14                      14
#>  7:                  cl_15                      15
#>  8:                   cl_2                       2
#>  9:                   cl_3                       3
#> 10:                   cl_4                       4
#> 11:                   cl_5                       5
#> 12:                   cl_6                       6
#> 13:                   cl_7                       7
#> 14:                   cl_8                       8
#> 15:                   cl_9                      11
```
