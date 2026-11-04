# Diagnose the threshold of the matching of clusters across windows

**\[experimental\]**

[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
links the clusters of successive windows when they share a majority of
their nodes (`method = "majority"`) or have a large enough Jaccard index
(`method = "jaccard"`), and the result depends on the threshold and on
the type of similarity. A low threshold links clusters easily and
produces a few long-lived dynamic clusters; a high one breaks
continuities and produces many short-lived clusters. There is no right
value, but there are *unstable* regions, where a small change of the
threshold changes the story. `merge_diagnostics()` runs the matching for
a grid of thresholds and similarity types and summarises, for each
setting, how the dynamic clusters change: how many there are, how long
they live, how many window clusters find a predecessor, and how the
nodes are spread between long-lived and short-lived clusters.
[`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
draws the result.

## Usage

``` r
merge_diagnostics(
  graphs,
  cluster_id,
  node_id,
  method = c("majority", "jaccard"),
  thresholds = NULL,
  similarity_types = c("complete", "partial"),
  min_length = 3,
  verbose = TRUE
)
```

## Arguments

- graphs:

  A list of at least two `tbl_graph`s in chronological order, with the
  clusters of each window (see
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)).

- cluster_id:

  Character scalar, the node column with the clusters of each window
  (e.g. `"cluster_leiden"`).

- node_id:

  Character scalar, the node column with an identifier that is stable
  across windows.

- method:

  `"majority"` or `"jaccard"`, the rule of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  to diagnose. Required: run the function once for each rule to compare
  them.

- thresholds:

  Numeric vector of thresholds to try, in \\(0.5, 1\]\\ for `"majority"`
  and \\(0, 1\]\\ for `"jaccard"`. `NULL` (default) tries 0.51, 0.55,
  0.6, 0.65, 0.7, 0.75, 0.8 and 0.9 for `"majority"`, and 0.1 to 0.8 by
  steps of 0.1 for `"jaccard"`.

- similarity_types:

  Subset of `c("complete", "partial")`: the types to try (see
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)).

- min_length:

  Whole number, the number of windows from which a dynamic cluster is
  called *persistent* (3 by default).

- verbose:

  Logical. If `TRUE` (default), reports progress.

## Value

A data frame with one row per threshold and similarity type, the
`method` and the columns described above, with the table of lifespans in
its `"lifespans"` attribute.

## Details

The matching is run for each setting (threshold and similarity type)
with
[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).
For each setting, with a *window cluster* meaning a cluster of one
window and a *dynamic cluster* the set of window clusters that share an
identifier, the columns of the result are:

- `n_window_clusters`: the number of window clusters (the same for all
  settings);

- `n_dynamic_clusters`: the number of dynamic clusters;

- `share_continued`: the share of the window clusters of the second
  window onwards that inherit the identifier of a cluster of the
  previous window (high: many continuities);

- `median_lifespan`, `max_lifespan`: the number of windows in which a
  dynamic cluster exists (median and maximum over dynamic clusters);

- `share_single_window`: the share of dynamic clusters that exist in one
  window only;

- `n_persistent`: the number of dynamic clusters existing in at least
  `min_length` windows;

- `share_nodes_persistent`: the share of the nodes of all windows (a
  node present in two windows counts twice) that belong to persistent
  dynamic clusters;

- `largest_share`: the share of those nodes in the largest dynamic
  cluster.

The table of the lifespans of each dynamic cluster is stored in the
attribute `"lifespans"` and used by
[`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md).

A good threshold is on a *plateau*: the main dynamic clusters, and the
share of nodes in persistent clusters, change little around it. Windows
without any shared node cannot be matched whatever the threshold, and
are reported in a warning.

## See also

[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
[`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md).

## Examples

``` r
networks <- build_dynamic_networks(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 10,
  window_step = 4,
  method = "angle",
  edges_threshold = 2,
  verbose = FALSE
)
#> Warning: The last window is shorter than the others.
#> ℹ The data end in "2013", but the last window ends in "2016".
networks <- add_clusters(
  networks,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

diagnostics <- merge_diagnostics(
  networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  method = "majority",
  thresholds = c(0.51, 0.6, 0.75),
  similarity_types = "partial",
  verbose = FALSE
)
#> Warning: 1 pair of consecutive windows share no node: its clusters cannot be matched,
#> whatever the threshold.
diagnostics[, c("similarity_type", "threshold", "n_dynamic_clusters", "median_lifespan")]
#>   similarity_type threshold n_dynamic_clusters median_lifespan
#> 1         partial      0.51                 15             2.0
#> 2         partial      0.60                 18             1.5
#> 3         partial      0.75                 20             1.0
```
