# Order the rectangles of the alluvial in a way that minimize crossing of flow

This function transform the node_id into a factor ordered by a newly
created column named "minimize_crossing_order".

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

  Data.frame of the alluvial created using the
  networkflow::networks_to_alluv function

- intertemporal_cluster_column:

  The column with the identifier of the inter-temporal cluster. If you
  have used
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and
  [merge_dynamic_clusters()](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
  it is of the form `dynamic_cluster_{clustering_method}`.

- node_id:

  The column with the unique identifier of each node. This is the
  alluvium of the alluvial.

- window_column:

  The column with your time windows.

  @examples
