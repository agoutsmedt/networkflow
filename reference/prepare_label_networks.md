# Creating and Positioning Labels for Plotting Networks or Alluvial

`prepare_label_networks()` creates a label column that make it easy to
center label around particular clusters on the alluvial graph of
intertemporal networks. `prepare_label_alluvial()`

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

  A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/)
  or a list of tibble graphs.

- x, y:

  The columns of your tibble graph or list of tibble graphs with the x
  and y coordinates of your nodes (for instance, the coordinates produce
  by
  [layout_networks()](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)).

- cluster_label_column:

  The column with the cluster label you want to display. By default,
  "cluster_label", as it is the default name of the column created with
  [name_clusters()](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md).
  You may also use the cluster identifier created by
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  for simple cluster, or by
  [merge_dynamic_clusters()](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  for inter-temporal clusters.

- alluv_dt:

  A data.frame of an alluvial created with
  [networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- window_column:

  The column of the alluvial with your time window. By default,
  "window", as created by
  [networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).
