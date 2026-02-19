# Plot the Alluvial

An easy way to plot the alluvial by using the columns created by the
different function of networkflow.

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

  A data.frame of an alluvial created with
  [networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- intertemporal_cluster_column:

  The column with the identifier of inter-temporal clusters. If you have
  used
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and
  [merge_dynamic_clusters()](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
  it is of the form `dynamic_cluster_{clustering_method}`.

- node_id:

  The column with the unique identifier of each node.

- window_column:

  The column of the alluvial with your time window. By default,
  "window", as created by
  [networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

- color_column:

  The column with the colors associated to the categories of
  `intertemporal_cluster_column`. By default, "color", as the result of
  [color_alluvial()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- color_alluvial:

  If no color has been set for the categories of
  `intertemporal_cluster_column`, you can attribute them colors by
  setting `color_alluvial` to `TRUE`.
  [color_alluvial()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  will be used.

- color:

  If `color_alluvial`is `TRUE`, the parameter `color` will be used to
  color the categories of the `intertemporal_cluster_column`. It may be
  a vector of colors (in a character format) or a two columns data frame
  with the first column as the distinct observations of the
  `intertemporal_cluster_column` and a second column with the vector of
  colors you want to use. If `NULL` colors will be automatically chosen
  by
  [color_alluvial()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)?

- minimize_crossing:

  If `TRUE`,
  [minimize_crossing_alluvial()](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
  is run to reorder the `intertemporal_cluster_colum` to limit
  overlapping in the plot.

- prepare_label:

  If `TRUE`,
  [prepare_label_alluvial()](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  is used to create a column `label_x` with

- cluster_label_column:

  If `prepare_label` is `TRUE`,
  [prepare_label_alluvial()](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  is used and the values of `cluster_label_column` are taken to be
  displayed as label. By default, "cluster_label", as it is the default
  name of the column created with
  [name_clusters()](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md).
  But you may also use the same column as in
  `intertemporal_cluster_column`.

- print_plot_code:

  Set to `TRUE` if you want the ggplot2 code to be printing. It is
  useful if you are not totally satisfied of the plot and want to
  manipulate the code yourself.
