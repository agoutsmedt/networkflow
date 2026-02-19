# Plot your Networks

An easy way to plot your networks by using the columns created by the
different function of networkflow.

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
  print_plot_code = FALSE
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

  The values of the column are taken to be displayed as labels. By
  default, "cluster_label", as it is the default name of the column
  created with
  [name_clusters()](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md).
  But you may also use the column produced with
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md).

- node_size_column:

  The column with numeric values to be used for the size of nodes. By
  default, "node_size", as the column created by
  [build_dynamic_networks()](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  or
  [build_network()](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  when `compute_size` set to `TRUE` in these functions. If `NULL`, the
  size of every nodes is set to 1.

- color_column:

  The column with the colors associated to the categories of
  `cluster_label_column`. By default, "color", as the result of
  [color_networks()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- color_networks:

  If no color has been set for the categories of `cluster_label_column`,
  you can attribute them colors by setting `color_networks` to `TRUE`.
  [color_networks()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
  will be used.

- color:

  If `color_networks`is `TRUE`, the parameter `color` will be used to
  color the categories of the `cluster_label_column`. It may be a vector
  of colors (in a character format) or a two columns data frame with the
  first column as the distinct observations of the
  `cluster_label_column` and a second column with the vector of colors
  you want to use. If `NULL` colors will be automatically chosen by
  [color_networks()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- print_plot_code:

  Set to `TRUE` if you want the ggplot2 code to be printing. It is
  useful if you are not totally satisfied of the plot and want to
  manipulate the code yourself.

## Details

If you have not used
[prepare_label_networks()](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md),
the function will be automatically used to produce coordinates to each
category of `cluster_label_column`.
