# Launch an Interactive Shiny App to Explore a Network Graph

**\[experimental\]**

Interpreting clusters means going back and forth between the structure
of the network and its content: which documents form this cluster, who
wrote them, when, which ones sit at its border? `launch_network_app()`
opens a local Shiny application for this reading. The network is drawn
with interactive nodes (with tooltips) and cluster labels; clicking a
cluster displays the metadata of its nodes (`cluster_information`) in a
searchable table. Node size, edge width, labels and the visibility of
edges can be adjusted to make dense networks readable. For a list of
networks (one per time window), a menu selects the window.

If `layout` is given, coordinates are computed with
[`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md),
replacing any existing `x` and `y`; with `layout = NULL`, the existing
`x` and `y` columns are used. The app needs the shiny, ggiraph, DT and
shinycssloaders packages.

## Usage

``` r
launch_network_app(
  graph_tbl,
  cluster_id,
  cluster_information,
  cluster_tooltip = NULL,
  node_id,
  node_tooltip = NULL,
  node_size = NULL,
  color = NULL,
  layout = "fr"
)
```

## Arguments

- graph_tbl:

  A `tbl_graph` object with or without layout coordinates.

- cluster_id:

  Column name in the node data identifying clusters. It can be the
  cluster ID or a unique label for each cluster.

- cluster_information:

  Character vector of node metadata columns to display in the data
  table.

- cluster_tooltip:

  Optional. Fixed text string to display when hovering over cluster
  labels (not a column name).

- node_id:

  Column name identifying node IDs.

- node_tooltip:

  Optional. Column name for tooltips shown when hovering over nodes.

- node_size:

  Optional. Column name used to scale node size.

- color:

  Optional. Column name used to color nodes. If `NULL`, colors are
  automatically assigned using
  `networkflow::color_networks(column_to_color = cluster_id)`.

- layout:

  Character. Name of a layout to compute node coordinates (e.g. `"fr"`,
  `"kk"`). If specified, this replaces any existing layout. If `NULL`,
  existing `x`/`y` coordinates are used.

## Value

A Shiny app interface for exploring the network.

## Examples

``` r
if (FALSE) { # \dontrun{
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_angle",
  edges_threshold = 1
)
network <- add_clusters(network, clustering_method = "leiden",
                        objective_function = "modularity", seed = 123)

launch_network_app(
  graph_tbl = network,
  cluster_id = "cluster_leiden",
  cluster_information = c("source_author", "source_title", "source_year", "source_journal"),
  node_id = "source_id",
  node_tooltip = "source_label",
  layout = "fr"
)

# One network per time window: the app shows a menu of windows
networks <- build_dynamic_networks(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 20,
  window_step = 1,
  cooccurrence_method = "coupling_angle"
)
networks <- add_clusters(networks, clustering_method = "leiden",
                         objective_function = "modularity", seed = 123)
launch_network_app(
  graph_tbl = networks,
  cluster_id = "cluster_leiden",
  cluster_information = c("source_author", "source_title", "source_year"),
  node_id = "source_id",
  layout = "fr"
)
} # }
```
