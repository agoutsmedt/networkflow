# Launch an Interactive Shiny App to Explore a Network Graph

**\[experimental\]**

This function launches an interactive Shiny application for visualizing
and exploring a network graph represented as a `tbl_graph` object. It
supports node and cluster interactivity with tooltips, dynamic sizing,
and customizable coloring. Users can inspect clusters interactively and
explore associated node metadata in a searchable datatable. The app is
designed for cluster/module exploration, so `cluster_id` and
`cluster_information` are required.

If the graph does not contain a layout (columns `x` and `y`) the
function will compute coordinates using
[`networkflow::layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
with the specified layout algorithm. If a layout name is provided, it
will replace any existing layout in the graph.

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
  layout = "kk"
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

  Character. Name of a layout to compute node coordinates (e.g. `"kk"`,
  `"fr"`). If specified, this replaces any existing layout. If `NULL`,
  existing `x`/`y` coordinates are used.

## Value

A Shiny app interface for exploring the network.

## Examples

``` r
library(networkflow)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

nodes <- networkflow::Nodes_stagflation |>
  dplyr::filter(source_type == "Stagflation") |>
  dplyr::mutate(source_id = as.character(source_id))

references <- networkflow::Ref_stagflation

g <- build_network(
  nodes = nodes,
  directed_edges = references,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  compute_size = FALSE,
  keep_singleton = FALSE
)

g <- add_clusters(
  g,
  clustering_method = "leiden",
  objective_function = "modularity"
)
#> ℹ The leiden method detected 7 clusters. The biggest cluster represents "35.4%" of the network.

if (FALSE) { # \dontrun{
launch_network_app(
  graph_tbl = g,
  cluster_id = "cluster_leiden",
  cluster_information = c("source_author", "source_title", "source_year", "source_journal"),
  cluster_tooltip = "Cluster",
  node_id = "source_id",
  node_tooltip = "source_label",
  node_size = NULL,
  color = NULL,
  layout = "kk"
)
} # }

# Dynamic networks
g_list <- build_dynamic_networks(
  nodes = nodes,
  directed_edges = references,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 20,
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  overlapping_window = TRUE,
  compute_size = FALSE,
  keep_singleton = FALSE
)
#> ℹ Backbone method selected: structured
#> ℹ Keep_singleton == FALSE: removing the nodes that are alone with no edge. 
#> 
#> ── Generation of the network for the 1975-1994 time window. ────────────────────
#> 
#> ── Generation of the network for the 1976-1995 time window. ────────────────────
#> 
#> ── Generation of the network for the 1977-1996 time window. ────────────────────
#> 
#> ── Generation of the network for the 1978-1997 time window. ────────────────────
#> 
#> ── Generation of the network for the 1979-1998 time window. ────────────────────
#> 
#> ── Generation of the network for the 1980-1999 time window. ────────────────────
#> 
#> ── Generation of the network for the 1981-2000 time window. ────────────────────
#> 
#> ── Generation of the network for the 1982-2001 time window. ────────────────────
#> 
#> ── Generation of the network for the 1983-2002 time window. ────────────────────
#> 
#> ── Generation of the network for the 1984-2003 time window. ────────────────────
#> 
#> ── Generation of the network for the 1985-2004 time window. ────────────────────
#> 
#> ── Generation of the network for the 1986-2005 time window. ────────────────────
#> 
#> ── Generation of the network for the 1987-2006 time window. ────────────────────
#> 
#> ── Generation of the network for the 1988-2007 time window. ────────────────────
#> 
#> ── Generation of the network for the 1989-2008 time window. ────────────────────
#> 
#> ── Generation of the network for the 1990-2009 time window. ────────────────────
#> 
#> ── Generation of the network for the 1991-2010 time window. ────────────────────
#> 
#> ── Generation of the network for the 1992-2011 time window. ────────────────────
#> 
#> ── Generation of the network for the 1993-2012 time window. ────────────────────
#> 
#> ── Generation of the network for the 1994-2013 time window. ────────────────────

g_list <- add_clusters(
  g_list,
  clustering_method = "leiden",
  objective_function = "modularity"
)
#> 
#> ── Cluster detection for the "1975-1994" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "29.7%" of the network.
#> 
#> ── Cluster detection for the "1976-1995" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "26.8%" of the network.
#> 
#> ── Cluster detection for the "1977-1996" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "33.8%" of the network.
#> 
#> ── Cluster detection for the "1978-1997" period ────────────────────────────────
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "40.6%" of the network.
#> 
#> ── Cluster detection for the "1979-1998" period ────────────────────────────────
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "32.8%" of the network.
#> 
#> ── Cluster detection for the "1980-1999" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "29.1%" of the network.
#> 
#> ── Cluster detection for the "1981-2000" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "39.1%" of the network.
#> 
#> ── Cluster detection for the "1982-2001" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "40.5%" of the network.
#> 
#> ── Cluster detection for the "1983-2002" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "33.3%" of the network.
#> 
#> ── Cluster detection for the "1984-2003" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "31%" of the network.
#> 
#> ── Cluster detection for the "1985-2004" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "26.7%" of the network.
#> 
#> ── Cluster detection for the "1986-2005" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "37.3%" of the network.
#> 
#> ── Cluster detection for the "1987-2006" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "35.2%" of the network.
#> 
#> ── Cluster detection for the "1988-2007" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "33.3%" of the network.
#> 
#> ── Cluster detection for the "1989-2008" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "34.5%" of the network.
#> 
#> ── Cluster detection for the "1990-2009" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "33.3%" of the network.
#> 
#> ── Cluster detection for the "1991-2010" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "33.8%" of the network.
#> 
#> ── Cluster detection for the "1992-2011" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "35.4%" of the network.
#> 
#> ── Cluster detection for the "1993-2012" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "36.4%" of the network.
#> 
#> ── Cluster detection for the "1994-2013" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "41.1%" of the network.

if (FALSE) { # \dontrun{
launch_network_app(
  graph_tbl = g_list,
  cluster_id = "cluster_leiden",
  cluster_information = c("source_author", "source_title", "source_year", "source_journal"),
  node_id = "source_id",
  node_tooltip = "source_label",
  node_size = NULL,
  color = NULL,
  layout = "kk"
)
} # }
```
