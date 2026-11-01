# Colour the nodes and edges of networks, or the clusters of an alluvial

**\[experimental\]**

`color_networks()` gives a colour to each category of a node column
(typically a cluster column) and stores it in a `color` column of the
nodes. Each edge gets a colour mixing the colours of its two ends, so
that edges within a cluster take the colour of the cluster and edges
between clusters a blend of both.

`color_alluvial()` does the same for the table of an alluvial plot
created with
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

## Usage

``` r
color_networks(
  graphs,
  column_to_color,
  color = NULL,
  unique_color_across_list = FALSE,
  verbose = TRUE
)

color_alluvial(alluv_dt, column_to_color, color = NULL, verbose = TRUE)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- column_to_color:

  Character scalar, the column with the categories to colour, e.g.
  `cluster_leiden`
  ([`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md))
  or `dynamic_cluster_leiden`
  ([`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)).

- color:

  The colours: `NULL` (default palettes, see the details), a character
  vector of colours, given to the categories by decreasing number of
  nodes, or a data frame whose first column holds the categories and the
  second column, named `color`, their colours.

- unique_color_across_list:

  Logical, for a list of graphs. `FALSE` (default) gives a category the
  same colour in every graph: use it for dynamic clusters. `TRUE` treats
  the same value in two graphs as two categories (cluster "01" of two
  windows gets two colours): use it for clusters detected separately in
  each window.

- verbose:

  Logical. If `TRUE` (default), reports when colours are recycled or
  truncated.

- alluv_dt:

  A data frame created with
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

## Value

`color_networks()`: the same object as `graphs` with a `color` column
for nodes and edges. `color_alluvial()`: `alluv_dt` with a `color`
column.

## Details

Giving the same colour to a cluster in every figure (network plots,
alluvial, tables) is what lets a reader follow it. Pass the same data
frame of colours to `color_networks()` and `color_alluvial()` for this.

Without `color`, the function uses the ggplot2 palette (without black)
for up to 7 categories, completed by the Okabe-Ito palette (without
black and grey) for up to 14; beyond 14 categories, the colours are
recycled and several clusters share a colour. For many clusters, give a
palette explicitly (e.g. from the scico or paletteer packages), or
colour only the largest clusters and give the others a neutral grey.

A vector with fewer colours than categories is recycled, a longer one is
truncated. A data frame of colours that misses some categories leaves
their colour missing.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1
)
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

network <- color_networks(network, column_to_color = "cluster_leiden")
unique(tidygraph::as_tibble(network, active = "nodes")[c("cluster_leiden", "color")])
#> # A tibble: 6 × 2
#>   cluster_leiden color  
#>   <chr>          <chr>  
#> 1 01             #F8766D
#> 2 06             #B79F00
#> 3 02             #00BA38
#> 4 03             #619CFF
#> 5 04             #00BFC4
#> 6 05             #F564E3
```
