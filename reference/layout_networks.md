# Compute the coordinates of nodes

**\[experimental\]**

A network has no natural position on a page. Force-directed layouts
place nodes so that tied nodes are close and untied nodes are pushed
apart, so that clusters appear as dense regions: Kamada-Kawai (`"kk"`)
draws graph distances as geometric distances (Kamada and Kawai 1989) ,
Fruchterman-Reingold (`"fr"`) balances attraction along edges and
repulsion between all nodes (Fruchterman and Reingold 1991) .
`layout_networks()` computes such coordinates with
[`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html),
which gives access to the layouts of igraph and graphlayouts, and stores
them in the node table.

For a list of networks, the coordinates of a node may jump from one
window to the next, which makes the plots hard to compare. With
`compute_dynamic_coordinates = TRUE`, the coordinates of window \\t-1\\
are used as the starting positions of the layout of window \\t\\, so
that the nodes present in both windows tend to stay in the same region.

## Usage

``` r
layout_networks(
  graphs,
  node_id,
  layout,
  compute_dynamic_coordinates = FALSE,
  save_coordinates = FALSE,
  ...
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- node_id:

  Character scalar, the node column with a unique identifier, used to
  join the coordinates.

- layout:

  Character scalar, a layout accepted by
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  (e.g. `"kk"`, `"fr"`, `"stress"`).

- compute_dynamic_coordinates:

  Logical. For a list of graphs, if `TRUE`, each layout starts from the
  coordinates of the previous window (see the details).

- save_coordinates:

  Logical. If `TRUE`, the coordinates are also stored in `x_{layout}`
  and `y_{layout}` (e.g. `x_kk`, `y_kk`), so that they are not lost when
  another layout is computed.

- ...:

  Other arguments of the layout (see
  [`ggraph::create_layout()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  and the igraph or graphlayouts function behind it).

## Value

The same object as `graphs`, with node columns `x` and `y` (any existing
`x` and `y` are replaced) and, if `save_coordinates = TRUE`,
`x_{layout}` and `y_{layout}`.

## Details

Force-directed layouts are stochastic and sensitive to the starting
positions: set a seed with
[`set.seed()`](https://rdrr.io/r/base/Random.html) before calling the
function to reproduce a figure. Distances in such plots are only roughly
interpretable: two clusters drawn side by side are more tied than two
clusters at opposite ends, but distances should not be measured.

**Weights.** igraph uses the `weight` edge column automatically, but not
always in the same way: Fruchterman-Reingold (`"fr"`) reads weights as
attraction (heavier edges pull nodes closer), while Kamada-Kawai
(`"kk"`) reads them as distances, so that similarity weights push the
most similar nodes apart and weakly tied nodes to the margins. With
similarity weights, prefer `"fr"`, or pass `weights = NA` to ignore
them.

For dynamic coordinates, nodes absent from the previous window start at
(0, 0). The starting positions are passed to the layout as its `coords`
argument: this works only with layouts that accept it, such as `"fr"`
and `"kk"` of igraph.

## References

Fruchterman TMJ, Reingold EM (1991). “Graph Drawing by Force-Directed
Placement.” *Software: Practice and Experience*, **21**(11), 1129–1164.
[doi:10.1002/spe.4380211102](https://doi.org/10.1002/spe.4380211102) .  
  
Kamada T, Kawai S (1989). “An Algorithm for Drawing General Undirected
Graphs.” *Information Processing Letters*, **31**(1), 7–15.
[doi:10.1016/0020-0190(89)90102-6](https://doi.org/10.1016/0020-0190%2889%2990102-6)
.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 2
)
set.seed(1)
network <- layout_networks(network, node_id = "source_id", layout = "fr")
head(tidygraph::as_tibble(network, active = "nodes")[c("source_id", "x", "y")])
#> # A tibble: 6 × 3
#>   source_id      x     y
#>   <chr>      <dbl> <dbl>
#> 1 96284971   -4.27 -2.09
#> 2 37095547  -14.1  -6.22
#> 3 46282251  -13.5  -7.26
#> 4 214927     -4.60 -7.96
#> 5 2207578    -6.95 -4.36
#> 6 10729971   -3.53 -5.94
```
