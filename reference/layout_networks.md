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
which gives access to the layouts of [`igraph`](https://r.igraph.org/)
and [`graphlayouts`](https://schochastics.github.io/graphlayouts/), and
stores them in the node table. Two layouts that are not in `ggraph` are
added: `"forceatlas2"` (Jacomy et al. 2014) , the force-directed layout
of Gephi, and `"umap"` (McInnes et al. 2018) , which embeds the
distances between nodes with the UMAP algorithm.

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
  (e.g. `"kk"`, `"fr"`, `"stress"`), or `"forceatlas2"` or `"umap"` (see
  the details).

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
  and the `igraph` or `graphlayouts` function behind it; the arguments
  of `"forceatlas2"` and `"umap"` are given in the details).

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

**Weights.** `igraph` uses the `weight` edge column automatically, but
not always in the same way: Fruchterman-Reingold (`"fr"`) reads weights
as attraction (heavier edges pull nodes closer), while Kamada-Kawai
(`"kk"`) reads them as distances, so that similarity weights push the
most similar nodes apart and weakly tied nodes to the margins. With
similarity weights, prefer `"fr"`, or pass `weights = NA` to ignore
them.

**ForceAtlas2** (`layout = "forceatlas2"`). A force-directed layout of
the same family as Fruchterman-Reingold, designed for the exploration of
networks (Jacomy et al. 2014) . Edges attract their ends in proportion
to their weight and to the distance; all nodes repel each other in
proportion to the product of their degrees plus one, so that hubs push
the others away and clusters separate; a gravity keeps the components
around the centre. The speed adapts to the stability of the forces. This
is a compact implementation, not the one of Gephi: the repulsion is
computed between all the pairs of nodes (no Barnes-Hut approximation),
so that the cost grows with the square of the number of nodes
(comfortable up to a few thousand). Arguments: `iterations` (500),
`scaling_ratio` (2, the strength of the repulsion: smaller values give a
more compact layout), `gravity` (1), `strong_gravity` (`FALSE`; `TRUE`
keeps disconnected components close), `linlog` (`FALSE`; `TRUE` uses a
logarithmic attraction, which separates the clusters more) and `weights`
(the edge column is used by default, divided by its maximum; `NA`
ignores it). Isolated components drift away from the rest: keep the main
component with
[`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
first, or use `strong_gravity = TRUE`.

**UMAP** (`layout = "umap"`). UMAP (McInnes et al. 2018) places points
in the plane so as to preserve their neighbourhoods. It needs distances
between all the pairs of nodes, which a network does not give directly:
the distance between two nodes is the length of the shortest path
between them, an edge being as long as the inverse of its weight (a
strong tie is a short one), and nodes of different components are set
farther apart than any connected pair. The coordinates come from
[`uwot::umap()`](https://jlmelville.github.io/uwot/reference/umap.html).
Arguments: `n_neighbors` (15, the size of the neighbourhood preserved:
larger values give a more global view), `min_dist` (0.1, how tightly
nodes are packed), `spread` (1) and `weights` (as above), and any other
argument of
[`uwot::umap()`](https://jlmelville.github.io/uwot/reference/umap.html).
The package `uwot` must be installed. UMAP often gives compact, well
separated clusters, but, like the force-directed layouts, its distances
are not to be measured, and the picture is partly built by the choice of
`n_neighbors`.

For dynamic coordinates, nodes absent from the previous window start at
(0, 0). The starting positions are passed to the layout as its `coords`
argument: this works only with layouts that accept it, such as `"fr"`
and `"kk"` of `igraph`, and `"forceatlas2"`; `"umap"` ignores them.

## References

Fruchterman TMJ, Reingold EM (1991). “Graph Drawing by Force-Directed
Placement.” *Software: Practice and Experience*, **21**(11), 1129–1164.
[doi:10.1002/spe.4380211102](https://doi.org/10.1002/spe.4380211102) .  
  
Jacomy M, Venturini T, Heymann S, Bastian M (2014). “ForceAtlas2, a
Continuous Graph Layout Algorithm for Handy Network Visualization
Designed for the Gephi Software.” *PLoS ONE*, **9**(6), e98679.
[doi:10.1371/journal.pone.0098679](https://doi.org/10.1371/journal.pone.0098679)
.  
  
Kamada T, Kawai S (1989). “An Algorithm for Drawing General Undirected
Graphs.” *Information Processing Letters*, **31**(1), 7–15.
[doi:10.1016/0020-0190(89)90102-6](https://doi.org/10.1016/0020-0190%2889%2990102-6)
.  
  
McInnes L, Healy J, Melville J (2018). “UMAP: Uniform Manifold
Approximation and Projection for Dimension Reduction.”
[doi:10.48550/arXiv.1802.03426](https://doi.org/10.48550/arXiv.1802.03426)
. 1802.03426.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 2
)
#> ℹ Coupling (`biblionetwork::biblio_coupling()`) with "similarity": ties
#>   weighted by the idf-weighted cosine of the two lists of targets, after Shen
#>   et al. (2019), between source_id entities sharing target_id entities.
#> ℹ Pairs sharing fewer than 2 target_id are dropped (`edges_threshold`).
#> ℹ Nodes without any edge are removed (`keep_singleton = FALSE`).
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
