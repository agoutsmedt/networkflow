# Detect and Add Clusters to Graphs

**\[experimental\]**

This function takes as input a tibble graph (from
[tidygraph](https://tidygraph.data-imaginist.com/)) or a list of tibble
graphs, and then runs different cluster detection algorithms depending
on the method chosen by the user (see @details for information on the
different methods. The function associate each node to its corresponding
cluster identifier. It also creates a cluster attribute for edges: to
each edge is associated a corresponding cluster identifier if the two
nodes connected by the edge belong to the same cluster If nodes have a
different cluster, the edge takes "00" as cluster attribute.

## Usage

``` r
add_clusters(
  graphs,
  weights = NULL,
  clustering_method = c("leiden", "louvain", "fast_greedy", "infomap", "walktrap"),
  objective_function = c("modularity", "CPM"),
  resolution = 1,
  n_iterations = 1000,
  n_groups = NULL,
  node_weights = NULL,
  trials = 10,
  steps = 4,
  verbose = TRUE,
  seed = NA
)
```

## Arguments

- graphs:

  A tibble graph from
  [tidygraph](https://tidygraph.data-imaginist.com/), a list of tibble
  graphs or a data frame.

- weights:

  The weights of the edges. It must be a positive numeric vector, `NULL`
  or `NA`. If it is `NULL` and the input graph has a ‘weight’ edge
  attribute, then that attribute will be used. If `NULL` and no such
  attribute is present, then the edges will have equal weights. Set this
  to `NA` if the graph was a ‘weight’ edge attribute, but you don't want
  to use it for community detection. Edge weights are used to calculate
  weighted edge betweenness. This means that edges are interpreted as
  distances, not as connection strengths.

- clustering_method:

  The different clustering algorithms implemented in the function (see
  details). The parameters of the function depend of the clustering
  method chosen.

- objective_function:

  The objective function to maximize for the leiden algorithm. Whether
  to use the Constant Potts Model (CPM) or modularity. Must be either
  "CPM" or "modularity" (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)).
  CPM is used by default.

- resolution:

  The resolution parameter to use for leiden algorithm (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)).
  Higher resolutions lead to more smaller communities, while lower
  resolutions lead to fewer larger communities.

- n_iterations:

  the number of iterations to iterate the Leiden algorithm. Each
  iteration may improve the partition further (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)).

- n_groups:

  May be used by the fast greedy or the walktrap algorithm. Integer
  scalar, the desired number of communities. If too low or two high,
  then an error message is given.

- node_weights:

  May be used both for the Leiden or infomap algorithms. For Leiden, if
  this is not provided, it will be automatically determined on the basis
  of the objective_function (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)).
  For infomap, if it is not present, then all vertices are considered to
  have the same weight. A larger vertex weight means a larger
  probability that the random surfer jumps to that vertex (see
  [`igraph::cluster_infomap()`](https://r.igraph.org/reference/cluster_infomap.html)).

- trials:

  The number of attempts to partition the network (can be any integer
  value equal or larger than 1) for the infomap algorithm (see
  [`igraph::cluster_infomap()`](https://r.igraph.org/reference/cluster_infomap.html)).

- steps:

  The length of the random walks to perform for the walktrap algorithm
  (see
  [`igraph::cluster_walktrap()`](https://r.igraph.org/reference/cluster_walktrap.html))

- verbose:

  Set to `FALSE` if you don't want the function to display different
  sort of information.

- seed:

  Enter a random number to set the seed within the function. Some
  algorithms use heuristics and random processes that might result in
  different cluster each time the function is run. Setting the seed is
  particularly useful for reproducibility and if you want to make sure
  to find the same clusters each time the function is run with the same
  graphs.

## Value

The same tidygraph graph or tidygraph list as input, but with a new
cluster column for nodes with a column with the size of these clusters,
and three cluster columns for edges (see the details).

## Details

The function could be run indifferently on one tidigraph object or on a
list of tidygraph object, as created by
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md).

The function implements five different algorithms. Four exists in
[igraph](https://igraph.org/r/) and are used in this package through
their implement in [tidygraph](https://tidygraph.data-imaginist.com/)
(see
[group_graph()](https://tidygraph.data-imaginist.com/reference/group_graph.html)).
The function also implements the Leiden algorithm (Traag et al. 2019)
which is in `igraph` but not in `tidygraph` yet (see
[cluster_leiden()](https://r.igraph.org/reference/cluster_leiden.html)).

The newly created columns with the cluster identifier for nodes and
edges are named depending of the method used. If you use the Leiden
algorithm, the function will create a column called `cluster_leiden` for
nodes, and three columns for the edges, called `cluster_leiden_from`,
`cluster_leiden_to` and `cluster_leiden`.

The function also automatically calculates the percentage of total nodes
that are gathered in each cluster, in the column
`size_cluster_{clustering_method}`.

To make plotting easier later, a zero is put before one-digit cluster
identifier (cluster 5 becomes "05"; cluster 10 becomes "10").
Attributing a cluster identifier to edges allow for giving edges the
same color of the nodes they are connecting together if the two nodes
have the same color, or a different color from both nodes, if the nodes
belong to different clusters.

## References

Traag VA, Waltman L, van Eck NJ (2019). “From Louvain to Leiden:
Guaranteeing Well-Connected Communities.” *Scientific reports*,
**9**(1), 1–12.

## Examples

``` r
library(networkflow)

nodes <- networkflow::Nodes_stagflation |>
  dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

temporal_networks <- build_dynamic_networks(nodes = nodes,
directed_edges = references,
source_id = "source_id",
target_id = "target_id",
time_variable = "source_year",
cooccurrence_method = "coupling_similarity",
time_window = 20,
edges_threshold = 1,
overlapping_window = TRUE,
filter_components = TRUE)
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

temporal_networks <- add_clusters(temporal_networks,
objective_function = "modularity",
clustering_method = "leiden")
#> 
#> ── Cluster detection for the "1975-1994" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "25.7%" of the network.
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
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "29.5%" of the network.
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
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "33.8%" of the network.
#> 
#> ── Cluster detection for the "1993-2012" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "36.4%" of the network.
#> 
#> ── Cluster detection for the "1994-2013" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "41.1%" of the network.

temporal_networks[[1]]
#> # A tbl_graph: 74 nodes and 446 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 446 × 8 (active)
#>     from    to   weight Source  Target     cluster_leiden_from cluster_leiden_to
#>    <int> <int>    <dbl> <chr>   <chr>      <chr>               <chr>            
#>  1     6    11 0.00158  1021902 1111111122 02                  02               
#>  2     6    45 0.000173 1021902 1111111128 02                  03               
#>  3     6    66 0.000430 1021902 1111111134 02                  03               
#>  4     6    35 0.000644 1021902 1111111146 02                  02               
#>  5     6    20 0.000126 1021902 1111111180 02                  04               
#>  6     6    42 0.000614 1021902 1111111182 02                  02               
#>  7     6    21 0.000343 1021902 1111111183 02                  02               
#>  8     6    53 0.000259 1021902 1184127    02                  03               
#>  9     6    31 0.00121  1021902 14490177   02                  02               
#> 10     6    65 0.000274 1021902 16167977   02                  03               
#> # ℹ 436 more rows
#> # ℹ 1 more variable: cluster_leiden <chr>
#> #
#> # Node Data: 74 × 10
#>   source_id source_author source_year source_label   source_title source_journal
#>   <chr>     <chr>               <int> <chr>          <chr>        <chr>         
#> 1 16182155  GORDON-R             1975 GORDON-R-1975a ALTERNATIVE… BROOKINGS PAP…
#> 2 26283591  GORDON-R             1975 GORDON-R-1975b THE IMPACT … BROOKINGS PAP…
#> 3 16182201  OKUN-A               1975 OKUN-A-1975a   INFLATION: … BROOKINGS PAP…
#> # ℹ 71 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>

```
