# Calculate the Coordinates of Nodes

**\[experimental\]**

The function implements different layout algorithms from
[ggraph](https://ggraph.data-imaginist.com/index.html) and
[graphlayouts](http://graphlayouts.schochastics.net/index.md). It also
implements a dynamic process of computation of coordinates for list of
tibble graphs, in order for the coordinates to be consistent between
graphs.

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

  A tibble graph from
  [tidygraph](https://tidygraph.data-imaginist.com/), a list of tibble
  graphs or a data frame.

- node_id:

  The column with the unique identifier of each node.

- layout:

  The type of layout to create. All strings accepted by the algorithm
  argument can also be supplied directly into layout. See
  [`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html)
  for more details on layouts.

- compute_dynamic_coordinates:

  When you have a list of tibble graphs and that some nodes are in
  multiple graphs, you may want that each node in several graphs has
  relatively similar coordinates. Set `compute_dynamic_coordinates` to
  `TRUE` to take into account the coordinates of the nodes in the `n-1`
  tibble graph, during the computation of the coordinates of the `n`
  tibble graph.

- save_coordinates:

  If you are running the function on the same object, the already
  existing columns `x` and `y` will be erased and new values will be
  computed. Set `save_coordinates` to `TRUE` to save the `x` and `y`
  values in columns with a name depending of the layout method used.

- ...:

  Additional arguments of the layout compatible with the function. See
  `ggraph::graph()`,
  [`ggraph::layout_tbl_graph_igraph()`](https://ggraph.data-imaginist.com/reference/layout_tbl_graph_igraph.html)
  and `graphlayouts::graphslayouts()` for more information.

## Value

The same tibble graph or list of tibble graphs with a column `x` and
`y`, and also additional column `x_{layout}` and `y_{layout}` if you
have set `save_coordinates` to `TRUE`.

## Details

The function allows for the use of different layouts implemented in
[ggraph](https://ggraph.data-imaginist.com/index.html) and
[graphlayouts](http://graphlayouts.schochastics.net/index.md). However,
some layout of `graphlayouts` are not working with this function. Also,
layouts do not all take as input pre-determined coordinates, meaning
that the `compute_dynamic_coordinates` argument does not work with all
layout. Please check if the layout used allows a parameter called
`coord`.

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

temporal_networks <- layout_networks(temporal_networks,
node_id = "source_id",
layout = "fr",
compute_dynamic_coordinates = TRUE)

temporal_networks [[1]]
#> # A tbl_graph: 74 nodes and 446 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 74 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 16182155   GORDON-R             1975 GORDON-R-19… ALTERNATIVE… "BROOKINGS PA…
#>  2 26283591   GORDON-R             1975 GORDON-R-19… THE IMPACT … "BROOKINGS PA…
#>  3 16182201   OKUN-A               1975 OKUN-A-1975a INFLATION: … "BROOKINGS PA…
#>  4 47749045   BRONFENBRENN…        1976 BRONFENBREN… ELEMENTS OF… "ZEITSCHRIFT …
#>  5 1111111141 KARNOSKY-D           1976 KARNOSKY-D-… THE LINK BE… "REVIEW"      
#>  6 1021902    FRIEDMAN-M           1977 FRIEDMAN-M-… NOBEL LECTU… "THE JOURNAL …
#>  7 5200398    GOLDSTEIN-M          1977 GOLDSTEIN-M… DOWNWARD PR… "STAFF PAPERS"
#>  8 31895842   GORDON-R             1977 GORDON-R-19… CAN THE INF… "BROOKINGS PA…
#>  9 14371908   RASCHE-R             1977 RASCHE-R-19… THE EFFECTS… ""            
#> 10 6013999    SHERMAN-H            1977 SHERMAN-H-1… MONOPOLY PO… "JOURNAL OF E…
#> # ℹ 64 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, x <dbl>, y <dbl>
#> #
#> # Edge Data: 446 × 5
#>    from    to   weight Source  Target    
#>   <int> <int>    <dbl> <chr>   <chr>     
#> 1     6    11 0.00158  1021902 1111111122
#> 2     6    45 0.000173 1021902 1111111128
#> 3     6    66 0.000430 1021902 1111111134
#> # ℹ 443 more rows
```
