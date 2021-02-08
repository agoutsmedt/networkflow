
-   [networkflow](#networkflow)
    -   [Installation](#installation)
    -   [The workflow for networks](#the-workflow-for-networks)
        -   [First step: creating the network and keeping the main
            component](#first-step-creating-the-network-and-keeping-the-main-component)
        -   [Second step: finding
            communities](#second-step-finding-communities)
        -   [Third step: giving a structure to your
            graph](#third-step-giving-a-structure-to-your-graph)
    -   [References](#references)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# networkflow

<!-- badges: start -->
<!-- badges: end -->

The goal of networkflow (a workflow for networks) is to propose a serie
of functions to make it easier and quicker to work on networks. It
mainly targets working on bibliometric networks (see the
[biblionetwork](https://github.com/agoutsmedt/biblionetwork) package for
creating such networks). This package heavily relies on
[igraph](https://igraph.org/r/) and
[tidygraph](https://tidygraph.data-imaginist.com/index.html), and aims
at producing ready-made networks for projecting them using
[ggraph](https://ggraph.data-imaginist.com/). This package does not
invent nothing new, properly speaking, but it allows the users to follow
more quickly and easily the main steps of network manipulation, from
creating the graph to projecting it. It is inspired by what could be
done with [GEPHI](https://gephi.org/): the package allows the use of the
Leiden community detection algorithm, as well as of the Force Atlas 2
layout, both being unavailable in igraph (and so in tidygraph).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agoutsmedt/networkflow")
```

## The workflow for networks

### First step: creating the network and keeping the main component

As soon as you have a nodes and a edges file (see the
[biblionetwork](https://github.com/agoutsmedt/biblionetwork) package for
creating such files), you can create a graph, using tidygraph and the
[tbl\_graph()](https://rdrr.io/cran/tidygraph/man/tbl_graph.html)
function. The next step, as it is recurrent in many network analyses,
notably in bibliometric netwoks like bibliographic coupling networks
would be to keep only the [main
component](https://en.wikipedia.org/wiki/Component_(graph_theory)) of
your network. This could be done in one step using the
`tbl_main_component()` function of `networkflow`.

``` r
library(networkflow)

## basic example code

graph <- tbl_main_component(nodes = Nodes_coupling, edges = Edges_coupling, directed = FALSE, node_key = "ItemID_Ref", nb_components = 1)
print(graph)
#> # A tbl_graph: 145 nodes and 2593 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 145 x 6 (active)
#>   Id     Author    Year Author_date   Title                     Journal         
#>   <chr>  <chr>    <int> <chr>         <chr>                     <chr>           
#> 1 96284~ ALBANES~  2003 ALBANESI-S-2~ Expectation traps and mo~ "The Review of ~
#> 2 37095~ ATESOGL~  1980 ATESOGLU-H-1~ Inflation and its accele~ "Journal of Pos~
#> 3 46282~ ATESOGL~  1982 ATESOGLU-H-1~ WAGES AND STAGFLATION     "JOURNAL OF POS~
#> 4 214927 BALL-L    1991 BALL-L-1991   The Genesis of Inflation~ "Journal of Mon~
#> 5 22075~ BALL-L    1995 BALL-L-1995a  Relative-Price Changes a~ "The Quarterly ~
#> 6 10729~ BALL-L    1995 BALL-L-1995b  Time-consistent policy a~ "Journal of Mon~
#> # ... with 139 more rows
#> #
#> # Edge Data: 2,593 x 5
#>    from    to weight Source  Target
#>   <int> <int>  <dbl>  <int>   <int>
#> 1     4     5 0.146  214927 2207578
#> 2     4    65 0.0408 214927 5982867
#> 3     4    46 0.0973 214927 8456979
#> # ... with 2,590 more rows
```

The parameter `nb_components` allows you to choose the number of
components you want to keep. For obvious reasons, it is settled to 1 by
default.

However, it could happen in some networks (for instance co-authorship
networks) that the second biggest component of your network is quite
large. To avoid removing too big components without knowing it, the
`tbl_main_component()` function integrates a warning that happens when a
secondary component gathering more than x% of the total number of nodes
is removed. The `threshold_alert` parameter is set to 0.05 by default,
but you can reduce it if you really want to avoid removing relatively
big components.

``` r
library(networkflow)

## basic example code

graph <- tbl_main_component(nodes = Nodes_coupling, edges = Edges_coupling, directed = FALSE, node_key = "ItemID_Ref", threshold_alert = 0.001)
#> Warning in tbl_main_component(nodes = Nodes_coupling, edges = Edges_coupling, :
#> Warning: you have removed a component gathering more than 0.001% of the nodes
print(graph)
#> # A tbl_graph: 145 nodes and 2593 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 145 x 6 (active)
#>   Id     Author    Year Author_date   Title                     Journal         
#>   <chr>  <chr>    <int> <chr>         <chr>                     <chr>           
#> 1 96284~ ALBANES~  2003 ALBANESI-S-2~ Expectation traps and mo~ "The Review of ~
#> 2 37095~ ATESOGL~  1980 ATESOGLU-H-1~ Inflation and its accele~ "Journal of Pos~
#> 3 46282~ ATESOGL~  1982 ATESOGLU-H-1~ WAGES AND STAGFLATION     "JOURNAL OF POS~
#> 4 214927 BALL-L    1991 BALL-L-1991   The Genesis of Inflation~ "Journal of Mon~
#> 5 22075~ BALL-L    1995 BALL-L-1995a  Relative-Price Changes a~ "The Quarterly ~
#> 6 10729~ BALL-L    1995 BALL-L-1995b  Time-consistent policy a~ "Journal of Mon~
#> # ... with 139 more rows
#> #
#> # Edge Data: 2,593 x 5
#>    from    to weight Source  Target
#>   <int> <int>  <dbl>  <int>   <int>
#> 1     4     5 0.146  214927 2207578
#> 2     4    65 0.0408 214927 5982867
#> 3     4    46 0.0973 214927 8456979
#> # ... with 2,590 more rows
```

### Second step: finding communities

Once you have you tidygraph graph, an important step is to run community
detection algorithms to group the nodes depending on their links. This
package uses the
[leidenAlg](https://cran.r-project.org/web/packages/leidenAlg/index.html)
package, and its `find_partition()` function, to implement the Leiden
algorithm ([Traag, Waltman, and van Eck 2019](#ref-traag2019)). The
`leiden_workflow()` function of our package runs the Leiden algorithm
and attributes a community number to each node in the `Com_ID` column,
but also to each edge (depending if the `from` and `to` nodes are within
the same community).

``` r
library(networkflow)

# creating again the graph
graph <- tbl_main_component(nodes = Nodes_coupling, edges = Edges_coupling, directed = FALSE, node_key = "ItemID_Ref", nb_components = 1)

# finding communities
graph <- leiden_workflow(graph)
#> Warning: The `add` argument of `group_by()` is deprecated as of dplyr 1.0.0.
#> Please use the `.add` argument instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
print(graph)
#> # A tbl_graph: 145 nodes and 2593 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 2,593 x 8 (active)
#>    from    to weight Source   Target com_ID_to com_ID_from Com_ID
#>   <int> <int>  <dbl>  <int>    <int> <chr>     <chr>       <chr> 
#> 1     4     5 0.146  214927  2207578 01        04          05    
#> 2     4    65 0.0408 214927  5982867 01        04          05    
#> 3     4    46 0.0973 214927  8456979 04        04          04    
#> 4     4     6 0.298  214927 10729971 04        04          04    
#> 5     4   113 0.0471 214927 16008556 01        04          05    
#> 6     4    62 0.0447 214927 16167977 01        04          05    
#> # ... with 2,587 more rows
#> #
#> # Node Data: 145 x 8
#>   Id     Author   Year Author_date  Title           Journal      Com_ID Size_com
#>   <chr>  <chr>   <int> <chr>        <chr>           <chr>        <chr>     <dbl>
#> 1 96284~ ALBANE~  2003 ALBANESI-S-~ Expectation tr~ "The Review~ 02        0.331
#> 2 37095~ ATESOG~  1980 ATESOGLU-H-~ Inflation and ~ "Journal of~ 01        0.490
#> 3 46282~ ATESOG~  1982 ATESOGLU-H-~ WAGES AND STAG~ "JOURNAL OF~ 01        0.490
#> # ... with 142 more rows
```

You can observe that the function also gives the size of the community,
by calculating the share of total nodes that are in each community.

The function also allows to play with the `resolution` parameter of
leidenAlg [`find_partition()`]() function. Varying the resolution of the
algorithm results in a different partition and different number of
communities. A lower resolution means less communities, and conversely.
The basic resolution of the `leiden_workflow()` is set by `res_1` and
equals 1 by default. You can vary this parameter, but also try a second
resolution with `res_2` and a third one with `res_3`:

``` r
library(networkflow)

# creating again the graph
graph <- tbl_main_component(nodes = Nodes_coupling, edges = Edges_coupling, directed = FALSE, node_key = "ItemID_Ref", nb_components = 1)

# finding communities
graph <- leiden_workflow(graph, res_1 = 0.5, res_2 = 2, res_3 = 3)
print(graph)
#> # A tbl_graph: 145 nodes and 2593 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 2,593 x 14 (active)
#>    from    to weight Source Target com_ID_to com_ID_from Com_ID Com_ID_2_to
#>   <int> <int>  <dbl>  <int>  <int> <chr>     <chr>       <chr>  <chr>      
#> 1     4     5 0.146  214927 2.21e6 02        01          03     08         
#> 2     4    65 0.0408 214927 5.98e6 02        01          03     01         
#> 3     4    46 0.0973 214927 8.46e6 01        01          01     08         
#> 4     4     6 0.298  214927 1.07e7 01        01          01     08         
#> 5     4   113 0.0471 214927 1.60e7 02        01          03     04         
#> 6     4    62 0.0447 214927 1.62e7 02        01          03     01         
#> # ... with 2,587 more rows, and 5 more variables: Com_ID_2_from <chr>,
#> #   Com_ID_2 <chr>, Com_ID_3_to <chr>, Com_ID_3_from <chr>, Com_ID_3 <chr>
#> #
#> # Node Data: 145 x 10
#>   Id    Author  Year Author_date Title Journal Com_ID Size_com Com_ID_2 Com_ID_3
#>   <chr> <chr>  <int> <chr>       <chr> <chr>   <chr>     <dbl> <chr>    <chr>   
#> 1 9628~ ALBAN~  2003 ALBANESI-S~ Expe~ "The R~ 01        0.510 08       14      
#> 2 3709~ ATESO~  1980 ATESOGLU-H~ Infl~ "Journ~ 02        0.490 04       03      
#> 3 4628~ ATESO~  1982 ATESOGLU-H~ WAGE~ "JOURN~ 02        0.490 04       03      
#> # ... with 142 more rows
```

Once you have detected different communities in your network, you are
well on the way of the projection of your graph, but two important steps
should be implemented before. First, you have to attribute some colors
to each community. These colors will be used for your nodes and edges
when you will project your graph with `ggraph`. The function
`community_colors` of the `networkflow` package allow to do that. You
just have to give it a palette (with as many colors as the number of
communities for a better visualisation).[1]

``` r
palette <- c("#1969B3","#01A5D8","#DA3E61","#3CB95F","#E0AF0C","#E25920","#6C7FC9","#DE9493","#CD242E","#6F4288","#B2EEF8","#7FF6FD","#FDB8D6","#8BF9A9","#FEF34A","#FEC57D","#DAEFFB","#FEE3E1","#FBB2A7","#EFD7F2","#5CAADA","#37D4F5","#F5779B","#62E186","#FBDA28","#FB8F4A","#A4B9EA","#FAC2C0","#EB6466","#AD87BC","#0B3074","#00517C","#871B2A","#1A6029","#7C4B05","#8A260E","#2E3679","#793F3F","#840F14","#401C56","#003C65","#741A09","#602A2A","#34134A","#114A1B","#27DDD1","#27DD8D","#4ADD27","#D3DD27","#DDA427","#DF2935","#DD27BC","#BA27DD","#3227DD","#2761DD","#27DDD1")

graph <- community_colors(graph, palette, community_column = "Com_ID")
#> Warning in as.data.table.list(x, keep.rownames = keep.rownames, check.names
#> = check.names, : Item 2 has 56 rows but longest item has 500; recycled with
#> remainder.
#> Joining, by = "Com_ID"
print(graph)
#> # A tbl_graph: 145 nodes and 2593 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 2,593 x 17 (active)
#>    from    to weight Source Target com_ID_to com_ID_from Com_ID Com_ID_2_to
#>   <int> <int>  <dbl>  <int>  <int> <chr>     <chr>       <chr>  <chr>      
#> 1     4     5 0.146  214927 2.21e6 02        01          03     08         
#> 2     4    65 0.0408 214927 5.98e6 02        01          03     01         
#> 3     4    46 0.0973 214927 8.46e6 01        01          01     08         
#> 4     4     6 0.298  214927 1.07e7 01        01          01     08         
#> 5     4   113 0.0471 214927 1.60e7 02        01          03     04         
#> 6     4    62 0.0447 214927 1.62e7 02        01          03     01         
#> # ... with 2,587 more rows, and 8 more variables: Com_ID_2_from <chr>,
#> #   Com_ID_2 <chr>, Com_ID_3_to <chr>, Com_ID_3_from <chr>, Com_ID_3 <chr>,
#> #   color_com_ID_to <chr>, color_com_ID_from <chr>, color_edges <chr>
#> #
#> # Node Data: 145 x 11
#>   Id    Author  Year Author_date Title Journal Com_ID Size_com Com_ID_2 Com_ID_3
#>   <chr> <chr>  <int> <chr>       <chr> <chr>   <chr>     <dbl> <chr>    <chr>   
#> 1 9628~ ALBAN~  2003 ALBANESI-S~ Expe~ "The R~ 01        0.510 08       14      
#> 2 3709~ ATESO~  1980 ATESOGLU-H~ Infl~ "Journ~ 02        0.490 04       03      
#> 3 4628~ ATESO~  1982 ATESOGLU-H~ WAGE~ "JOURN~ 02        0.490 04       03      
#> # ... with 142 more rows, and 1 more variable: color <chr>
```

### Third step: giving a structure to your graph

You now have your network with your communities (and names and colors
for these communities). The next step is

``` r
# installing the Vite package for the Force Atlas layout
# devtools::install_github("ParkerICI/vite")
library(vite)

graph <- vite::complete_forceatlas2(graph, first.iter = 40000, overlap.method = "repel", overlap.iter = 1000, barnes.hut = TRUE, stopping.tolerance = 0.15)
#> First iteration
#> Using Barnes-Hut approximation
#> Stopping tolerance: 0.001000
#> Total number of iterations: 40000
#> Second iteration with prevent overalp
#> Using Barnes-Hut approximation
#> Stopping tolerance: 0.001000
#> Total number of iterations: 1000
```

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-traag2019" class="csl-entry">

Traag, Vincent A, Ludo Waltman, and Nees Jan van Eck. 2019. “From
Louvain to Leiden: Guaranteeing Well-Connected Communities.” *Scientific
Reports* 9 (1): 1–12.

</div>

</div>

[1] If two connected nodes are in the same community, their edge will
take the same color. If they are in different communities, their edge
will have a mix of the two communities colors.
