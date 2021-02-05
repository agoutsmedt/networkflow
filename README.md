
-   [networkflow](#networkflow)
    -   [Installation](#installation)
    -   [The workflow for networks](#the-workflow-for-networks)

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
