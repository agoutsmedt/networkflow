
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

You can cite this package as:

``` r
citation("networkflow")
#> 
#> Pour citer le package 'networkflow' dans une publication, utilisez :
#> 
#>   Goutsmedt A, Truc A (2022). _networkflow: Functions For A Workflow To
#>   Manipulate Networks_. https://github.com/agoutsmedt/networkflow,
#>   https://agoutsmedt.github.io/networkflow/.
#> 
#> Une entrée BibTeX pour les utilisateurs LaTeX est
#> 
#>   @Manual{,
#>     title = {networkflow: Functions For A Workflow To Manipulate Networks},
#>     author = {Aurélien Goutsmedt and Alexandre Truc},
#>     year = {2022},
#>     note = {https://github.com/agoutsmedt/networkflow,
#> https://agoutsmedt.github.io/networkflow/},
#>   }
```

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agoutsmedt/networkflow")
```

## Example

See `vignette(workflow-network)` and
`vignette(exploring_dynamic_networks)` for more explanations on the
package.
