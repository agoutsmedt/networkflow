
<!-- README.md is generated from README.Rmd. Please edit that file -->

# networkflow

<!-- badges: start -->
<!-- badges: end -->

The goal of networkflow (a workflow for networks) is to propose a series
of functions to make it easier and quicker to manipulats networks. It
mainly targets working on bibliometric networks (see the
[biblionetwork](https://github.com/agoutsmedt/biblionetwork) package for
creating such networks). This package heavily relies on
[igraph](https://igraph.org/r/) and
[tidygraph](https://tidygraph.data-imaginist.com/index.html), and aims
at producing ready-made networks for projecting them using
[ggraph](https://ggraph.data-imaginist.com/). This package aims at
helping the users to follow more quickly and easily the main steps of
network manipulation, from creating the graph, through detecting
clusters, to projecting it. Please see `vignette("workflow-network")`
for details on the workflow for dealing with a unique network.

Networkflow also proposes a worfklow to deal with a list of networks, in
order to develop a dynamic analysis. It implements a method to merge
clusters across successive networks, to identify inter-temporal
clusters. It also develops corresponding visualisations to display the
evolution of clusters across networks.
`vignette("exploring_dynamic_networks")` gives an example of the
workflow for dynamic networks. You can also find illustrations for this
method in [“An Independent European Macroeconomics? A History of
European Macroeconomics through the Lens of the European Economic
Review](https://aurelien-goutsmedt.com/publication/eer-history/).

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
install.packages("devtools")
devtools::install_github("agoutsmedt/networkflow")
```
