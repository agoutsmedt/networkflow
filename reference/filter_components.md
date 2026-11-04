# Keep the main component(s) of networks

**\[experimental\]**

A projected network is often made of one large connected component and
of small islands: a few articles that share references only with each
other, or isolated pairs. Community detection and layouts are hard to
read on such fragments, and many studies keep only the main component.
`filter_components()` numbers the (weakly) connected components by
decreasing size, keeps the `nb_components` largest, and warns when the
first component removed is not negligible.

## Usage

``` r
filter_components(
  graphs,
  nb_components = 1,
  threshold_alert = 0.05,
  keep_component_columns = FALSE
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s (e.g. the output of
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)).

- nb_components:

  Integer, the number of components to keep, starting from the largest
  (1 by default).

- threshold_alert:

  Number between 0 and 1. If the largest removed component gathers more
  than this share of the nodes (0.05 by default), a warning is given:
  removing it would change the object of study.

- keep_component_columns:

  Logical. If `TRUE`, keeps the columns `components_att` (component
  rank, 1 being the largest) and `size_components` (share of the nodes
  in the component).

## Value

The same object as `graphs` (a `tbl_graph` or a list of `tbl_graph`s)
restricted to the nodes of the kept components.

## Details

Dropping small components is a choice, not a technicality: the removed
documents may be a marginal literature, or a literature that cites
sources absent from the data. Look at what is removed before discarding
it, especially when the warning is triggered. For a list of graphs, the
filter is applied to each graph separately, so a node may be kept in one
window and removed in another.

## See also

[`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
and
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md),
which can call this function with `filter_components = TRUE`.

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
main_component <- filter_components(network, nb_components = 1)
c(before = igraph::vcount(network), after = igraph::vcount(main_component))
#> before  after 
#>    131    129 
```
