# Creating Dynamic Networks from a List of Nodes and Directed Edges

**\[deprecated\]**

This function was implemented in an earlier version of the development
of this package. It has been replaced by
[`networkflow::build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md).
This function creates one or several tidygraph networks from a table of
nodes and its directed edges. For instance, for bibliometric networks,
you can give a list of articles and the list of the references these
articles cite. You can use it to build a single network or multiple
networks over different time windows.

## Usage

``` r
dynamic_network_cooccurrence(
  nodes = NULL,
  directed_edges = NULL,
  source_column = NULL,
  target_column = NULL,
  time_variable = NULL,
  time_window = NULL,
  cooccurrence_method = c("coupling_angle", "coupling_strength", "coupling_similarity"),
  overlapping_window = FALSE,
  edges_threshold = 1,
  compute_size = FALSE,
  keep_singleton = FALSE,
  verbose = TRUE
)
```

## Arguments

- nodes:

  The table with all the nodes and their metadata. For instance, if your
  nodes are articles, this table is likely to contain the year of
  publication, the name of the authors, the title of the article, etc...
  The table must have one row per node.

- directed_edges:

  The table with of all the elements to which your nodes are connected.
  If your nodes are articles, the `directed_edges` table can contain the
  list of the references cited by these articles, the authors that have
  written these articles, or the affiliations of the authors of these
  articles.

- source_column:

  The column with the unique identifier of each node. For instance, for
  a bibliographic coupling network, the id of your citing documents.

- target_column:

  The column with the unique identifier of each element connected to the
  node (for instance, the identifier of the reference cited by your node
  if the node is an article).

- time_variable:

  The column with the temporal variable you want to use to build your
  windows for the succession of networks. By default, `time_variable` is
  `NULL` and the function will only build one network without taking
  into account any temporal variable.

- time_window:

  The length of your network relatively of the unity of the
  `time_variable` column. If you use a variable in years as
  `time_variable` and you set `time_window` at 5, the function will
  build network on five year windows. By default, `time_window` is
  `NULL` and the function will only build one network.

- cooccurrence_method:

  Choose a cooccurrence method to build your indirect edges table. The
  function propose three methods that depends on the [biblionetwork
  package](https://agoutsmedt.github.io/biblionetwork/) and three
  methods that are implemented in it:

  - the coupling angle measure (see
    [`biblionetwork::biblio_coupling()`](https://agoutsmedt.github.io/biblionetwork//reference/biblio_coupling.html)
    for documentation);

  - the coupling strength measure
    ([`biblionetwork::coupling_strength()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_strength.html));

  - the coupling similarity measure
    ([`biblionetwork:: coupling_similarity()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_similarity.html)).

- overlapping_window:

  Set to `FALSE` by default. If set to `TRUE`, and if `time_variable`
  and `time_window` not `NULL`, the function will create a succession of
  networks for moving time windows. The windows are moving one unit per
  one unit of the `time_variable`. For instance, for years, if
  `time_window` set to 5, it creates networks for successive time
  windows like 1970-1974, 1971-1975, 1972-1976, etc.

- edges_threshold:

  Threshold value for building your edges. With a higher threshold, only
  the stronger links will be kept. See the [biblionetwork
  package](https://agoutsmedt.github.io/biblionetwork/) documentation
  and the `cooccurrence_method` parameter.

- compute_size:

  Set to `FALSE` by default. If `TRUE`, the function uses the
  `directed_edges` data to calculate how many directed edges a node
  receives (as a target). If `directed_edges` is a table of direct
  citations, the functions calculates the number of time a node is cited
  by the other nodes. You need to have the `target_column` in the
  `nodes` table to make the link with the targetted nodes in the
  `directed_edges` table.

- keep_singleton:

  Set to `FALSE` by default. If `TRUE`, the function removes the nodes
  that have no undirected edges, i.e. no cooccurrence with any other
  nodes. In graphical terms, these nodes are alone in the network, with
  no link with other nodes.

- verbose:

  Set to `FALSE` if you don't want the function to display different
  sort of information.

## Value

If `time_window` is `NULL`, the function computes only one network and
return a tidygraph object built with
[tbl_graph()](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).
If `time_variable` and `time_window` are not `NULL`, the function
returns a list of tidygraph networks, for each time window.

## Examples

``` r
nodes <- networkflow::Nodes_stagflation |>
dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

temporal_networks <- dynamic_network_cooccurrence(nodes = nodes,
directed_edges = references,
source_column = "source_id",
target_column = "target_id",
time_variable = "source_year",
cooccurrence_method = "coupling_similarity",
time_window = NULL,
edges_threshold = 1,
compute_size = FALSE,
keep_singleton = FALSE,
overlapping_window = TRUE)
#> Warning: `dynamic_network_cooccurrence()` was deprecated in networkflow 0.1.0.
#> ℹ Please use `build_dynamic_networks()` instead.
#> Creation of the network for the 1975-2013 window.
#> We remove the nodes that are alone with no edge. 
#> 
```
