# Build a single network from bipartite links

**\[experimental\]**

`build_network()` projects a two-mode relation (e.g. articles and the
references they cite) onto a one-mode network of the source entities,
for the whole corpus at once. It is
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
without time windows: see that function for the measures, the thresholds
and the literature behind them.

## Usage

``` r
build_network(
  nodes,
  directed_edges,
  source_id,
  target_id,
  document_id = NULL,
  projection_on = c("source", "target"),
  method,
  edges_threshold = 1,
  alpha = NULL,
  backbone_args = list(),
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...,
  verbose = TRUE
)
```

## Arguments

- nodes:

  A data frame with one row per source entity (e.g. one row per article)
  and its metadata (authors, year, title...). Not modified.

- directed_edges:

  A data frame of bipartite links, one row per `source_id` -\>
  `target_id` link (e.g. article -\> cited reference). Duplicated links
  are counted once. Not modified.

- source_id:

  Character scalar, the column identifying the entities that become the
  nodes of the network. It must exist in both `nodes` and
  `directed_edges`, with one row per value in `nodes`.

- target_id:

  Character scalar, the column of `directed_edges` with the entities
  that create the links (e.g. references).

- document_id:

  Optional column of `directed_edges` identifying the document through
  which each link exists. With it, `source_id` entities are not
  documents but *entities that publish documents* (authors, journals,
  institutions) and a reference cited by several documents of an entity
  counts several times, following
  [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork/reference/entity_network.html).
  Only for the similarity measures.

- projection_on:

  `"source"` (default) links the `source_id` entities that share
  `target_id` entities (bibliographic coupling: articles sharing
  references). `"target"` links the `target_id` entities that share
  `source_id` entities (co-citation: cited documents linked when they
  are cited by the same documents). With `"target"`, `nodes` is keyed by
  `target_id`.

- method:

  Required, no default. How the ties between sources are built; the
  origin of each method is given in the details. One of

  - the similarity measures of
    [biblionetwork](https://agoutsmedt.github.io/biblionetwork/)
    (`biblionetwork::biblio_coupling(method = )`, or
    [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork/reference/entity_network.html)
    with `document_id`): `"angle"`, `"strength"`, `"similarity"`,
    `"jaccard"`, `"vladutz"`;

  - the null models of the statistical backbones of
    [`backbone::backbone_from_projection()`](https://rdrr.io/pkg/backbone/man/backbone_from_projection.html):
    `"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedrow"`, `"fixedcol"`. They
    need `alpha`.

- edges_threshold:

  For the similarity measures, the minimum number of shared targets for
  two sources to be linked (1 by default). Ignored by the backbone
  methods.

- alpha:

  For the backbone methods, the significance level of the test of each
  tie (e.g. 0.05). Lower values keep fewer edges. Required for them,
  ignored by the similarity measures.

- backbone_args:

  Optional list of further arguments for
  [`backbone::backbone_from_projection()`](https://rdrr.io/pkg/backbone/man/backbone_from_projection.html)
  (e.g. `mtc = "holm"` to correct for multiple tests). `alpha` given
  here overrides the argument of the function.

- compute_size:

  Logical. If `TRUE`, adds a `node_size` column: the number of times
  each node is a target of the sources of the same window (for a
  coupling network, the number of citations received within the corpus).
  It requires `nodes` to contain the `target_id` column.

- keep_singleton:

  Logical. If `FALSE` (default), nodes without any edge are removed.

- filter_components:

  Logical. If `TRUE`, keeps only the main component(s) with
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

- ...:

  With `filter_components = TRUE`, arguments of
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
  (e.g. `nb_components`).

- verbose:

  Logical. If `TRUE` (default), reports the steps.

## Value

A `tbl_graph`, undirected, with the columns of `nodes` as node
attributes and, for the structured method, a `weight` edge column.

## Details

The cases (which arguments for which kind of network), the methods and
where they come from are described in the Details of
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md).
With `verbose = TRUE` (default), the function says which method it
applies.

## See also

[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md);
[`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md),
[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
for the next steps.

## Examples

``` r
nodes <- subset(Nodes_stagflation, source_type == "Stagflation")

network <- build_network(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 2
)
#> ℹ Coupling (`biblionetwork::biblio_coupling()`) with "similarity": ties
#>   weighted by the idf-weighted cosine of the two lists of targets, after Shen
#>   et al. (2019), between source_id entities sharing target_id entities.
#> ℹ Two source_id are linked when they share at least 2 target_id
#>   (`edges_threshold`).
#> ℹ Nodes without any edge are removed (`keep_singleton = FALSE`).
network
#> # A tbl_graph: 131 nodes and 1501 edges
#> #
#> # An undirected simple graph with 2 components
#> #
#> # Node Data: 131 × 7 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>         <chr>       <chr>         <chr>        <chr>         
#>  1 96284971  ALBANESI-S    2003        ALBANESI-S-2… EXPECTATION… "THE REVIEW O…
#>  2 37095547  ATESOGLU-H    1980        ATESOGLU-H-1… INFLATION A… "JOURNAL OF P…
#>  3 46282251  ATESOGLU-H    1982        ATESOGLU-H-1… WAGES AND S… "JOURNAL OF P…
#>  4 214927    BALL-L        1991        BALL-L-1991   THE GENESIS… "JOURNAL OF M…
#>  5 2207578   BALL-L        1995        BALL-L-1995a  RELATIVE-PR… "THE QUARTERL…
#>  6 10729971  BALL-L        1995        BALL-L-1995b  TIME-CONSIS… "JOURNAL OF M…
#>  7 95575910  BARSKY-R      2002        BARSKY-R-2002 DO WE REALL… "NBER MACROEC…
#>  8 105203318 BARSKY-R      2004        BARSKY-R-2004 OIL AND THE… "JOURNAL OF E…
#>  9 140987361 BENATI-L      2011        BENATI-L-2011 WOULD THE B… "JOURNAL OF E…
#> 10 43590636  BERNANKE-B    1997        BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#> # ℹ 121 more rows
#> # ℹ 1 more variable: source_type <chr>
#> #
#> # Edge Data: 1,501 × 5
#>    from    to weight Source    Target   
#>   <int> <int>  <dbl> <chr>     <chr>    
#> 1   110   111 0.122  101742279 101742296
#> 2    98   111 0.0288 101742279 101777013
#> 3    98   110 0.0851 101742296 101777013
#> # ℹ 1,498 more rows
```
