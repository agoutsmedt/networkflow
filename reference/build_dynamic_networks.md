# Build one network, or one network per time window, from bipartite links

**\[experimental\]**

The first step of the networkflow pipeline. Research data rarely come as
a network: they come as a table of links between two kinds of entities,
such as articles and the references they cite, authors and the papers
they sign, or accounts and the hashtags they use.
`build_dynamic_networks()` *projects* this two-mode (bipartite) relation
onto the first kind of entity: two `source_id` entities (e.g. two
articles) are linked when they share `target_id` entities (e.g.
references). With `time_variable` and `time_window`, it builds one such
network per time window, which is the starting point of a dynamic
analysis.

Write \\a\_{ij} = 1\\ when source \\i\\ is linked to target \\j\\. The
raw strength of the tie between sources \\i\\ and \\k\\ is the number of
targets they share, \$\$c\_{ik} = \sum_j a\_{ij} a\_{kj} = \|R_i \cap
R_k\|,\$\$ with \\R_i\\ the set of targets of \\i\\. When sources are
citing documents and targets their references, this is bibliographic
coupling (Kessler 1963) . Swapping the roles (cited documents as
`source_id`, citing documents as `target_id`) gives a co-citation
network (Small 1973) .

Raw counts favour sources with many targets (articles with long
bibliographies), and most pairs share a target or two by chance. The two
`projection_method`s answer this problem in different ways:

- `"structured"` normalises the counts with a bibliometric similarity
  measure computed by
  [biblionetwork](https://agoutsmedt.github.io/biblionetwork/) and keeps
  the pairs sharing at least `edges_threshold` targets;

- `"statistical"` keeps only the ties that are stronger than expected
  under a null model of random co-occurrence, using the
  [backbone](https://github.com/zpneal/backbone) package (Neal
  2014, 2022) .

## Usage

``` r
build_dynamic_networks(
  nodes,
  directed_edges,
  source_id,
  target_id,
  document_id = NULL,
  projection_on = c("source", "target"),
  projection_method = c("structured", "statistical"),
  cooccurrence_method = c("coupling_angle", "coupling_strength", "coupling_similarity"),
  edges_threshold = 1,
  model = c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
  alpha = NULL,
  backbone_args = list(),
  time_variable = NULL,
  time_in = c("nodes", "edges"),
  time_unit = NULL,
  time_window = NULL,
  window_step = NULL,
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
  [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork//reference/entity_network.html).
  Only for `projection_method = "structured"` with `"coupling_angle"` or
  `"coupling_strength"`.

- projection_on:

  `"source"` (default) links the `source_id` entities that share
  `target_id` entities (bibliographic coupling: articles sharing
  references). `"target"` links the `target_id` entities that share
  `source_id` entities (co-citation: cited documents linked when they
  are cited by the same documents). With `"target"`, `nodes` is keyed by
  `target_id`.

- projection_method:

  `"structured"` (default) or `"statistical"`, see the description and
  the details.

- cooccurrence_method:

  For `projection_method = "structured"`, the similarity measure used as
  edge weight: `"coupling_angle"`, `"coupling_strength"` or
  `"coupling_similarity"` (see the details). There is no default: the
  choice must be explicit.

- edges_threshold:

  For `projection_method = "structured"`, the minimum number of shared
  targets for two sources to be linked (1 by default).

- model:

  For `projection_method = "statistical"`, the null model of
  [`backbone::backbone_from_projection()`](https://rdrr.io/pkg/backbone/man/backbone_from_projection.html):
  `"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedrow"` or `"fixedcol"`.

- alpha:

  For `projection_method = "statistical"`, the significance level of the
  test of each tie. Lower values keep fewer edges.

- backbone_args:

  Optional list of further arguments for
  [`backbone::backbone_from_projection()`](https://rdrr.io/pkg/backbone/man/backbone_from_projection.html)
  (e.g. `mtc = "holm"` to correct for multiple tests). `alpha` or
  `model` given here override the arguments of the function.

- time_variable:

  Optional name of a column with the time of each node (or of each link,
  with `time_in = "edges"`): a number (e.g. a year), a `Date`, a
  `POSIXct`, or an ISO date written as text (`"2001-03"`,
  `"2001-03-15"`). Years written as text (`"2001"`) are numbers. Without
  it, a single network is built.

- time_in:

  `"nodes"` (default): `time_variable` is a column of `nodes` and a
  window selects the nodes of the period. `"edges"`: `time_variable` is
  a column of `directed_edges` (e.g. the year of the citing document)
  and a window selects the links of the period, the nodes being those
  with a link in the window. Use `"edges"` for co-citation, whose period
  is the one of the citing documents, and for entities without a date of
  their own (authors).

- time_unit:

  For dates only: the unit in which windows are counted, `"year"`
  (default), `"quarter"`, `"month"`, `"week"` (starting on Mondays) or
  `"day"`. For numbers, the unit is the one of the variable and
  `time_unit` must stay `NULL`.

- time_window:

  Optional whole number, the width of each window in `time_unit`s (or in
  units of the variable): 10 for ten years, 12 with
  `time_unit = "month"` for a year. `NULL` builds a single network.

- window_step:

  Optional whole number, the number of units between the starts of two
  windows. `NULL` (default) gives successive windows
  (`window_step = time_window`); `1` slides the window one unit at a
  time (1975-1984, 1976-1985...); `3` with `time_unit = "month"` and a
  window of 12 gives a yearly window every quarter.

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

  Arguments passed to
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
  (e.g. `nb_components`).

- verbose:

  Logical. If `TRUE` (default), reports the steps.

## Value

A `tbl_graph` (undirected) if `time_variable` is `NULL`; otherwise a
list of `tbl_graph`s named after their window (e.g. `"1975-1984"`), in
chronological order, whose nodes have a `time_window` column. Edges have
a `weight` column for the structured method.

## Details

**Similarity measures** (`projection_method = "structured"`). With \\L_i
= \|R_i\|\\ the number of targets of source \\i\\, \\N\\ the number of
sources and \\f_j\\ the number of sources linked to target \\j\\, the
three measures computed by biblionetwork are:

- `"coupling_angle"`, the cosine of Sen and Gan (1983) : \$\$w\_{ik} =
  \frac{\|R_i \cap R_k\|}{\sqrt{L_i L_k}};\$\$

- `"coupling_strength"` (Vladutz and Cook 1984; Shen et al. 2019) ,
  where a shared target counts less when it is linked to many sources,
  in the spirit of an inverse document frequency: \$\$w\_{ik} =
  \frac{\sum\_{j \in R_i \cap R_k} \log(N / f_j)}{L_i L_k};\$\$

- `"coupling_similarity"`, the idf-weighted cosine of Shen et al. (2019)
  : each source is described by a vector with one component per target,
  equal to \\\sqrt{\log(N / f_j)}\\, and the weight is the cosine of the
  vectors of two sources, with \\S_i = \sum\_{j \in R_i} \log(N / f_j)\\
  the weighted size of the bibliography: \$\$w\_{ik} = \frac{\sum\_{j
  \in R_i \cap R_k} \log(N / f_j)}{\sqrt{S_i S_k}}.\$\$ Like the
  coupling angle, it lies between 0 and 1; like the coupling strength,
  it discounts the targets linked to many sources. (Versions 0.1.1 and
  earlier of biblionetwork computed this measure wrongly: use
  biblionetwork 0.1.1.9000 or later.)

Targets linked to a single source cannot create a tie and are dropped
before the computation. The threshold applies to the raw count \\\|R_i
\cap R_k\|\\, not to the weight: `edges_threshold = 2` keeps the pairs
of articles that share at least two references, a common choice to
remove ties that one shared classic is enough to create. The measures
differ in how they treat very cited targets and long target lists; in
practice, clusters are often robust to the choice, but it is worth
checking on one's own data.

**Statistical backbones** (`projection_method = "statistical"`). Instead
of fixing a threshold, the backbone approach asks, for each pair,
whether the observed number of shared targets is larger than in random
bipartite networks that preserve some properties of the observed one:
the degrees of sources and targets on average (`"sdsm"`, the stochastic
degree sequence model) or exactly (`"fdsm"`), or only the total number
of links (`"fixedfill"`) (Neal 2014; Domagalski et al. 2021) . Edges are
kept when the test is significant at level `alpha`; the resulting
network is unweighted. This is slower, but each edge can be read as a
tie unlikely to be due to chance.

**Time windows.** Time is counted in whole units: the values of a
numeric variable, or years, quarters, months, weeks or days for dates. A
window starting at unit \\s\\ and \\w\\ units wide (`time_window`)
gathers the nodes (or links) with \\s \le t \< s + w\\, and the network
is computed as if the corpus were limited to them. A new window starts
every `window_step` units: successive windows (the default) give
independent snapshots, a step of 1 slides the window and smooths the
evolution of the networks, at the price of many networks sharing most of
their nodes. The last window may be shorter than the others when the
period is not a multiple of the step (a warning is given); windows are
labelled by their first and last unit (`"1975-1984"`,
`"2001-01 to 2001-12"`). The choice of the window is substantive: long
enough for each network to have enough nodes (a few dozen at least) to
show a structure, and short enough for changes to be visible. For
documents that follow each other quickly (central bank statements, news,
posts), months or weeks make sense, with a window wide enough to contain
enough documents.

**Beyond bibliographic coupling.** Nothing in the function is specific
to citations. The two sides of the links can be any two kinds of
entities:

- *Co-citation* (Small 1973) : with `projection_on = "target"`, cited
  documents are linked when the same documents cite them. Windows should
  follow the citing documents: put their year in `directed_edges` and
  use `time_in = "edges"`.

- *Author (or journal, institution) coupling* (Zhao and Strotmann 2008)
  : with `document_id`, authors are linked when the documents they sign
  cite the same references.

- *Co-authorship and other co-occurrences*: authors and the documents
  they sign, accounts and the hashtags they use, members and the boards
  they sit on: any two-mode table.

**Identifiers of references.** Every measure assumes that one work has
one identifier. With raw bibliographic data, the same reference is often
written in several ways (spelling, abbreviations, missing years), each
spelling becomes a different `target_id`, and two documents citing the
same work in different ways no longer share it. This matters most for
co-citation, where cited works are the nodes: a work split into several
spellings is split into several nodes, each with a fraction of its
citations. It matters less when the database gives an identifier to each
cited reference (e.g. the identifiers of Scopus, OpenAlex or Web of
Science), but even then references that the database could not match are
missing or duplicated. Clean and match references before building
networks, and check the most cited nodes for duplicates.

## References

Domagalski R, Neal ZP, Sagan B (2021). “Backbone: An R Package for
Extracting the Backbone of Bipartite Projections.” *PLOS ONE*,
**16**(1), e0244363.
[doi:10.1371/journal.pone.0244363](https://doi.org/10.1371/journal.pone.0244363)
.  
  
Kessler MM (1963). “Bibliographic Coupling between Scientific Papers.”
*American Documentation*, **14**(1), 10–25.
[doi:10.1002/asi.5090140103](https://doi.org/10.1002/asi.5090140103) .  
  
Neal Z (2014). “The Backbone of Bipartite Projections: Inferring
Relationships from Co-Authorship, Co-Sponsorship, Co-Attendance and
Other Co-Behaviors.” *Social Networks*, **39**, 84–97.
[doi:10.1016/j.socnet.2014.06.001](https://doi.org/10.1016/j.socnet.2014.06.001)
.  
  
Neal ZP (2022). “backbone: An R Package to Extract Network Backbones.”
*PLOS ONE*, **17**(5), e0269137.
[doi:10.1371/journal.pone.0269137](https://doi.org/10.1371/journal.pone.0269137)
.  
  
Sen SK, Gan SK (1983). “A Mathematical Extension of the Idea of
Bibliographic Coupling and Its Applications.” *Annals of Library Science
and Documentation*, **30**(2), 78–82.  
  
Shen S, Zhu D, Rousseau R, Su X, Wang D (2019). “A Refined Method for
Computing Bibliographic Coupling Strengths.” *Journal of Informetrics*,
**13**(2), 605–615.
[doi:10.1016/j.joi.2019.01.012](https://doi.org/10.1016/j.joi.2019.01.012)
.  
  
Small H (1973). “Co-Citation in the Scientific Literature: A New Measure
of the Relationship between Two Documents.” *Journal of the American
Society for Information Science*, **24**(4), 265–269.
[doi:10.1002/asi.4630240406](https://doi.org/10.1002/asi.4630240406) .  
  
Vladutz G, Cook J (1984). “Bibliographic Coupling and Subject
Relatedness.” *Proceedings of the American Society for Information
Science*, **21**, 204–207.  
  
Zhao D, Strotmann A (2008). “Author Bibliographic Coupling: Another
Approach to Citation-Based Author Knowledge Network Analysis.” In
*Proceedings of the American Society for Information Science and
Technology*, volume 45(1), 1–10.
<https://asistdl.onlinelibrary.wiley.com/doi/full/10.1002/meet.2008.1450450292>.

## See also

[`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
for a single network;
[`biblionetwork::coupling_angle()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_angle.html),
[`biblionetwork::coupling_strength()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_strength.html),
[`biblionetwork::coupling_similarity()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_similarity.html),
[`backbone::backbone_from_projection()`](https://rdrr.io/pkg/backbone/man/backbone_from_projection.html).
The next steps of the pipeline are
[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
and
[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).

## Examples

``` r
nodes <- subset(Nodes_stagflation, source_type == "Stagflation")

# Bibliographic coupling networks of the articles explaining the 1970s
# stagflation, one per decade
networks <- build_dynamic_networks(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 13,
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  verbose = FALSE
)
names(networks)
#> [1] "1975-1987" "1988-2000" "2001-2013"
networks[[1]]
#> # A tbl_graph: 67 nodes and 405 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 67 × 8 (active)
#>    source_id source_author  source_year source_label source_title source_journal
#>    <chr>     <chr>          <chr>       <chr>        <chr>        <chr>         
#>  1 37095547  ATESOGLU-H     1980        ATESOGLU-H-… INFLATION A… "JOURNAL OF P…
#>  2 46282251  ATESOGLU-H     1982        ATESOGLU-H-… WAGES AND S… "JOURNAL OF P…
#>  3 42623802  BLAAS-W        1982        BLAAS-W-1982 INSTITUTION… "JOURNAL OF E…
#>  4 5240516   BLINDER-A      1979        BLINDER-A-1… ECONOMIC PO… ""            
#>  5 25091445  BLINDER-A      1981        BLINDER-A-1… THE 1971-19… ""            
#>  6 31895841  BLINDER-A      1982        BLINDER-A-1… THE ANATOMY… "INFLATION: C…
#>  7 1172921   BRANSON-W      1980        BRANSON-W-1… INTERNATION… ""            
#>  8 47749045  BRONFENBRENNE… 1976        BRONFENBREN… ELEMENTS OF… "ZEITSCHRIFT …
#>  9 14490177  BRUNNER-K      1980        BRUNNER-K-1… STAGFLATION… "JOURNAL OF M…
#> 10 6119013   BRUNO-M        1979        BRUNO-M-197… MACRO-ECONO… ""            
#> # ℹ 57 more rows
#> # ℹ 2 more variables: source_type <chr>, time_window <chr>
#> #
#> # Edge Data: 405 × 5
#>    from    to weight Source   Target    
#>   <int> <int>  <dbl> <chr>    <chr>     
#> 1    14    21 0.116  1021902  1111111122
#> 2    15    21 0.0259 1021902  1111111128
#> 3    15    22 0.0681 10585974 1111111128
#> # ℹ 402 more rows

# Co-citation: cited documents linked when the same documents cite them,
# with windows on the year of the citing documents
citations <- merge(
  transform(Ref_stagflation[, c("source_id", "target_id")],
            source_id = as.character(source_id), target_id = as.character(target_id)),
  nodes[, c("source_id", "source_year")],
  by = "source_id"
)
cited <- Ref_stagflation[!duplicated(Ref_stagflation$target_id),
                         c("target_id", "target_author", "target_year")]
cocitation <- build_dynamic_networks(
  nodes = transform(cited, target_id = as.character(target_id)),
  directed_edges = citations,
  source_id = "source_id",
  target_id = "target_id",
  projection_on = "target",
  time_in = "edges",
  time_variable = "source_year",
  time_window = 13,
  cooccurrence_method = "coupling_angle",
  edges_threshold = 2,
  compute_size = TRUE,
  verbose = FALSE
)
cocitation[[1]]
#> # A tbl_graph: 122 nodes and 354 edges
#> #
#> # An undirected simple graph with 3 components
#> #
#> # Node Data: 122 × 5 (active)
#>    target_id  target_author                   target_year time_window node_size
#>    <chr>      <chr>                                 <int> <chr>           <int>
#>  1 1021902    FRIEDMAN, M.                           1977 1975-1987           4
#>  2 10585974   FRYE-J                                 1980 1975-1987           2
#>  3 1059321    TOBIN-J                                1972 1975-1987           5
#>  4 10990368   PERRY-G                                1970 1975-1987           3
#>  5 1111111171 SOLOW-R                                1980 1975-1987           2
#>  6 1111111317 HAVRILESKY-T                           1975 1975-1987           2
#>  7 1111111343 PERLOFF, J.M. AND WACHTER, W.H.        1979 1975-1987           5
#>  8 1111111507 DENISON E.                             1978 1975-1987           3
#>  9 1111111520 RASCHE-R                               1977 1975-1987           4
#> 10 1111111579 HAHN-F                                 1974 1975-1987           2
#> # ℹ 112 more rows
#> #
#> # Edge Data: 354 × 5
#>    from    to weight Source     Target    
#>   <int> <int>  <dbl> <chr>      <chr>     
#> 1     1     3  0.447 1021902    1059321   
#> 2     3     4  0.516 1059321    10990368  
#> 3     6     7  0.632 1111111317 1111111343
#> # ℹ 351 more rows

# Authors linked by the references of the documents they sign
by_author <- merge(
  transform(Authors_stagflation, source_id = as.character(source_id)),
  citations, by = "source_id"
)
author_coupling <- build_network(
  nodes = data.frame(author_name = unique(by_author$author_name)),
  directed_edges = by_author,
  source_id = "author_name",
  target_id = "target_id",
  document_id = "source_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_strength",
  edges_threshold = 2
)
author_coupling
#> # A tbl_graph: 120 nodes and 2365 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 120 × 1 (active)
#>    author_name 
#>    <chr>       
#>  1 ROMER-C     
#>  2 ROMER-D     
#>  3 ORPHANIDES-A
#>  4 FRIEDMAN-M  
#>  5 KILIAN-L    
#>  6 BARSKY-R    
#>  7 BLANCHARD-O 
#>  8 FRYE-J      
#>  9 GORDON-R    
#> 10 SARGENT-T   
#> # ℹ 110 more rows
#> #
#> # Edge Data: 2,365 × 6
#>    from    to  weight Source     Target     Weighting_method
#>   <int> <int>   <dbl> <chr>      <chr>      <chr>           
#> 1    16   119 0.00355 ALBANESI-S BALL-L     strength        
#> 2     6   119 0.00267 ALBANESI-S BARSKY-R   strength        
#> 3    78   119 0.00548 ALBANESI-S BILBIIE-FO strength        
#> # ℹ 2,362 more rows

# \donttest{
# Statistical backbone: keep the ties that random networks with the same
# degrees would rarely produce
if (requireNamespace("backbone", quietly = TRUE)) {
backbones <- build_dynamic_networks(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 20,
  projection_method = "statistical",
  model = "sdsm",
  alpha = 0.05,
  backbone_args = list(mtc = "holm"),
  verbose = FALSE
)
}
#> Warning: The last window is shorter than the others.
#> ℹ The data end in "2013", but the last window ends in "2014".
# }
```
