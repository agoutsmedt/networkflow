# Follow clusters across time windows

**\[experimental\]**

When clusters are detected in each time window separately, their
identifiers mean nothing from one window to the next: cluster "01" of
the 1975-1984 network and cluster "01" of the 1976-1985 network may be
two different literatures. `merge_dynamic_clusters()` links the clusters
of successive windows that share most of their nodes, and gives them a
common identifier, so that a cluster becomes an entity that can be born,
persist and disappear. This is the step that turns a series of networks
into a history of research communities, in the spirit of the alluvial
maps of Rosvall and Bergstrom (2010) .

Two rules are available, chosen with `method`. Let \\V_a\\ be the nodes
of cluster \\a\\ in window \\t-1\\ and \\V_b\\ those of cluster \\b\\ in
window \\t\\.

- `method = "majority"`, a **double majority**. Cluster \\b\\ inherits
  the identifier of \\a\\ when most nodes of \\a\\ go to \\b\\ *and*
  most nodes of \\b\\ come from \\a\\: \$\$\frac{\|V_a \cap V_b\|}{D_a}
  \> \tau \quad \text{and} \quad \frac{\|V_a \cap V_b\|}{D_b} \>
  \tau,\$\$ with \\\tau\\ the `threshold_similarity`, strictly above
  0.5. Since \\\tau \> 0.5\\, a cluster can inherit at most one
  identifier, and an identifier passes to at most one cluster.

- `method = "jaccard"`, an **overlap matching with a Jaccard
  threshold**, as in the tracking of dynamic communities of Greene et
  al. (2010) . The Jaccard index of two clusters is the share of their
  union that they have in common, \$\$J(a, b) = \frac{\|V_a \cap
  V_b\|}{\|V_a \cup V_b\|}.\$\$ The pairs with \\J(a, b) \geq \theta\\,
  \\\theta\\ being the `threshold_similarity`, are candidate matches.
  They are taken one by one, by decreasing \\J\\ (then by decreasing
  number of shared nodes), skipping the pairs where \\a\\ or \\b\\ has
  already been matched: each past identifier goes to the present cluster
  that resembles it most, and each cluster inherits at most one
  identifier. \\\theta\\ can be below 0.5: a cluster that loses a large
  part of its members to another cluster, or that doubles with
  newcomers, can keep its identity if it stays the best match of its
  predecessor.

In both rules, a cluster that meets no match gets a new identifier. The
sets \\V\\ depend on `similarity_type`: with `"complete"`, they are all
the nodes of the clusters (\\D_a = \|V_a\|\\, \\D_b = \|V_b\|\\); with
`"partial"`, only the nodes present in both windows count (\\D_a = \|V_a
\cap V_t\|\\, \\D_b = \|V_b \cap V\_{t-1}\|\\, and the union of the
Jaccard index is restricted to them), \\V_t\\ being all the nodes of
window \\t\\.

There is no default `method`, `threshold_similarity` or
`similarity_type`: they change the dynamic clusters, so the choice must
be explicit, and `verbose = TRUE` reports the rule that is applied.

## Usage

``` r
merge_dynamic_clusters(
  list_graph,
  cluster_id,
  node_id,
  method = c("majority", "jaccard"),
  threshold_similarity,
  similarity_type = c("complete", "partial"),
  verbose = TRUE
)
```

## Arguments

- list_graph:

  A list of at least two `tbl_graph`s in chronological order (oldest
  first), as produced by
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  and
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md).

- cluster_id:

  Character scalar, the node column with the clusters of each window
  (e.g. `"cluster_leiden"`).

- node_id:

  Character scalar, the node column with an identifier that is stable
  across windows (e.g. `"source_id"`).

- method:

  `"majority"` (double majority) or `"jaccard"` (overlap matching with a
  Jaccard threshold), see the description. Required.

- threshold_similarity:

  The threshold of the rule. For `method = "majority"`, \\\tau\\ in
  \\(0.5, 1\]\\: a cluster is linked when the share is *above* it. For
  `method = "jaccard"`, \\\theta\\ in \\(0, 1\]\\: two clusters can be
  linked when their Jaccard index is *at least* it. Higher values
  require more similar clusters and create more distinct dynamic
  clusters. Required.

- similarity_type:

  `"complete"` or `"partial"`, see the description. Required.

- verbose:

  Logical. If `TRUE` (default), says which rule is applied.

## Value

The list of graphs, with the same names, where nodes have a new column
`dynamic_{cluster_id}` (e.g. `dynamic_cluster_leiden`, with identifiers
"cl_1", "cl_2"...) and edges the columns `dynamic_{cluster_id}_from`,
`dynamic_{cluster_id}_to` and `dynamic_{cluster_id}` (missing for edges
between clusters).

## Details

**Which windows?** The rules need nodes present in successive windows.
In a network of documents (e.g. bibliographic coupling), each document
has a single date, so windows that do not overlap (`window_step` equal
to `time_window`, the default of
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md))
share no node and no cluster can be matched: the windows must overlap
(`window_step` smaller than `time_window`). Non-overlapping windows work
when nodes persist over time, as cited references (co-citation), authors
or journals do; two windows may still share no node when the literature
changed completely.

**Complete or partial?** With overlapping windows, the network of window
\\t\\ loses the documents of the first years of window \\t-1\\ and gains
those of new years; in a growing literature, newcomers can be numerous.
With `"complete"`, a cluster that keeps all its past members but doubles
with newcomers cannot inherit the identifier under the double majority,
because its members coming from \\a\\ are less than half of it: the
cluster is considered new because its composition changed. With
`"partial"`, the newcomers are ignored and the same cluster keeps its
identifier. Which one is right depends on what a cluster is taken to be:
a group of documents (complete), or a structure that recruits new
documents (partial).

**Majority or Jaccard?** The double majority is demanding and
transparent: two clusters are the same if each is mostly made of the
other, and the identifier is never ambiguous. The Jaccard matching is
more flexible: with a low \\\theta\\, it follows a cluster that splits
(the largest piece, the best match, keeps the identifier) or that merges
with another. Both use the nodes of consecutive windows only; compare
them with
[`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md).

**What the rules do not do.** Clusters are compared only with the
previous window: a cluster that vanishes for one window and comes back
gets a new identifier. Splits and merges are not recorded as such: one
part keeps the identifier, the others are new. With a threshold close to
its minimum, small fluctuations of the partition (a stochastic
algorithm, a resolution change) may break or create continuities; it is
good practice to compare the results for a few thresholds and seeds.

## References

Greene D, Doyle D, Cunningham P (2010). “Tracking the Evolution of
Communities in Dynamic Social Networks.” In *2010 International
Conference on Advances in Social Networks Analysis and Mining*, 176–183.
[doi:10.1109/ASONAM.2010.17](https://doi.org/10.1109/ASONAM.2010.17) .  
  
Rosvall M, Bergstrom CT (2010). “Mapping Change in Large Networks.”
*PLoS ONE*, **5**(1), e8694.
[doi:10.1371/journal.pone.0008694](https://doi.org/10.1371/journal.pone.0008694)
.

## See also

[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
with `name_dynamic_clusters = TRUE` to label the dynamic clusters;
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)
and
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
to draw their evolution.

## Examples

``` r
networks <- build_dynamic_networks(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 10,
  method = "similarity",
  edges_threshold = 1,
  verbose = FALSE
)
#> Warning: The last window is shorter than the others.
#> ℹ The data end in "2013", but the last window ends in "2014".
networks <- add_clusters(
  networks,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

networks <- merge_dynamic_clusters(
  networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  method = "majority",
  threshold_similarity = 0.51,
  similarity_type = "partial"
)
#> ℹ Linking clusters across windows with the "majority" rule: a cluster inherits
#>   the identifier of a past cluster when more than 0.51 of the nodes of each go
#>   to / come from the other (double majority). The comparison counts the nodes
#>   present in both windows only (`similarity_type` = "partial").
#> Warning: 3 pairs of consecutive windows share no node: their clusters cannot be matched.
#> ℹ Clusters are matched through the nodes they share. In a network of documents
#>   (coupling), each node has one date: the windows must overlap (`window_step`
#>   smaller than `time_window` in `build_dynamic_networks()`). If windows already
#>   overlap, the nodes may have changed completely, or the threshold may leave a
#>   window without edges.
networks[[2]]
#> # A tbl_graph: 7 nodes and 6 edges
#> #
#> # An unrooted tree
#> #
#> # Node Data: 7 × 11 (active)
#>   source_id  source_author source_year source_label  source_title source_journal
#>   <chr>      <chr>         <chr>       <chr>         <chr>        <chr>         
#> 1 214927     BALL-L        1991        BALL-L-1991   THE GENESIS… "JOURNAL OF M…
#> 2 1111111134 FISCHER-S     1985        FISCHER-S-19… SUPPLY SHOC… "JOURNAL OF M…
#> 3 1184130    GISSER-M      1986        GISSER-M-1986 CRUDE OIL A… "JOURNAL OF M…
#> 4 25481516   HAKES-DR      1988        HAKES-DR-1988 EVIDENCE OF… "REVIEW OF RA…
#> 5 6714008    HELLIWELL-J   1988        HELLIWELL-J-… COMPARATIVE… "JOURNAL OF E…
#> 6 19627977   PARKIN-M      1993        PARKIN-M-1993 INFLATION I… "PRICE STABIL…
#> 7 1111111176 TAYLOR-J      1992        TAYLOR-J-1992 THE GREAT I… "RESERVE BANK…
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 6 × 11
#>    from    to weight Source     Target   cluster_leiden_from cluster_leiden_to
#>   <int> <int>  <dbl> <chr>      <chr>    <chr>               <chr>            
#> 1     6     7 0.0220 1111111176 19627977 01                  01               
#> 2     3     6 0.0286 1184130    19627977 01                  01               
#> 3     1     6 0.0807 19627977   214927   01                  01               
#> # ℹ 3 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>

# The same windows, with the Jaccard matching
merge_dynamic_clusters(
  networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  method = "jaccard",
  threshold_similarity = 0.3,
  similarity_type = "partial"
)
#> ℹ Linking clusters across windows with the "jaccard" rule: a cluster inherits
#>   the identifier of the past cluster with which its Jaccard index (shared nodes
#>   over the union of the two clusters) is the highest, if it is at least 0.3;
#>   each identifier passes to one cluster at most. The comparison counts the
#>   nodes present in both windows only (`similarity_type` = "partial").
#> ℹ The existing dynamic_cluster_leiden columns are replaced.
#> Warning: 3 pairs of consecutive windows share no node: their clusters cannot be matched.
#> ℹ Clusters are matched through the nodes they share. In a network of documents
#>   (coupling), each node has one date: the windows must overlap (`window_step`
#>   smaller than `time_window` in `build_dynamic_networks()`). If windows already
#>   overlap, the nodes may have changed completely, or the threshold may leave a
#>   window without edges.
#> $`1975-1984`
#> # A tbl_graph: 64 nodes and 375 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 64 × 11 (active)
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
#> # ℹ 54 more rows
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 375 × 11
#>    from    to weight Source   Target     cluster_leiden_from cluster_leiden_to
#>   <int> <int>  <dbl> <chr>    <chr>      <chr>               <chr>            
#> 1    14    20 0.115  1021902  1111111122 03                  03               
#> 2    15    20 0.0257 1021902  1111111128 01                  03               
#> 3    15    21 0.0678 10585974 1111111128 01                  02               
#> # ℹ 372 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
#> 
#> $`1985-1994`
#> # A tbl_graph: 7 nodes and 6 edges
#> #
#> # An unrooted tree
#> #
#> # Node Data: 7 × 11 (active)
#>   source_id  source_author source_year source_label  source_title source_journal
#>   <chr>      <chr>         <chr>       <chr>         <chr>        <chr>         
#> 1 214927     BALL-L        1991        BALL-L-1991   THE GENESIS… "JOURNAL OF M…
#> 2 1111111134 FISCHER-S     1985        FISCHER-S-19… SUPPLY SHOC… "JOURNAL OF M…
#> 3 1184130    GISSER-M      1986        GISSER-M-1986 CRUDE OIL A… "JOURNAL OF M…
#> 4 25481516   HAKES-DR      1988        HAKES-DR-1988 EVIDENCE OF… "REVIEW OF RA…
#> 5 6714008    HELLIWELL-J   1988        HELLIWELL-J-… COMPARATIVE… "JOURNAL OF E…
#> 6 19627977   PARKIN-M      1993        PARKIN-M-1993 INFLATION I… "PRICE STABIL…
#> 7 1111111176 TAYLOR-J      1992        TAYLOR-J-1992 THE GREAT I… "RESERVE BANK…
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 6 × 11
#>    from    to weight Source     Target   cluster_leiden_from cluster_leiden_to
#>   <int> <int>  <dbl> <chr>      <chr>    <chr>               <chr>            
#> 1     6     7 0.0220 1111111176 19627977 01                  01               
#> 2     3     6 0.0286 1184130    19627977 01                  01               
#> 3     1     6 0.0807 19627977   214927   01                  01               
#> # ℹ 3 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
#> 
#> $`1995-2004`
#> # A tbl_graph: 36 nodes and 379 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 36 × 11 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>         <chr>       <chr>        <chr>        <chr>         
#>  1 96284971   ALBANESI-S    2003        ALBANESI-S-… EXPECTATION… THE REVIEW OF…
#>  2 2207578    BALL-L        1995        BALL-L-1995a RELATIVE-PR… THE QUARTERLY…
#>  3 10729971   BALL-L        1995        BALL-L-1995b TIME-CONSIS… JOURNAL OF MO…
#>  4 95575910   BARSKY-R      2002        BARSKY-R-20… DO WE REALL… NBER MACROECO…
#>  5 105203318  BARSKY-R      2004        BARSKY-R-20… OIL AND THE… JOURNAL OF EC…
#>  6 43590636   BERNANKE-B    1997        BERNANKE-B-… SYSTEMATIC … BROOKINGS PAP…
#>  7 105203321  BLANCHARD-O   2002        BLANCHARD-O… DO WE REALL… NBER MACROECO…
#>  8 1111111113 BLINDER-A     2002        BLINDER-A-2… DO WE REALL… NBER MACROECO…
#>  9 117262816  CHAPPELL-H    2004        CHAPPELL-H-… DID TIME IN… ECONOMICS & P…
#> 10 60300900   CHARI-V       1998        CHARI-V-1998 EXPECTATION… JOURNAL OF EC…
#> # ℹ 26 more rows
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 379 × 11
#>    from    to weight Source    Target    cluster_leiden_from cluster_leiden_to
#>   <int> <int>  <dbl> <chr>     <chr>     <chr>               <chr>            
#> 1    30    31 0.119  101742279 101742296 03                  03               
#> 2    28    31 0.0270 101742279 101777013 03                  03               
#> 3    28    30 0.0783 101742296 101777013 03                  03               
#> # ℹ 376 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
#> 
#> $`2005-2014`
#> # A tbl_graph: 37 nodes and 525 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 37 × 11 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>         <chr>       <chr>        <chr>        <chr>         
#>  1 140987361  BENATI-L      2011        BENATI-L-20… WOULD THE B… "JOURNAL OF E…
#>  2 113856501  BEYER-A       2007        BEYER-A-2007 NATURAL RAT… "JOURNAL OF E…
#>  3 154773441  BILBIIE-FO    2013        BILBIIE-FO-… ASSET MARKE… "REVIEW OF EC…
#>  4 1111111116 BLINDER-A     2013        BLINDER-A-2… THE SUPPLY-… "THE GREAT IN…
#>  5 1111111117 BORDO-M       2013        BORDO-M-2013 BRETTON WOO… "THE GREAT IN…
#>  6 106371058  BULLARD-J     2005        BULLARD-J-2… DID THE GRE… "REVIEW OF EC…
#>  7 125746912  CANOVA-F      2009        CANOVA-F-20… STRUCTURAL … ""            
#>  8 131137084  CARBONI-G     2009        CARBONI-G-2… THE GREAT I… "JOURNAL OF M…
#>  9 6796575    CECCHETTI-S   2007        CECCHETTI-S… UNDERSTANDI… "US MONETARY …
#> 10 106371053  COGLEY-T      2005        COGLEY-T-20… DRIFTS AND … "REVIEW OF EC…
#> # ℹ 27 more rows
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 525 × 11
#>    from    to weight Source    Target    cluster_leiden_from cluster_leiden_to
#>   <int> <int>  <dbl> <chr>     <chr>     <chr>               <chr>            
#> 1     6    10 0.0543 106371053 106371058 04                  02               
#> 2    10    11 0.150  106371053 106371076 02                  02               
#> 3     6    11 0.0580 106371058 106371076 04                  02               
#> # ℹ 522 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
#> 
```
