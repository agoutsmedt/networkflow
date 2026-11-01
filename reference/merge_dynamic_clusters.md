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

The rule is a double majority. Let \\V_a\\ be the nodes of cluster \\a\\
in window \\t-1\\ and \\V_b\\ those of cluster \\b\\ in window \\t\\.
Cluster \\b\\ inherits the identifier of \\a\\ when most nodes of \\a\\
go to \\b\\ *and* most nodes of \\b\\ come from \\a\\: \$\$\frac{\|V_a
\cap V_b\|}{D_a} \> \tau \quad \text{and} \quad \frac{\|V_a \cap
V_b\|}{D_b} \> \tau,\$\$ with \\\tau\\ the `threshold_similarity`. The
denominators depend on `similarity_type`: with `"complete"`, \\D_a =
\|V_a\|\\ and \\D_b = \|V_b\|\\; with `"partial"`, only the nodes
present in both windows count, \\D_a = \|V_a \cap V_t\|\\ and \\D_b =
\|V_b \cap V\_{t-1}\|\\, \\V_t\\ being all the nodes of window \\t\\.
Since \\\tau \> 0.5\\, a cluster can inherit at most one identifier, and
an identifier passes to at most one cluster. A cluster that meets no
match gets a new identifier.

## Usage

``` r
merge_dynamic_clusters(
  list_graph,
  cluster_id,
  node_id,
  threshold_similarity = 0.5001,
  similarity_type = c("complete", "partial")
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

- threshold_similarity:

  Number in \\(0.5, 1\]\\, the threshold \\\tau\\. Higher values require
  more similar clusters and create more distinct dynamic clusters.

- similarity_type:

  `"complete"` or `"partial"`, see the description. Required: there is
  no default, as the choice changes the results.

## Value

The list of graphs, with the same names, where nodes have a new column
`dynamic_{cluster_id}` (e.g. `dynamic_cluster_leiden`, with identifiers
"cl_1", "cl_2"...) and edges the columns `dynamic_{cluster_id}_from`,
`dynamic_{cluster_id}_to` and `dynamic_{cluster_id}` (missing for edges
between clusters).

## Details

**Which windows?** The rule needs nodes present in successive windows.
In a network of documents (e.g. bibliographic coupling), each document
has a single date, so successive windows (`overlapping_window = FALSE`
in
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md))
share no node and no cluster can be matched: use sliding windows.
Successive windows work when nodes persist over time, as cited
references (co-citation), authors or journals do; two windows may still
share no node when the literature changed completely.

**Complete or partial?** With sliding windows, the network of window
\\t\\ loses the documents of the first year of window \\t-1\\ and gains
those of a new year; in a growing literature, newcomers can be numerous.
With `"complete"`, a cluster that keeps all its past members but doubles
with newcomers cannot inherit the identifier, because its members coming
from \\a\\ are less than half of it: the cluster is considered new
because its composition changed. With `"partial"`, the newcomers are
ignored and the same cluster keeps its identifier. Which one is right
depends on what a cluster is taken to be: a group of documents
(complete), or a structure that recruits new documents (partial).

**What the rule does not do.** Clusters are compared only with the
previous window: a cluster that vanishes for one window and comes back
gets a new identifier. Splits and merges are not recorded as such: the
part that meets the double majority keeps the identifier, the others are
new. With \\\tau\\ close to 0.5, small fluctuations of the partition (a
stochastic algorithm, a resolution change) may break or create
continuities; it is good practice to compare the results for a few
thresholds and seeds.

## References

Rosvall M, Bergstrom CT (2010). “Mapping Change in Large Networks.”
*PLoS ONE*, **5**(1), e8694.
[doi:10.1371/journal.pone.0008694](https://doi.org/10.1371/journal.pone.0008694)
.

## See also

[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
with `name_merged_clusters = TRUE` to label the dynamic clusters;
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
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  verbose = FALSE
)
#> Warning: The last window is shorter than the others.
#> ℹ The data end in 2013, but the last window ends in 2014.
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
  threshold_similarity = 0.51,
  similarity_type = "partial"
)
#> Warning: 3 pairs of consecutive windows share no node: their clusters cannot be matched.
#> ℹ Clusters are matched through the nodes they share. In a network of documents
#>   (coupling), each node has one date: use sliding windows (`overlapping_window
#>   = TRUE` in `build_dynamic_networks()`). If windows already slide, the nodes
#>   may have changed completely, or the threshold may leave a window without
#>   edges.
networks[[2]]
#> # A tbl_graph: 7 nodes and 6 edges
#> #
#> # An unrooted tree
#> #
#> # Node Data: 7 × 11 (active)
#>   source_id  source_author source_year source_label  source_title source_journal
#>   <chr>      <chr>               <int> <chr>         <chr>        <chr>         
#> 1 1111111134 FISCHER-S            1985 FISCHER-S-19… SUPPLY SHOC… "JOURNAL OF M…
#> 2 1184130    GISSER-M             1986 GISSER-M-1986 CRUDE OIL A… "JOURNAL OF M…
#> 3 25481516   HAKES-DR             1988 HAKES-DR-1988 EVIDENCE OF… "REVIEW OF RA…
#> 4 6714008    HELLIWELL-J          1988 HELLIWELL-J-… COMPARATIVE… "JOURNAL OF E…
#> 5 214927     BALL-L               1991 BALL-L-1991   THE GENESIS… "JOURNAL OF M…
#> 6 1111111176 TAYLOR-J             1992 TAYLOR-J-1992 THE GREAT I… "RESERVE BANK…
#> 7 19627977   PARKIN-M             1993 PARKIN-M-1993 INFLATION I… "PRICE STABIL…
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
#> #
#> # Edge Data: 6 × 11
#>    from    to   weight Source     Target   cluster_leiden_from cluster_leiden_to
#>   <int> <int>    <dbl> <chr>      <chr>    <chr>               <chr>            
#> 1     1     3 0.00185  1111111134 25481516 02                  02               
#> 2     1     4 0.00130  1111111134 6714008  02                  02               
#> 3     6     7 0.000305 1111111176 19627977 01                  01               
#> # ℹ 3 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
```
