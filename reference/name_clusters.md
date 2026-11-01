# Give readable names to clusters

**\[experimental\]**

A cluster identifier ("01", "cl_12") says nothing about its content.
`name_clusters()` gives each cluster a provisional label that helps
reading plots and tables, with one of three methods: the words that
distinguish the cluster from the others (`"tf-idf"`), or the metadata of
its most central node (`"tidygraph_functions"`) or of its node with the
highest value of a given column (`"given_column"`). These labels are
starting points for interpretation, not the result of it: naming a
cluster requires reading its documents, the references they share, and
what distinguishes them from neighbouring clusters (Ding 2011) .

## Usage

``` r
name_clusters(
  graphs,
  method = c("tidygraph_functions", "given_column", "tf-idf"),
  name_merged_clusters = FALSE,
  cluster_id,
  label_columns,
  label_name = "cluster_label",
  tidygraph_function = NULL,
  order_by = NULL,
  text_columns = NULL,
  nb_terms_label = 3,
  ...
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- method:

  `"tf-idf"` (chosen when no method is given), `"tidygraph_functions"`
  or `"given_column"` (see the details).

- name_merged_clusters:

  Logical. `TRUE` when `cluster_id` identifies clusters across a list of
  graphs (the dynamic clusters of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)):
  each cluster then gets one name, computed on all its nodes in all
  windows. `FALSE` (default) names the clusters of each graph
  separately.

- cluster_id:

  Character scalar, the node column with the clusters to name.

- label_columns:

  For `"tidygraph_functions"` and `"given_column"`, the node column(s)
  used to build the label of the chosen node, joined by "\_" (e.g.
  `c("source_author", "source_year")`).

- label_name:

  Character scalar, the name of the new column (`"cluster_label"` by
  default).

- tidygraph_function:

  For `"tidygraph_functions"`, a call to a centrality function of
  tidygraph, such as
  [`tidygraph::centrality_pagerank()`](https://tidygraph.data-imaginist.com/reference/centrality.html)
  (see
  [tidygraph::centrality](https://tidygraph.data-imaginist.com/reference/centrality.html)).

- order_by:

  For `"given_column"`, the numeric node column used to choose the node
  (e.g. `"node_size"` from
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  with `compute_size = TRUE`, i.e. the most cited document of the
  cluster).

- text_columns:

  For `"tf-idf"`, the node column(s) with text (titles, abstracts,
  keywords), passed to
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md).

- nb_terms_label:

  For `"tf-idf"`, the number of terms in each label (3 by default),
  separated by commas.

- ...:

  For `"tf-idf"`, other arguments of
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
  (e.g. `clean_word_method`, `n_gram`, `ngrams_filter`).

## Value

The same object as `graphs`, with a new node column `label_name`, also
joined to the edges through their cluster column.

## Details

**tf-idf.** Each cluster is treated as one document made of the texts of
its nodes, and each term \\t\\ (word or n-gram) gets the weight
\$\$\mathrm{tf}\_{tc} \times \mathrm{idf}\_t = \frac{n\_{tc}}{\sum_u
n\_{uc}} \times \log\frac{C}{\mathrm{df}\_t}\$\$ (Spärck Jones 1972;
Salton and Buckley 1988) , with \\n\_{tc}\\ the number of occurrences of
\\t\\ in cluster \\c\\, \\C\\ the number of clusters and
\\\mathrm{df}\_t\\ the number of clusters where \\t\\ appears. The label
gathers the terms with the highest weight: frequent in the cluster, rare
in the others. With `name_merged_clusters = FALSE`, the comparison is
made within each graph, so clusters of different windows may get the
same label.

**Most central node.** `"tidygraph_functions"` computes the centrality
given in `tidygraph_function` on each graph (it is added as a node
column) and labels each cluster with the node with the highest value.
The centrality is computed on the whole graph, not within the cluster.
This method needs a network structure and therefore cannot be used with
`name_merged_clusters = TRUE`.

**Given column.** `"given_column"` labels each cluster with its node
with the highest `order_by` value: for an article network, typically its
most cited article.

Nodes of an unnamed cluster get "no_name".

## References

Ding Y (2011). “Community Detection: Topological vs. Topical.” *Journal
of Informetrics*, **5**(4), 498–514.
[doi:10.1016/j.joi.2011.02.006](https://doi.org/10.1016/j.joi.2011.02.006)
.  
  
Salton G, Buckley C (1988). “Term-Weighting Approaches in Automatic Text
Retrieval.” *Information Processing & Management*, **24**(5), 513–523.  
  
Spärck Jones K (1972). “A Statistical Interpretation of Term Specificity
and Its Application in Retrieval.” *Journal of Documentation*,
**28**(1), 11–21.

## See also

[`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
to see more terms than the labels show.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1
)
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

# The words of the titles that distinguish each cluster
network <- name_clusters(
  network,
  method = "tf-idf",
  cluster_id = "cluster_leiden",
  text_columns = "source_title",
  nb_terms_label = 3,
  clean_word_method = "none"
)

# The article with the highest PageRank of each cluster
network <- name_clusters(
  network,
  method = "tidygraph_functions",
  cluster_id = "cluster_leiden",
  label_columns = c("source_author", "source_year"),
  label_name = "central_article",
  tidygraph_function = tidygraph::centrality_pagerank()
)
unique(tidygraph::as_tibble(network, active = "nodes")[
  c("cluster_leiden", "cluster_label", "central_article")
])
#> # A tibble: 6 × 3
#>   cluster_leiden cluster_label                         central_article
#>   <chr>          <chr>                                 <chr>          
#> 1 01             great inflation, great, stabilization VELDE-F_2004   
#> 2 06             comment, stagflation, inflation       WACHTER-M_1978 
#> 3 02             productivity, supply shocks, oil      GORDON-R_1984  
#> 4 03             oil, oil price, great stagflation     BARSKY-R_2002  
#> 5 04             great, alternative, supply            BLINDER-A_1979 
#> 6 05             unemployment, stagflation, evidence   OKUN-A_1975    
```
