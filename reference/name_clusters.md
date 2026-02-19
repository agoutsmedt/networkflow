# Automatically Attributing Names to Clusters

**\[experimental\]**

The function gives a name to networks clusters. It also gives the edges
the name of their cluster. The clusters are named according to the
column chosen by the user (for instance, in the case nodes are articles,
the name may be the author and date of an article).

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

  A tibble graph (from
  [tidygraph](https://tidygraph.data-imaginist.com/)) or a list of
  tibble graphs.

- method:

  The method for finding the names, among `tidygraph_functions`,
  `given_column`, and `tf-idf` (see the details). The `tf-idf` method is
  chosen by default.

- name_merged_clusters:

  Set to `TRUE` if your clusters have been established for all your
  tibble graphs and thus are unique. Typically, you have such clusters
  after running
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).

- cluster_id:

  The column you want to name. Generally, the column with the identifier
  of the clusters, whether the simple cluster detected with
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  or the merged clusters detected with
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).

- label_columns:

  The column you want to be used to name the clusters. If the nodes are
  article, you can choose, for instance, the columns with the author of
  the article and the date of publication.

- label_name:

  The name of the column with cluster names, that will be created by the
  function. "cluster_label" by default.

- tidygraph_function:

  For the `tidygraph_functions` method (see the details), the centrality
  measure to be chosen among the measures implemented in in `tidygraph`
  (see
  [`tidygraph::centrality()`](https://tidygraph.data-imaginist.com/reference/centrality.html)).

- order_by:

  For the `given_column` method, the column within the nodes list of
  your tibble graph(s) you want to be used to classify nodes and choose
  names. This must be a numeric column. For instance, you can use the
  `node_size` column of your network if you have set `compute_size` to
  `TRUE` in
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  or
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md).

- text_columns:

  For the `tf-idf` method, the columns with the text you want to
  analyze. If you give multiple columns, they will be united to extract
  the terms. This is a parameter of
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md).

- nb_terms_label:

  For the `tf-idf` method, the number of terms you want to be used to
  serve a the name of a cluster. Terms will be separated by a comma.
  This is a parameter of
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md).

- ...:

  Additional arguments from `extract_tfidf`, outside of those referred
  above as well as of `grouping_across_list` which is not relevant here.

## Value

The same tibble graph or list of tibble graphs with a new column with
the names of the clusters, for both nodes and edges. If you choose the
`tidygraph_functions` method, the function also returns for nodes a
column with the centrality measure computed.

## Details

The node to be used for naming the community is chosen depending on 3
methods:

- the `tidygraph_functions` method: the name of a cluster comes from the
  node, within the cluster, which has the highest centrality measure.
  The user can choose the different centrality measure implemented in
  `tidygraph` (see
  [`tidygraph::centrality()`](https://tidygraph.data-imaginist.com/reference/centrality.html)
  for details).

- the `given_column` method: the user gives a column of the tibble
  graph(s), with numeric values, that will be used to classify the nodes
  and choose the name of each cluster. The `label_columns` of the node
  with the highest numerical value in the cluster will be used to name
  the cluster.

- the `tf-idf` method: clusters are name according to the terms with the
  highest tf-idf value for each cluster. The user furnishes one or
  several columns with text, and the function extracts the term and
  calculate the tf-idf value of each term, depending on all the
  clusters. This method uses
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md).

Please note that, when `name_merged_clusters` is set to `FALSE`, the
TF-IDF is computed tibble graph by tibble graph. It means that it is
more likely that clusters in different tibble graphs will share the same
name.

## Examples

``` r
library(networkflow)

nodes <- networkflow::Nodes_stagflation |>
  dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

temporal_networks <- build_dynamic_networks(nodes = nodes,
directed_edges = references,
source_id = "source_id",
target_id = "target_id",
time_variable = "source_year",
cooccurrence_method = "coupling_similarity",
time_window = 20,
edges_threshold = 1,
overlapping_window = TRUE,
filter_components = TRUE,
verbose = FALSE)

temporal_networks <- add_clusters(temporal_networks,
objective_function = "modularity",
clustering_method = "leiden",
verbose = FALSE)

# You can name the clusters in each tibble graphs:

temporal_networks_with_names <- name_clusters(graphs = temporal_networks,
method = "tidygraph_functions",
name_merged_clusters = FALSE,
cluster_id = "cluster_leiden",
label_columns = c("source_author", "source_year"),
tidygraph_function = tidygraph::centrality_pagerank())

temporal_networks_with_names[[1]]
#> # A tbl_graph: 74 nodes and 446 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 446 × 9 (active)
#>     from    to   weight Source  Target     cluster_leiden_from cluster_leiden_to
#>    <int> <int>    <dbl> <chr>   <chr>      <chr>               <chr>            
#>  1     6    11 0.00158  1021902 1111111122 02                  02               
#>  2     6    45 0.000173 1021902 1111111128 02                  03               
#>  3     6    66 0.000430 1021902 1111111134 02                  03               
#>  4     6    35 0.000644 1021902 1111111146 02                  02               
#>  5     6    20 0.000126 1021902 1111111180 02                  02               
#>  6     6    42 0.000614 1021902 1111111182 02                  02               
#>  7     6    21 0.000343 1021902 1111111183 02                  02               
#>  8     6    53 0.000259 1021902 1184127    02                  03               
#>  9     6    31 0.00121  1021902 14490177   02                  02               
#> 10     6    65 0.000274 1021902 16167977   02                  03               
#> # ℹ 436 more rows
#> # ℹ 2 more variables: cluster_leiden <chr>, cluster_label <chr>
#> #
#> # Node Data: 74 × 12
#>   source_id source_author source_year source_label   source_title source_journal
#>   <chr>     <chr>               <int> <chr>          <chr>        <chr>         
#> 1 16182155  GORDON-R             1975 GORDON-R-1975a ALTERNATIVE… BROOKINGS PAP…
#> 2 26283591  GORDON-R             1975 GORDON-R-1975b THE IMPACT … BROOKINGS PAP…
#> 3 16182201  OKUN-A               1975 OKUN-A-1975a   INFLATION: … BROOKINGS PAP…
#> # ℹ 71 more rows
#> # ℹ 6 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>, centrality_pagerank <dbl>,
#> #   cluster_label <chr>

# Or you can name the dynamic clusters:

temporal_networks <- merge_dynamic_clusters(temporal_networks,
cluster_id = "cluster_leiden",
node_id = "source_id",
threshold_similarity = 0.51,
similarity_type = "partial")

temporal_networks_with_names <- name_clusters(graphs = temporal_networks,
method = "tf-idf",
name_merged_clusters = TRUE,
cluster_id = "dynamic_cluster_leiden",
text_columns = "source_title",
nb_terms_label = 5,
clean_word_method = "lemmatize")
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 2 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.

temporal_networks_with_names[[1]]
#> # A tbl_graph: 74 nodes and 446 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 446 × 12 (active)
#>     from    to   weight Source  Target     cluster_leiden_from cluster_leiden_to
#>    <int> <int>    <dbl> <chr>   <chr>      <chr>               <chr>            
#>  1     6    11 0.00158  1021902 1111111122 02                  02               
#>  2     6    45 0.000173 1021902 1111111128 02                  03               
#>  3     6    66 0.000430 1021902 1111111134 02                  03               
#>  4     6    35 0.000644 1021902 1111111146 02                  02               
#>  5     6    20 0.000126 1021902 1111111180 02                  02               
#>  6     6    42 0.000614 1021902 1111111182 02                  02               
#>  7     6    21 0.000343 1021902 1111111183 02                  02               
#>  8     6    53 0.000259 1021902 1184127    02                  03               
#>  9     6    31 0.00121  1021902 14490177   02                  02               
#> 10     6    65 0.000274 1021902 16167977   02                  03               
#> # ℹ 436 more rows
#> # ℹ 5 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>,
#> #   cluster_label <chr>
#> #
#> # Node Data: 74 × 12
#>   source_id source_author source_year source_label   source_title source_journal
#>   <chr>     <chr>               <int> <chr>          <chr>        <chr>         
#> 1 16182155  GORDON-R             1975 GORDON-R-1975a ALTERNATIVE… BROOKINGS PAP…
#> 2 26283591  GORDON-R             1975 GORDON-R-1975b THE IMPACT … BROOKINGS PAP…
#> 3 16182201  OKUN-A               1975 OKUN-A-1975a   INFLATION: … BROOKINGS PAP…
#> # ℹ 71 more rows
#> # ℹ 6 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>, cluster_label <chr>
```
