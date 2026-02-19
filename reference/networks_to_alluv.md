# Create a data.frame suitable for alluvial graph projection

This function creates a data.frame that can be easily plotted with
ggalluvial from a list of networks.

## Usage

``` r
networks_to_alluv(
  graphs,
  intertemporal_cluster_column,
  node_id,
  summary_cluster_stats = TRUE,
  keep_color = TRUE,
  color_column = "color",
  keep_cluster_label = TRUE,
  cluster_label_column = "cluster_label"
)
```

## Arguments

- graphs:

  A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/)
  or a list of tibble graphs.

- intertemporal_cluster_column:

  The column with the identifier of the inter-temporal cluster. If you
  have used
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and
  [merge_dynamic_clusters()](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
  it is of the form `dynamic_cluster_{clustering_method}`.

- node_id:

  The column with the unique identifier of each node.

- summary_cluster_stats:

  If set to `TRUE`, the data.frame will contain a list of variable that
  summarize cluster statistics of the alluvial. These variables can be
  particularly useful to filter smaller communities when plotting
  according to different variables:

  - `share_cluster_alluv` is the percentage share of a given cluster
    across all time windows;

  - `share_cluster_window` is the percentage share of a given cluster in
    a given time window;

  - `share_cluster_max` is the highest value of `share_cluster_window`
    for a given cluster across all individual time windows;

  - `length_cluster` is the number of time windows a cluster exists.

- keep_color:

  Set to `TRUE` (by default) if you want to keep the column with the
  color associated to the different categories of
  `intertemporal_cluster_column`. Such a column exists in your list of
  tibble graphs if you have use
  [color_networks()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- color_column:

  The name of the column with the colors of the categories in
  `intertemporal_cluster_column`. By default, "color", as it is the
  column name resulting from the use of
  [color_networks()](https://agoutsmedt.github.io/networkflow/reference/color_networks.md).

- keep_cluster_label:

  Set to `TRUE` if you want to keep the column with a name/label
  associated to the different categories of
  `intertemporal_cluster_column`. Such a column exists in your list of
  tibble graphs if you have use
  [name_clusters()](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md).

- cluster_label_column:

  The name of the column with the name/label associated to the
  categories in `intertemporal_cluster_column`. By default,
  "cluster_label", as it is the column name resulting from the use of
  [name_clusters()](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md).

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

temporal_networks <- merge_dynamic_clusters(temporal_networks,
cluster_id = "cluster_leiden",
node_id = "source_id",
threshold_similarity = 0.51,
similarity_type = "partial")

temporal_networks <- name_clusters(graphs = temporal_networks,
method = "tf-idf",
name_merged_clusters = TRUE,
cluster_id = "dynamic_cluster_leiden",
text_columns = "source_title",
nb_terms_label = 5,
clean_word_method = "lemmatize")
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 2 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.

temporal_networks <- color_networks(graphs = temporal_networks,
column_to_color = "dynamic_cluster_leiden",
color = NULL)
#> ℹ unique_color_across_list has been set to FALSE. There are 13 different categories to color.
#> ℹ color is neither a vector of color characters, nor a data.frame. We will proceed with base R colors.
#> ℹ We draw 7 colors from the ggplot2 palette and 6 colors from the Okabe-Ito palette.

alluv_dt <- networks_to_alluv(temporal_networks,
intertemporal_cluster_column = "dynamic_cluster_leiden",
node_id = "source_id")

alluv_dt[1:5]
#>    dynamic_cluster_leiden    window  source_id   color
#>                    <char>    <char>     <char>  <char>
#> 1:                   cl_1 1975-1994   16182155 #B79F00
#> 2:                   cl_1 1975-1994   26283591 #B79F00
#> 3:                   cl_1 1975-1994   31895842 #B79F00
#> 4:                   cl_1 1975-1994 1111111131 #B79F00
#> 5:                   cl_1 1975-1994 1111111150 #B79F00
#>                                            cluster_label share_cluster_alluv
#>                                                   <char>               <num>
#> 1: control, control program, program, price level, level                6.22
#> 2: control, control program, program, price level, level                6.22
#> 3: control, control program, program, price level, level                6.22
#> 4: control, control program, program, price level, level                6.22
#> 5: control, control program, program, price level, level                6.22
#>    share_cluster_window share_cluster_max length_cluster    y_alluv
#>                   <num>             <num>          <int>      <num>
#> 1:                17.57             18.31              7 0.01351351
#> 2:                17.57             18.31              7 0.01351351
#> 3:                17.57             18.31              7 0.01351351
#> 4:                17.57             18.31              7 0.01351351
#> 5:                17.57             18.31              7 0.01351351
```
