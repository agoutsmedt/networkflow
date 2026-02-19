# Add Node Roles to Graphs

**\[experimental\]**

Adds node-level roles following (Guimera and Amaral 2005) to a tidygraph
or a list of tidygraphs. The function computes within-module degree,
participation coefficient, within-module z-score, and the seven role
categories, and adds them as new node attributes.

## Usage

``` r
add_node_roles(
  graphs,
  module_col = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
)
```

## Arguments

- graphs:

  A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/)
  or a list of tibble graphs.

- module_col:

  Name of the node column containing module/cluster identifiers.

- weight_col:

  Name of the edge column containing weights.

- z_threshold:

  Threshold for hub detection in the within-module z-score.

## Value

The same tibble graph or list of tibble graphs as input, with added node
columns: `within_module_degree` (within-module edge weight sum),
`within_module_z` (within-module degree z-score), `participation_coeff`
(participation coefficient across modules), `role_ga` (Guimera-Amaral
role label).

## Details

This function treats the graph as undirected. If your graph is directed,
the function will ignore edge directions and compute roles based on the
undirected version of the graph. The role classification is based on the
within-module degree z-score and the participation coefficient,
following the method proposed by Guimera and Amaral (2005). The
`z_threshold` parameter can be adjusted to change the sensitivity of hub
detection.

## References

Guimera R, Amaral LAN (2005). “Functional cartography of complex
metabolic networks.” *Nature*, **433**(7028), 895–900.
[doi:10.1038/nature03288](https://doi.org/10.1038/nature03288) .

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
clustering_method = "leiden",
objective_function = "modularity",
verbose = FALSE)

temporal_networks <- add_node_roles(temporal_networks,
module_col = "cluster_leiden")

temporal_networks[[1]]
#> # A tbl_graph: 74 nodes and 446 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 74 × 14 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 16182155   GORDON-R             1975 GORDON-R-19… ALTERNATIVE… "BROOKINGS PA…
#>  2 26283591   GORDON-R             1975 GORDON-R-19… THE IMPACT … "BROOKINGS PA…
#>  3 16182201   OKUN-A               1975 OKUN-A-1975a INFLATION: … "BROOKINGS PA…
#>  4 47749045   BRONFENBRENN…        1976 BRONFENBREN… ELEMENTS OF… "ZEITSCHRIFT …
#>  5 1111111141 KARNOSKY-D           1976 KARNOSKY-D-… THE LINK BE… "REVIEW"      
#>  6 1021902    FRIEDMAN-M           1977 FRIEDMAN-M-… NOBEL LECTU… "THE JOURNAL …
#>  7 5200398    GOLDSTEIN-M          1977 GOLDSTEIN-M… DOWNWARD PR… "STAFF PAPERS"
#>  8 31895842   GORDON-R             1977 GORDON-R-19… CAN THE INF… "BROOKINGS PA…
#>  9 14371908   RASCHE-R             1977 RASCHE-R-19… THE EFFECTS… ""            
#> 10 6013999    SHERMAN-H            1977 SHERMAN-H-1… MONOPOLY PO… "JOURNAL OF E…
#> # ℹ 64 more rows
#> # ℹ 8 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   within_module_degree <dbl>, within_module_z <dbl>,
#> #   participation_coeff <dbl>, role_ga <chr>
#> #
#> # Edge Data: 446 × 8
#>    from    to   weight Source  Target     cluster_leiden_from cluster_leiden_to
#>   <int> <int>    <dbl> <chr>   <chr>      <chr>               <chr>            
#> 1     6    11 0.00158  1021902 1111111122 02                  02               
#> 2     6    45 0.000173 1021902 1111111128 02                  03               
#> 3     6    66 0.000430 1021902 1111111134 02                  03               
#> # ℹ 443 more rows
#> # ℹ 1 more variable: cluster_leiden <chr>
```
