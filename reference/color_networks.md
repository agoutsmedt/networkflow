# Color Nodes and Edges of Networks or Alluvial

**\[experimental\]**

`color_networks()` takes as input a tibble graph (from
[tidygraph](https://tidygraph.data-imaginist.com/)) or a list of tibble
graphs and associates a color for each graphs' edges and nodes,
depending on a chosen categorical variable in `columnr_to_color` (most
likely a cluster column).

`color_alluvial()` takes a data.frame and associates a color to each
value of the chosen categorical variable in `column_to_color`. This
function may be used with any data.frame even if it aims at coloring
alluvial data frame created with
[networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md).

You may either provide the color palette, provide a data frame
associating the different values of the categorical variable with
colors, or let the function provide colors (see details).

## Usage

``` r
color_networks(
  graphs,
  column_to_color,
  color = NULL,
  unique_color_across_list = FALSE
)

color_alluvial(alluv_dt, column_to_color, color = NULL)
```

## Arguments

- graphs:

  A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/)
  or a list of tibble graphs.

- column_to_color:

  The column of the categorical variable to use to color nodes and
  edges. For instance, the `cluster_{clustering_method}` created with
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  or the `dynamic_cluster_{clustering_method}` created with
  [merge_dynamic_clusters()](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).

- color:

  The colors to use. It may be a vector of colors (in a character
  format) or a two columns data.frame with the first column as the
  distinct observations of the `column_to_color` and a second column
  with the vector of colors you want to use.

- unique_color_across_list:

  If set to `TRUE`, in a list of tibble graphs, the same categorical
  variable will be considered as a different variable in different
  graphs and thus receive a different color. In other words, if set to
  `TRUE` cluster "01" in two different graphs will have two different
  colors. If set to `FALSE` (by default), cluster "01" will have the
  same color in every graphs it exists.

- alluv_dt:

  A data.frame of an alluvial created with
  [networks_to_alluv()](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)

## Value

The same tibble graph or list of tibble graphs as input, but with a new
`color` column for both nodes and edges.

## Details

The best practice is to provide a list of colors equals to the number of
categorical variable to color. If you provide more colors, excess colors
will not be used. If you provide less colors, colors will be recycled.
If you provide no colors,
[`palette.colors()`](https://rdrr.io/r/grDevices/palette.html) of base R
will be used: the 7 colors of `ggplot2` palette will be used (black is
excluded) and then the 7 colors of `Okabe-Ito` palette (black and gray
are excluded). Above 14 colors, the colors of the two palettes will be
recycled.

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

temporal_networks <- color_networks(graphs = temporal_networks,
column_to_color = "cluster_leiden",
color = NULL)
#> ℹ unique_color_across_list has been set to FALSE. There are 7 different categories to color.
#> ℹ color is neither a vector of color characters, nor a data.frame. We will proceed with base R colors.
#> ℹ We draw 7 colors from the ggplot2 palette.

temporal_networks[[1]]
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
#> # ℹ 2 more variables: cluster_leiden <chr>, color <chr>
#> #
#> # Node Data: 74 × 11
#>   source_id source_author source_year source_label   source_title source_journal
#>   <chr>     <chr>               <int> <chr>          <chr>        <chr>         
#> 1 16182155  GORDON-R             1975 GORDON-R-1975a ALTERNATIVE… BROOKINGS PAP…
#> 2 26283591  GORDON-R             1975 GORDON-R-1975b THE IMPACT … BROOKINGS PAP…
#> 3 16182201  OKUN-A               1975 OKUN-A-1975a   INFLATION: … BROOKINGS PAP…
#> # ℹ 71 more rows
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>, color <chr>
```
