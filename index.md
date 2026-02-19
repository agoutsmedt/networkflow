# networkflow

`networkflow` provides a complete workflow to build, structure, and
explore networks from tabular data.

Its key feature is a built-in dynamic analysis workflow: the package can
build networks across time windows, detect clusters in each window, and
link clusters across periods to track their evolution.

More broadly, `networkflow` supports the full analysis pipeline, from
network construction to interpretation and visualization, including
clustering, layout and color preparation, static plotting, and
interactive exploration with a Shiny app.

The package was developed with projected networks in mind (for example,
article -\> reference), but it can also be used more generally once data
are represented as `tbl_graph` objects.

The package includes:

- network construction
  ([`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md),
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)),
- clustering and inter-temporal matching
  ([`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md),
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)),
- interpretation
  ([`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md),
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)),
- visualization
  ([`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md),
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md),
  [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)),
- interactive exploration
  ([`launch_network_app()`](https://agoutsmedt.github.io/networkflow/reference/launch_network_app.md)).

For a full walkthrough, see:

- [`vignette("networkflow_presentation")`](https://agoutsmedt.github.io/networkflow/articles/networkflow_presentation.md)
- <https://agoutsmedt.github.io/networkflow/>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("agoutsmedt/networkflow")
```

## Quick start

``` r
library(networkflow)

nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
references <- Ref_stagflation

g <- build_network(
  nodes = nodes,
  directed_edges = references,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  keep_singleton = FALSE
)

g <- add_clusters(
  graphs = g,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123
)

g <- layout_networks(g, node_id = "source_id", layout = "kk")
g <- color_networks(g, column_to_color = "cluster_leiden")

plot_networks(
  graphs = g,
  x = "x",
  y = "y",
  cluster_label_column = "cluster_leiden",
  node_size_column = NULL,
  color_column = "color"
)
```
