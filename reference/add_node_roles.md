# Characterise the role of nodes within and between clusters

**\[experimental\]**

Two nodes of the same cluster can play very different roles: one may be
tied to almost every member of its cluster, another may sit at the
border and connect several clusters. `add_node_roles()` computes the two
measures of the "functional cartography" of Guimerà and Nunes Amaral
(2005) and classifies each node into one of their seven roles.

With \\\kappa_i\\ the weight of the edges of node \\i\\ towards its own
cluster \\s_i\\, the **within-module degree** z-score measures how
strongly \\i\\ is connected to its cluster compared with the other
members: \$\$z_i = \frac{\kappa_i -
\bar{\kappa}\_{s_i}}{\sigma\_{\kappa\_{s_i}}}.\$\$ With \\k\_{is}\\ the
weight of the edges of \\i\\ towards cluster \\s\\ and \\k_i\\ its total
weight, the **participation coefficient** is close to 0 when all the
ties of \\i\\ stay within one cluster, and close to 1 when they are
spread evenly across clusters: \$\$P_i = 1 - \sum_s \left(
\frac{k\_{is}}{k_i} \right)^2.\$\$

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

  A `tbl_graph` or a list of `tbl_graph`s with a cluster column.

- module_col:

  Character scalar, the node column with the clusters. The default,
  `"dynamic_cluster_leiden"`, fits the output of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md);
  use `"cluster_leiden"` after
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  alone.

- weight_col:

  Character scalar, the edge column with the weights (`"weight"` by
  default). Use a column of ones for an unweighted analysis.

- z_threshold:

  Number, the within-module z-score above which a node is a hub (2.5,
  the value of Guimerà and Amaral).

## Value

The same object as `graphs` with four new node columns:
`within_module_degree` (\\\kappa_i\\), `within_module_z` (\\z_i\\, 0
when all members of the cluster have the same \\\kappa\\),
`participation_coeff` (\\P_i\\, 0 for isolated nodes) and `role_ga` (the
role).

## Details

The roles combine the two measures, with the thresholds of Guimerà and
Nunes Amaral (2005) :

|                      |                        |              |
|----------------------|------------------------|--------------|
| Role                 | \\z_i\\                | \\P_i\\      |
| R1 ultra-peripheral  | below `z_threshold`    | below 0.05   |
| R2 peripheral        | below `z_threshold`    | 0.05 to 0.62 |
| R3 non-hub connector | below `z_threshold`    | 0.62 to 0.80 |
| R4 non-hub kinless   | below `z_threshold`    | above 0.80   |
| R5 provincial hub    | at least `z_threshold` | below 0.30   |
| R6 connector hub     | at least `z_threshold` | 0.30 to 0.75 |
| R7 kinless hub       | at least `z_threshold` | above 0.75   |

The original measures count edges; here they use the edge weights
(strengths), which is the natural generalisation for weighted projected
networks. The graph is treated as undirected. The thresholds were
calibrated on biological networks: in small bibliometric networks, few
nodes pass \\z \geq 2.5\\, and the measures are best read as continuous
indicators (who are the central members of a literature, which articles
bridge two literatures) rather than as categories.

## References

Guimerà R, Nunes Amaral LA (2005). “Functional Cartography of Complex
Metabolic Networks.” *Nature*, **433**(7028), 895–900.
[doi:10.1038/nature03288](https://doi.org/10.1038/nature03288) .

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

network <- add_node_roles(network, module_col = "cluster_leiden")
table(tidygraph::as_tibble(network, active = "nodes")$role_ga)
#> 
#>  R1 ultra-peripheral        R2 peripheral R3 non-hub connector 
#>                    9                  107                   27 
#>   R4 non-hub kinless    R5 provincial hub     R6 connector hub 
#>                    1                    2                    1 
```
