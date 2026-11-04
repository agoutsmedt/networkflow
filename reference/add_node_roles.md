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
  cluster_id = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s with a cluster column.

- cluster_id:

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

**Reading the roles.** The two measures answer two questions: *how
central is this node in its own cluster?* (\\z_i\\) and *where do its
ties go?* (\\P_i\\). What a role means depends on what the nodes and the
ties are. For the stagflation data of the package (documents linked by
bibliographic coupling, Leiden clusters, resolution 1), the examples
are:

- *Provincial hub* (R5): the central document of a literature, tied to
  most of its members and to almost nothing else. In a coupling network,
  it shares references with most documents of its cluster: Lansing's
  "Exploring the causes of the great inflation" (2000) in the cluster on
  the great inflation, Nordhaus's "Who's afraid of a big bad oil
  shock?" (2007) in the cluster on oil. In a co-citation network, the
  same role is played by the classics that are cited with most works of
  a literature.

- *Connector hub* (R6): central in its cluster and tied to other
  clusters: a document that is embedded in a literature while drawing on
  others. Pindyck's "Energy price increases and macroeconomic
  policy" (1980) is the only one: central in the cluster on
  productivity, oil and supply, it shares references with several other
  clusters.

- *Kinless hub* (R7): central in its cluster, with ties spread evenly
  over all clusters. It requires a cluster that is itself not very
  separate from the others; there is none in the stagflation example.

- *Non-hub connector* (R3): a document whose ties are spread over
  several clusters without being central in any: a bridge. Goldstein's
  "Downward price inflexibility, ratchet effects, and the inflationary
  impact of import price changes" (1977), or Blinder's "The supply-shock
  explanation of the great stagflation revisited" (2013), which belongs
  to the oil cluster but shares references with the other clusters, are
  such bridges. In a co-citation network, they are works used by several
  literatures.

- *Non-hub kinless* (R4): ties spread almost evenly over all clusters,
  which can mark an outlier as well as a bridge. In the example, the
  only one, Nitzan's "Regimes of differential accumulation: mergers,
  stagflation and the logic of globalization" (2001), shares references
  with five clusters in almost equal parts (17 to 20% of its tie weight
  each) and is below the average of its own cluster for within-module
  degree: it is tied to everybody a little, and to nobody much.

- *Peripheral* (R1, R2): all or most ties stay within the cluster. R1
  (participation below 0.05) are documents whose ties all stay in their
  own cluster, such as Phelps's "Commodity-supply shock and
  full-employment monetary policy" (1978), in the cluster on imports.

The same classification means other things for other nodes. For authors
coupled through their references (see
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)),
a connector is an author who cites several literatures, a hub an author
who cites like most of the group. For networks that are not
bibliographic, the roles describe how much each node holds its community
together (hubs) and how much it links communities (connectors).

**Cautions.** (1) Roles depend on the clusters, hence on the algorithm
and on the resolution: on the stagflation data, the mean participation
coefficient is 0.31, 0.39 and 0.52 for Leiden resolutions of 0.75, 1 and
1.5, because smaller clusters leave more ties between clusters. Check
that the nodes you interpret keep their role when the resolution changes
a little. (2) The thresholds of Guimerà and Nunes Amaral (2005) come
from metabolic networks. Projected networks are dense (here, a quarter
of the pairs of documents are tied) and most nodes have a participation
above 0.05: R2 is the usual role, and hubs are rare. (3) With the
weights of similarity measures, long bibliographies and well-cited
references influence the weights (see
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)).
(4) A role is a position in a network, not a judgement on the importance
or the quality of a document; reading the document is necessary.

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
  method = "similarity",
  edges_threshold = 1
)
#> ℹ Coupling (`biblionetwork::biblio_coupling()`) with "similarity": ties
#>   weighted by the idf-weighted cosine of the two lists of targets, after Shen
#>   et al. (2019), between source_id entities sharing target_id entities.
#> ℹ Two source_id are linked when they share at least 1 target_id
#>   (`edges_threshold`).
#> ℹ Nodes without any edge are removed (`keep_singleton = FALSE`).
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

network <- add_node_roles(network, cluster_id = "cluster_leiden")
table(tidygraph::as_tibble(network, active = "nodes")$role_ga)
#> 
#>  R1 ultra-peripheral        R2 peripheral R3 non-hub connector 
#>                   12                  125                    9 
#>    R5 provincial hub 
#>                    1 
```
