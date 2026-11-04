# Detect communities in networks

**\[experimental\]**

The second step of the pipeline. In a bibliographic coupling network,
groups of articles that cite the same literature, much more among
themselves than with the rest of the network, are a usual operational
definition of a research specialty or a research front (Boyack and
Klavans 2010; Morris and Van der Veer Martens 2008) . `add_clusters()`
finds such groups (communities, or clusters) with one of five algorithms
of [`igraph`](https://r.igraph.org/), adds the cluster of each node and
of each edge, and the relative size of each cluster. It works on one
`tbl_graph` or on each graph of a list, as produced by
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md).

Most methods search for the partition that maximises the *modularity*
(Newman and Girvan 2004) : the share of the edge weight that falls
within clusters, minus the share expected if edges were placed at random
given the weighted degree \\k_i\\ of each node, \$\$Q = \frac{1}{2m}
\sum\_{i,j} \left( A\_{ij} - \gamma \frac{k_i k_j}{2m} \right)
\delta(c_i, c_j),\$\$ with \\A\_{ij}\\ the weight of the edge between
\\i\\ and \\j\\, \\m\\ the total weight, \\c_i\\ the cluster of \\i\\,
and \\\gamma\\ the resolution (1 in the original definition).

## Usage

``` r
add_clusters(
  graphs,
  weights = NULL,
  clustering_method = c("leiden", "louvain", "fast_greedy", "infomap", "walktrap"),
  objective_function = c("modularity", "CPM"),
  resolution = 1,
  n_iterations = -1,
  n_groups = NULL,
  node_weights = NULL,
  trials = 10,
  steps = 4,
  verbose = TRUE,
  seed = NA
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- weights:

  Edge weights. `NULL` (default) uses the `weight` edge column when it
  exists, `NA` ignores it, and a numeric vector gives the weights
  directly. Weights are read as strengths of ties: a larger weight pulls
  two nodes into the same cluster.

- clustering_method:

  The algorithm, to choose explicitly among `"leiden"`, `"louvain"`,
  `"fast_greedy"`, `"infomap"` and `"walktrap"` (see the details).

- objective_function:

  For Leiden, the quality function to maximise: `"modularity"` or
  `"CPM"` (constant Potts model). Required for Leiden.

- resolution:

  For Leiden, the resolution \\\gamma\\. Higher values give more and
  smaller clusters.

- n_iterations:

  For Leiden, the number of iterations of the algorithm. The default,
  `-1`, iterates until the partition no longer improves (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)).
  A fixed number is much slower on large networks for no gain: on a
  window of 8,700 articles and 116,000 edges, 1,000 iterations took 44
  s, against 0.6 s for `-1`, for the same modularity.

- n_groups:

  For fast greedy and walktrap, an optional number of clusters at which
  to cut the dendrogram.

- node_weights:

  For Leiden and Infomap, optional name of a node column with node
  weights (see
  [`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html)
  and
  [`igraph::cluster_infomap()`](https://r.igraph.org/reference/cluster_infomap.html)).

- trials:

  For Infomap, the number of attempts to partition the network.

- steps:

  For walktrap, the length of the random walks.

- verbose:

  Logical. If `TRUE` (default), reports the number of clusters and the
  size of the largest one.

- seed:

  Optional integer. Leiden, Louvain and Infomap are stochastic: two runs
  can give different partitions. A seed makes the result reproducible.
  It is set locally (with
  [`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)):
  the random-number stream of the session is left unchanged.

## Value

The same object as `graphs` (a `tbl_graph` or a list of `tbl_graph`s)
with the cluster columns described in the details, with the node table
active.

## Details

**The algorithms.**

- `"leiden"` (Traag et al. 2019) , the default choice of recent
  bibliometric studies, improves on Louvain: it guarantees that clusters
  are connected, and iterating it refines the partition. It can maximise
  the modularity or the constant Potts model (CPM) (Traag et al. 2011) ,
  which compares the internal weight \\e_c\\ of each cluster with
  \\\gamma\\ times its number of pairs, \$\$H = \sum_c \left( e_c -
  \gamma \binom{n_c}{2} \right):\$\$ a cluster is worth creating when
  its density exceeds \\\gamma\\. Unlike the modularity, CPM does not
  depend on the size of the network, which makes clusters more
  comparable across networks of different sizes, but \\\gamma\\ must
  then be set on the scale of the edge weights (with weights well below
  1, `resolution = 1` yields almost only singletons).

- `"louvain"` (Blondel et al. 2008) , the greedy multi-level
  optimisation of modularity that Leiden refines.

- `"fast_greedy"` (Clauset et al. 2004) , an agglomerative optimisation
  of modularity, deterministic.

- `"walktrap"` (Pons and Latapy 2006) , which groups nodes that short
  random walks tend to visit together.

- `"infomap"` (Rosvall and Bergstrom 2008) , which finds the partition
  that best compresses the description of a random walk on the network.

All algorithms are called from `igraph`
([`igraph::cluster_leiden()`](https://r.igraph.org/reference/cluster_leiden.html),
[`igraph::cluster_louvain()`](https://r.igraph.org/reference/cluster_louvain.html),
[`igraph::cluster_fast_greedy()`](https://r.igraph.org/reference/cluster_fast_greedy.html),
[`igraph::cluster_walktrap()`](https://r.igraph.org/reference/cluster_walktrap.html),
[`igraph::cluster_infomap()`](https://r.igraph.org/reference/cluster_infomap.html)),
and all use the edge weights unless `weights = NA`.

**The resolution.** Modularity has a resolution limit: in large
networks, it can merge small but clear-cut communities (Fortunato 2010)
. There is no single right number of clusters: a network of a discipline
can be described at the level of fields or of specialties (Sjögårde and
Ahlgren 2020) . Changing `resolution`, and checking that the main
clusters persist across close values and across seeds (Traag et al.
2013) , is part of the analysis.

**The output columns.** With `clustering_method = "leiden"`, nodes get
`cluster_leiden` (identifiers numbered by decreasing cluster size, "01"
being the largest, written with two digits so that they sort correctly)
and `size_cluster_leiden` (the share of the nodes of the graph in the
cluster). Edges get `cluster_leiden_from`, `cluster_leiden_to` and
`cluster_leiden`, the latter being the common cluster of both ends, or
"00" for edges between clusters. The edge cluster is what lets
[`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
give within-cluster edges the colour of their cluster. Other methods
produce the same columns with their own suffix.

## References

Blondel VD, Guillaume J, Lambiotte R, Lefebvre E (2008). “Fast Unfolding
of Communities in Large Networks.” *Journal of Statistical Mechanics:
Theory and Experiment*, **2008**(10), P10008.
[doi:10.1088/1742-5468/2008/10/P10008](https://doi.org/10.1088/1742-5468/2008/10/P10008)
.  
  
Boyack KW, Klavans R (2010). “Co-Citation Analysis, Bibliographic
Coupling, and Direct Citation: Which Citation Approach Represents the
Research Front Most Accurately?” *Journal of the American Society for
Information Science and Technology*, **61**(12), 2389–2404.
[doi:10.1002/asi.21419](https://doi.org/10.1002/asi.21419) .  
  
Clauset A, Newman MEJ, Moore C (2004). “Finding Community Structure in
Very Large Networks.” *Physical Review E*, **70**(6), 066111.
[doi:10.1103/PhysRevE.70.066111](https://doi.org/10.1103/PhysRevE.70.066111)
.  
  
Fortunato S (2010). “Community Detection in Graphs.” *Physics Reports*,
**486**(3), 75–174.
[doi:10.1016/j.physrep.2009.11.002](https://doi.org/10.1016/j.physrep.2009.11.002)
.  
  
Morris SA, Van der Veer Martens B (2008). “Mapping Research
Specialties.” *Annual Review of Information Science and Technology*,
**42**(1), 213–295.
[doi:10.1002/aris.2008.1440420113](https://doi.org/10.1002/aris.2008.1440420113)
.  
  
Newman MEJ, Girvan M (2004). “Finding and Evaluating Community Structure
in Networks.” *Physical Review E*, **69**(2), 026113.
[doi:10.1103/PhysRevE.69.026113](https://doi.org/10.1103/PhysRevE.69.026113)
.  
  
Pons P, Latapy M (2006). “Computing Communities in Large Networks Using
Random Walks.” *Journal of Graph Algorithms and Applications*,
**10**(2), 191–218.
[doi:10.7155/jgaa.00124](https://doi.org/10.7155/jgaa.00124) .  
  
Rosvall M, Bergstrom CT (2008). “Maps of Random Walks on Complex
Networks Reveal Community Structure.” *Proceedings of the National
Academy of Sciences*, **105**(4), 1118–1123.
[doi:10.1073/pnas.0706851105](https://doi.org/10.1073/pnas.0706851105)
.  
  
Sjögårde P, Ahlgren P (2020). “Granularity of Algorithmically
Constructed Publication-Level Classifications of Research Publications:
Identification of Specialties.” *Quantitative Science Studies*,
**1**(1), 207–238.
[doi:10.1162/qss_a_00004](https://doi.org/10.1162/qss_a_00004) .  
  
Traag VA, Krings G, Van Dooren P (2013). “Significant Scales in
Community Structure.” *Scientific Reports*, **3**, 2930.
[doi:10.1038/srep02930](https://doi.org/10.1038/srep02930) .  
  
Traag VA, Van Dooren P, Nesterov Y (2011). “Narrow Scope for
Resolution-Limit-Free Community Detection.” *Physical Review E*,
**84**(1), 016114.
[doi:10.1103/PhysRevE.84.016114](https://doi.org/10.1103/PhysRevE.84.016114)
.  
  
Traag VA, Waltman L, van Eck NJ (2019). “From Louvain to Leiden:
Guaranteeing Well-Connected Communities.” *Scientific Reports*,
**9**(1), 5233.
[doi:10.1038/s41598-019-41695-z](https://doi.org/10.1038/s41598-019-41695-z)
.

## See also

[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
to follow clusters across time windows,
[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
to label them,
[`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
to characterise the position of nodes within and between clusters.

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
  resolution = 1,
  seed = 123
)
#> ℹ "leiden" found 4 clusters; the largest gathers 46.9% of the nodes.
table(tidygraph::as_tibble(network, active = "nodes")$cluster_leiden)
#> 
#> 01 02 03 04 
#> 69 51 18  9 
```
