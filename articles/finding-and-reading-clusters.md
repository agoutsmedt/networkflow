# Finding and reading clusters

This guide covers the second step of the pipeline: detecting clusters in
a network and interpreting them. It starts from the bibliographic
coupling network of the explanations of the 1970s stagflation built in
[the first
guide](https://agoutsmedt.github.io/networkflow/articles/from-references-to-networks.md).

``` r

library(networkflow)
library(ggplot2)
library(tidygraph)

nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
network <- build_network(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1
)
```

## What is a cluster?

In a bibliographic coupling network, a group of documents that cite the
same literature, much more among themselves than with the rest of the
network, is the usual operational definition of a *research specialty*
or a *research front* ([Morris and Van der Veer Martens
2008](#ref-morris2008); [Boyack and Klavans 2010](#ref-boyack2010)).
Detecting such groups without deciding them in advance is the task of
*community detection* ([Fortunato 2010](#ref-fortunato2010)). The
algorithms differ, but most formalise “more ties inside than outside”
with the **modularity** of a partition ([Newman and Girvan
2004](#ref-newman2004)):

``` math
Q = \frac{1}{2m} \sum_{i,j} \left( A_{ij} - \gamma \frac{k_i k_j}{2m} \right) \delta(c_i, c_j),
```

with $`A_{ij}`$ the weight of the edge between documents $`i`$ and
$`j`$, $`k_i`$ the total weight of the edges of $`i`$, $`m`$ the total
weight of the network, $`\delta(c_i, c_j) = 1`$ when $`i`$ and $`j`$ are
in the same cluster, and $`\gamma`$ the *resolution*. The term
$`k_i k_j / 2m`$ is the weight expected between $`i`$ and $`j`$ if edges
were placed at random given each node’s weight: a partition has a high
modularity when clusters gather much more weight than expected by
chance.

## Detecting clusters with Leiden

[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
gives access to five algorithms of igraph. The Leiden algorithm ([Traag
et al. 2019](#ref-traag2019)) is the default choice of recent
bibliometric work. It improves on Louvain ([Blondel et al.
2008](#ref-blondel2008)) by guaranteeing that clusters are connected,
and iterating it keeps refining the partition.

``` r

network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  resolution = 1,
  seed = 123
)
```

Each document gets a `cluster_leiden` identifier and the share of the
network that its cluster represents (`size_cluster_leiden`). Edges get
the cluster of their two ends, or “00” when they connect two clusters.

``` r

node_table <- as_tibble(network, active = "nodes")
sort(table(node_table$cluster_leiden), decreasing = TRUE)
```


    01 02 03 04 05 06
    53 33 17 16 16 12 

### Resolution and the number of clusters

The partition depends on $`\gamma`$: a higher resolution makes the
expected weight larger and favours more, smaller clusters. Modularity
also has a *resolution limit*: in large networks, it can merge small
communities that are clearly separated ([Fortunato
2010](#ref-fortunato2010)). A network can legitimately be described at
several levels of detail, from fields to specialties ([Sjögårde and
Ahlgren 2020](#ref-sjogarde2020)), and choosing the resolution is
choosing the level of the study. A scan of resolutions shows how the
number of clusters reacts.

Leiden is also stochastic: two runs with different seeds can give
different partitions. A partition we can trust should come back from one
seed to the next. The **adjusted Rand index** (ARI) measures the
agreement between two partitions, from about 0 (no more than chance) to
1 (identical). We run Leiden with ten seeds for each resolution and
compare each partition with the one of the first seed.

``` r

scan <- expand.grid(resolution = seq(0.5, 2, by = 0.25), seed = 1:10)
memberships <- lapply(seq_len(nrow(scan)), function(i) {
  clustered <- add_clusters(
    network,
    clustering_method = "leiden",
    objective_function = "modularity",
    resolution = scan$resolution[i],
    seed = scan$seed[i],
    verbose = FALSE
  )
  as.integer(as_tibble(clustered, active = "nodes")$cluster_leiden)
})
scan$n_clusters <- sapply(memberships, function(m) length(unique(m)))
scan$ari <- sapply(seq_len(nrow(scan)), function(i) {
  reference <- which(scan$resolution == scan$resolution[i] & scan$seed == 1)
  igraph::compare(memberships[[i]], memberships[[reference]], method = "adjusted.rand")
})
```

``` r

scan_summary <- aggregate(cbind(n_clusters, ari) ~ resolution, data = scan[scan$seed != 1, ], FUN = mean)
scan_long <- rbind(
  data.frame(resolution = scan_summary$resolution, value = scan_summary$n_clusters, measure = "Mean number of clusters"),
  data.frame(resolution = scan_summary$resolution, value = scan_summary$ari, measure = "Mean ARI with seed 1")
)
ggplot(scan_long, aes(resolution, value)) +
  geom_line(colour = scico::scico(1, palette = "batlow", begin = 0.3), linewidth = 1) +
  geom_point(colour = scico::scico(1, palette = "batlow", begin = 0.3), size = 2.5) +
  facet_wrap(~measure, scales = "free_y") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Resolution", y = NULL) +
  theme_minimal(base_size = 16)
```

![Two panels against the resolution, from 0.5 to 2. Left: the number of
clusters grows from 2 to about 12. Right: the mean adjusted Rand index
between seeds stays close to 1, except a dip to about 0.75 at a
resolution of
1.5.](finding-and-reading-clusters_files/figure-html/fig-resolution-1.png)

Figure 1

The number of clusters grows steadily with the resolution, and most
resolutions give partitions that the seed hardly changes. Where the ARI
drops, the algorithm hesitates between partitions of similar quality:
results at that resolution should be read with care. A reasonable
practice is to choose a resolution in a stable range, to report it, and
to check that the clusters a narrative relies on survive at close values
([Traag et al. 2013](#ref-traag2013)).

### Modularity or CPM?

Leiden can also maximise the **constant Potts model** ([Traag et al.
2011](#ref-traag2011)), which compares the internal weight $`e_c`$ of
each cluster with $`\gamma`$ times its number of pairs:
$`H = \sum_c [e_c - \gamma \binom{n_c}{2}]`$. A cluster is worth
creating when its density exceeds $`\gamma`$. Unlike modularity, CPM
does not depend on the size of the network, which helps when comparing
clusters across networks of very different sizes, as time windows often
are. But $`\gamma`$ then lives on the scale of the edge weights:

``` r

summary(igraph::E(network)$weight)
```

         Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
    1.124e-05 1.019e-04 1.863e-04 3.267e-04 3.438e-04 2.488e-02 

``` r

cpm <- add_clusters(network, clustering_method = "leiden", objective_function = "CPM",
                    resolution = 1, seed = 123, verbose = FALSE)
length(unique(as_tibble(cpm, active = "nodes")$cluster_leiden))
```

    [1] 147

With weights around $`10^{-4}`$, no group of documents is dense enough
for $`\gamma = 1`$, and every document is its own cluster. With CPM, the
resolution must be chosen from the distribution of the weights, for
instance close to the density of the network for an unweighted graph.

## Reading clusters

A cluster is a set of documents with shared references, not an
interpretation. Reading it means going back to its documents: what they
discuss, when, who wrote them, which references they share, and what
distinguishes them from neighbouring clusters ([Ding
2011](#ref-ding2011)).

### Naming clusters with tf-idf

[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
gives each cluster a provisional label. With `method = "tf-idf"`, each
cluster is treated as one document made of the titles of its members,
and its label gathers the terms with the highest tf-idf ([Spärck Jones
1972](#ref-sparckjones1972); [Salton and Buckley
1988](#ref-salton1988)): terms frequent in the cluster and rare in the
others,

``` math
\text{tf-idf}_{tc} = \frac{n_{tc}}{\sum_u n_{uc}} \times \log\frac{C}{\mathrm{df}_t},
```

with $`n_{tc}`$ the count of term $`t`$ in cluster $`c`$, $`C`$ the
number of clusters and $`\mathrm{df}_t`$ the number of clusters where
$`t`$ appears.

``` r

network <- name_clusters(
  network,
  method = "tf-idf",
  cluster_id = "cluster_leiden",
  text_columns = "source_title",
  nb_terms_label = 3,
  clean_word_method = "lemmatize"
)
node_table <- as_tibble(network, active = "nodes")
clusters <- aggregate(
  as.integer(source_year) ~ cluster_leiden + cluster_label,
  data = node_table,
  FUN = function(year) c(documents = length(year), median_year = median(year))
)
clusters <- do.call(data.frame, clusters)
names(clusters)[3:4] <- c("documents", "median_year")
clusters[order(clusters$median_year), ]
```

      cluster_leiden                     cluster_label documents median_year
    6             05 unemployment, import, stagflation        16      1979.0
    1             06        comment, wage, stagflation        12      1979.5
    3             04        import, great, alternative        16      1979.5
    5             02         productivity, oil, supply        33      1982.0
    4             03 oil, oil price, great stagflation        17      2002.0
    2             01      great inflation, great, rule        53      2005.0

The labels and the median years already sketch a history. Clusters of
the late 1970s and early 1980s talk about productivity, oil, wages and
unemployment; clusters centred on the 2000s about the “great inflation”
and policy “rules”. This matches the account of Goutsmedt
([2021](#ref-goutsmedt2021)): supply shocks and inflation inertia
dominated the first explanations, and a new literature centred on the
role of monetary policy emerged in the 1990s. Titles are short, and
their words are not always informative (“great”, “comment”): labels are
only a first guess.
[`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
shows more terms per cluster, and abstracts, when available, give better
labels than titles.

Other methods name a cluster after one of its documents:
`method = "tidygraph_functions"` uses the most central document, for
instance by PageRank, and `method = "given_column"` the document with
the highest value of a column, such as the number of citations received.

### The role of documents: hubs and bridges

Within a cluster, documents do not play the same role. Guimerà and Nunes
Amaral ([2005](#ref-guimera2005)) propose to place each node with two
measures. The **within-module degree** compares the weight $`\kappa_i`$
of the ties of $`i`$ with its own cluster $`s_i`$ with that of the other
members,
$`z_i = (\kappa_i - \bar{\kappa}_{s_i}) / \sigma_{\kappa_{s_i}}`$. The
**participation coefficient** $`P_i = 1 - \sum_s (k_{is} / k_i)^2`$,
with $`k_{is}`$ the weight of the ties of $`i`$ with cluster $`s`$, is 0
when all ties stay in one cluster and approaches 1 when they spread
evenly across clusters.

``` r

network <- add_node_roles(network, module_col = "cluster_leiden")
roles <- as_tibble(network, active = "nodes")
table(roles$role_ga)
```


     R1 ultra-peripheral        R2 peripheral R3 non-hub connector
                       9                  107                   27
      R4 non-hub kinless    R5 provincial hub     R6 connector hub
                       1                    2                    1 

``` r

ggplot(roles, aes(participation_coeff, within_module_z, colour = cluster_label)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = Inf, alpha = 0.08) +
  geom_point(size = 2.5, alpha = 0.8) +
  scico::scale_colour_scico_d(palette = "batlow") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Participation coefficient", y = "Within-module degree (z-score)", colour = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 2))
```

![Scatter plot of the within-module degree z-score against the
participation coefficient for each document, coloured by cluster. Most
documents have z-scores between -2 and 2; a handful of documents above
2.5 are hubs of their
cluster.](finding-and-reading-clusters_files/figure-html/fig-roles-1.png)

Figure 2

The shaded area holds the hubs of their cluster ($`z \geq 2.5`$, the
threshold of Guimerà and Amaral). Documents on the right have ties
spread over several clusters: they are good candidates to understand how
literatures communicate. In small networks such as this one, the seven
categories of Guimerà and Amaral are thinly populated, and the two
measures are more useful as continuous indicators than as roles.

## Drawing the network

Plotting needs coordinates, colours and label positions.
[`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
computes coordinates with a force-directed layout: here
Fruchterman-Reingold ([Fruchterman and Reingold
1991](#ref-fruchterman1991)), which balances an attraction along edges,
stronger for heavier edges, with a repulsion between all nodes.
Kamada-Kawai (`"kk"`, Kamada and Kawai ([1989](#ref-kamada1989))), which
draws graph distances as geometric distances, is another common choice,
with a trap: igraph reads the `weight` column as a *distance* for this
layout, so that similarity weights pull weakly tied documents to the
margins. With similarity weights, either use `"fr"` or ignore the
weights (`weights = NA`).
[`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
gives each cluster a colour, and edges a mix of the colours of their
ends.
[`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
assembles the figure.

``` r

set.seed(1)
network <- layout_networks(network, node_id = "source_id", layout = "fr")
n_clusters <- length(unique(roles$cluster_leiden))
palette <- data.frame(
  cluster_leiden = sort(unique(roles$cluster_leiden)),
  color = scico::scico(n_clusters, palette = "batlow")
)
network <- color_networks(network, column_to_color = "cluster_leiden", color = palette)
plot_networks(
  network,
  cluster_label_column = "cluster_label",
  node_size_column = NULL
)
```

![Bibliographic coupling network of the documents explaining the 1970s
stagflation, with nodes coloured by Leiden cluster and a label at the
centre of each cluster made of its three most distinctive title
words.](finding-and-reading-clusters_files/figure-html/fig-network-1.png)

Figure 3

Clusters drawn next to each other share more references than clusters at
opposite ends, but distances in such a figure should not be measured:
the layout is a compromise between many constraints, and another seed
gives another, equally valid, picture.

For a closer reading,
[`launch_network_app()`](https://agoutsmedt.github.io/networkflow/reference/launch_network_app.md)
opens the same network in an interactive application: clicking a cluster
lists its documents with their authors, titles and journals.

``` r

launch_network_app(
  graph_tbl = network,
  cluster_id = "cluster_label",
  cluster_information = c("source_author", "source_title", "source_year", "source_journal"),
  node_id = "source_id",
  node_tooltip = "source_label",
  color = "color",
  layout = NULL
)
```

## What comes next

A single network of the whole period mixes literatures that did not
coexist. The [last
guide](https://agoutsmedt.github.io/networkflow/articles/following-clusters-over-time.md)
builds one network per time window and follows the clusters across
windows.

## References

Blondel, Vincent D., Jean-Loup Guillaume, Renaud Lambiotte, and Etienne
Lefebvre. 2008. “Fast Unfolding of Communities in Large Networks.”
*Journal of Statistical Mechanics: Theory and Experiment* 2008 (10):
P10008. <https://doi.org/10.1088/1742-5468/2008/10/P10008>.

Boyack, Kevin W., and Richard Klavans. 2010. “Co-Citation Analysis,
Bibliographic Coupling, and Direct Citation: Which Citation Approach
Represents the Research Front Most Accurately?” *Journal of the American
Society for Information Science and Technology* 61 (12): 2389–404.
<https://doi.org/10.1002/asi.21419>.

Ding, Ying. 2011. “Community Detection: Topological Vs. Topical.”
*Journal of Informetrics* 5 (4): 498–514.
<https://doi.org/10.1016/j.joi.2011.02.006>.

Fortunato, Santo. 2010. “Community Detection in Graphs.” *Physics
Reports* 486 (3): 75–174.
<https://doi.org/10.1016/j.physrep.2009.11.002>.

Fruchterman, Thomas M. J., and Edward M. Reingold. 1991. “Graph Drawing
by Force-Directed Placement.” *Software: Practice and Experience* 21
(11): 1129–64. <https://doi.org/10.1002/spe.4380211102>.

Goutsmedt, Aurélien. 2021. “From the Stagflation to the Great Inflation:
Explaining the US Economy of the 1970s.” *Revue d’Économie Politique*
131 (3): 557–82.
<https://www.cairn.info/revue-d-economie-politique-2021-3-page-557.htm>.

Guimerà, Roger, and Luís A. Nunes Amaral. 2005. “Functional Cartography
of Complex Metabolic Networks.” *Nature* 433 (7028): 895–900.
<https://doi.org/10.1038/nature03288>.

Kamada, Tomihisa, and Satoru Kawai. 1989. “An Algorithm for Drawing
General Undirected Graphs.” *Information Processing Letters* 31 (1):
7–15. <https://doi.org/10.1016/0020-0190(89)90102-6>.

Morris, Steven A., and Betsy Van der Veer Martens. 2008. “Mapping
Research Specialties.” *Annual Review of Information Science and
Technology* 42 (1): 213–95.
<https://doi.org/10.1002/aris.2008.1440420113>.

Newman, M. E. J., and M. Girvan. 2004. “Finding and Evaluating Community
Structure in Networks.” *Physical Review E* 69 (2): 026113.
<https://doi.org/10.1103/PhysRevE.69.026113>.

Salton, Gerard, and Christopher Buckley. 1988. “Term-Weighting
Approaches in Automatic Text Retrieval.” *Information Processing &
Management* 24 (5): 513–23.

Sjögårde, Peter, and Per Ahlgren. 2020. “Granularity of Algorithmically
Constructed Publication-Level Classifications of Research Publications:
Identification of Specialties.” *Quantitative Science Studies* 1 (1):
207–38. <https://doi.org/10.1162/qss_a_00004>.

Spärck Jones, Karen. 1972. “A Statistical Interpretation of Term
Specificity and Its Application in Retrieval.” *Journal of
Documentation* 28 (1): 11–21.

Traag, V. A., G. Krings, and P. Van Dooren. 2013. “Significant Scales in
Community Structure.” *Scientific Reports* 3: 2930.
<https://doi.org/10.1038/srep02930>.

Traag, V. A., P. Van Dooren, and Y. Nesterov. 2011. “Narrow Scope for
Resolution-Limit-Free Community Detection.” *Physical Review E* 84 (1):
016114. <https://doi.org/10.1103/PhysRevE.84.016114>.

Traag, V. A., L. Waltman, and N. J. van Eck. 2019. “From Louvain to
Leiden: Guaranteeing Well-Connected Communities.” *Scientific Reports* 9
(1): 5233. <https://doi.org/10.1038/s41598-019-41695-z>.
