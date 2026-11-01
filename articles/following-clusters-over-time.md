# Following clusters over time

The previous guides built [one
network](https://agoutsmedt.github.io/networkflow/articles/from-references-to-networks.md)
and [found its
clusters](https://agoutsmedt.github.io/networkflow/articles/finding-and-reading-clusters.md).
A single network of a long period, however, puts side by side
literatures that never coexisted, and hides how a literature is born,
grows, splits or fades. This guide follows the dynamic workflow that is
the core of networkflow: one network per time window, clusters in each
window, and a rule that links the clusters of successive windows into
*dynamic clusters* whose history can be told and drawn.

## One network per window

We build bibliographic coupling networks of the explanations of the
stagflation on sliding ten-year windows (1975-1984, 1976-1985…,
2004-2013), keep the main component of each, and detect clusters in each
window with Leiden.

``` r

library(networkflow)
library(ggplot2)
library(tidygraph)

nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
networks <- build_dynamic_networks(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "source_year",
  time_window = 10,
  overlapping_window = TRUE,
  projection_method = "structured",
  cooccurrence_method = "coupling_similarity",
  edges_threshold = 1,
  filter_components = TRUE,
  verbose = FALSE
)
networks <- add_clusters(
  networks,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)
n_window_clusters <- sum(sapply(networks, function(graph) {
  length(unique(as_tibble(graph, active = "nodes")$cluster_leiden))
}))
c(windows = length(networks), clusters = n_window_clusters)
```

     windows clusters
          30      104 

Each window has its own clusters, numbered “01”, “02”… These numbers
mean nothing across windows: cluster “01” of 1975-1984 and cluster “01”
of 1976-1985 may be two different literatures.

## Linking clusters across windows

### The rule

[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
links each cluster to a cluster of the previous window when they share
most of their documents. Let $`V_a`$ be the documents of cluster $`a`$
in window $`t-1`$, and $`V_b`$ those of cluster $`b`$ in window $`t`$.
Cluster $`b`$ inherits the identifier of $`a`$ when a majority of $`a`$
goes to $`b`$*and* a majority of $`b`$ comes from $`a`$:

``` math
\frac{|V_a \cap V_b|}{D_a} > \tau
\quad \text{and} \quad
\frac{|V_a \cap V_b|}{D_b} > \tau,
```

with $`\tau`$ the `threshold_similarity`, strictly above 0.5 so that
each cluster inherits at most one identifier and passes it to at most
one cluster. The denominators depend on `similarity_type`:

- `"complete"`: $`D_a = |V_a|`$ and $`D_b = |V_b|`$, all the documents
  of both clusters;
- `"partial"`: only the documents present in both windows count,
  $`D_a = |V_a \cap V_t|`$ and $`D_b = |V_b \cap V_{t-1}|`$.

With sliding windows, window $`t`$ loses the documents of the first year
of window $`t-1`$ and gains those of a new year. With `"complete"`, a
cluster that recruits many new documents, or loses many old ones, may
lose its identifier because its composition changed. With `"partial"`,
only the documents present in both windows are compared, and a cluster
that keeps its members together keeps its identifier however many
documents join it. The first view treats a cluster as a group of
documents, the second as a structure that recruits documents: which one
fits depends on the question.

The rule needs documents present in successive windows. With successive
windows (`overlapping_window = FALSE`), a network of documents shares no
document from one window to the next (each document has a single year),
and no cluster can be matched: sliding windows are necessary here.

``` r

window_networks <- networks # kept to compare other settings below
networks <- merge_dynamic_clusters(
  window_networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  threshold_similarity = 0.51,
  similarity_type = "partial"
)
```

### How many dynamic clusters?

The threshold and the type of similarity decide how easily clusters are
linked, hence how many distinct dynamic clusters the history contains.

``` r

settings <- expand.grid(
  threshold = c(0.51, 0.6, 0.7, 0.8),
  similarity_type = c("complete", "partial"),
  stringsAsFactors = FALSE
)
settings$dynamic_clusters <- mapply(function(threshold, type) {
  merged <- merge_dynamic_clusters(
    window_networks,
    cluster_id = "cluster_leiden",
    node_id = "source_id",
    threshold_similarity = threshold,
    similarity_type = type
  )
  length(unique(unlist(lapply(merged, function(graph) {
    as_tibble(graph, active = "nodes")$dynamic_cluster_leiden
  }))))
}, settings$threshold, settings$similarity_type)
settings
```

      threshold similarity_type dynamic_clusters
    1      0.51        complete               21
    2      0.60        complete               33
    3      0.70        complete               46
    4      0.80        complete               62
    5      0.51         partial               15
    6      0.60         partial               23
    7      0.70         partial               30
    8      0.80         partial               41

The 104 window clusters reduce to a few dozen dynamic clusters or fewer.
A stricter threshold breaks more continuities, and `"partial"` links
more clusters than `"complete"`, since it ignores the documents that
enter or leave the corpus. Studies that used networkflow chose
thresholds just above 0.5 with partial similarity: 0.501 for Delcey et
al. ([2026](#ref-delcey2026)), 0.55 for Duluins et al.
([2026](#ref-duluins2026)). Whatever the choice, the continuities that a
narrative relies on should survive close thresholds, and a different
seed of the clustering.

The rule has limits worth knowing. It compares a cluster only with the
previous window, so a cluster that vanishes for one window and comes
back gets a new identifier. Splits and merges are not recorded as such:
the part that meets the double majority keeps the identifier, the others
are new clusters. Methods that model these events explicitly, such as
the alluvial diagrams of Rosvall and Bergstrom
([2010](#ref-rosvall2010)) based on significance clustering, are an
alternative for large networks.

## Naming dynamic clusters

Once clusters are linked, each dynamic cluster can be named on all its
documents across windows (`name_merged_clusters = TRUE`), with the words
of the titles that distinguish it from the other dynamic clusters.

``` r

networks <- name_clusters(
  networks,
  method = "tf-idf",
  name_merged_clusters = TRUE,
  cluster_id = "dynamic_cluster_leiden",
  text_columns = "source_title",
  nb_terms_label = 3,
  clean_word_method = "lemmatize"
)
```

## Drawing the history: alluvial plots

An alluvial plot shows the dynamic clusters as blocks in each window,
with a height proportional to their share of the window, and flows that
follow the documents from one window to the next.
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)
stacks the node tables into the long table such a plot needs, with
statistics on each cluster: its share of the whole history, its maximum
share in a window, and the number of windows where it exists.

``` r

alluv_dt <- networks_to_alluv(
  networks,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id"
)
cluster_stats <- unique(alluv_dt[, c("dynamic_cluster_leiden", "cluster_label",
                                     "share_cluster_max", "length_cluster")])
cluster_stats[order(-cluster_stats$length_cluster), ]
```

        dynamic_cluster_leiden                                      cluster_label
                        <char>                                             <char>
     1:                  cl_52               rule, great inflation, stabilization
     2:                  cl_53                   rise, great inflation, peacetime
     3:                  cl_67 monetary alternative, oil price, great stagflation
     4:                   cl_1          productivity, productivity growth, growth
     5:                   cl_3                  control, control program, program
     6:                  cl_50                change, time consistent, consistent
     7:                   cl_4 acceleration, acceleration evidence, postwar unite
     8:                  cl_31            shock wage, wage stickiness, stickiness
     9:                   cl_2                 unemployment, import, import price
    10:                   cl_5            inflation employment, employment, stein
    11:                   cl_6        wage, wage profit, macroeconomic adjustment
    12:                  cl_46            great, disinflation, great disinflation
    13:                  cl_38                social, social consensus, consensus
    14:                  cl_30                international, energy, energy price
    15:                  cl_35                        world inflation, oil, world
        share_cluster_max length_cluster
                    <num>          <int>
     1:             55.26             18
     2:             50.00             17
     3:             37.04             13
     4:             39.06             10
     5:             22.22              7
     6:             62.50              7
     7:             14.58              6
     8:             42.86              6
     9:             26.56              5
    10:              5.36              4
    11:             35.42              4
    12:             57.14              3
    13:             33.33              2
    14:             17.50              1
    15:             22.22              1

Clusters that exist in one or two windows, or remain small, clutter the
plot. We keep the clusters that last at least three windows, give each a
fixed colour, order the clusters to limit crossing flows
(`minimize_crossing = TRUE`) and place each label in the middle of the
cluster’s life (`prepare_label = TRUE`).

``` r

main_alluv <- alluv_dt[alluv_dt$length_cluster >= 3, ]
main_clusters <- sort(unique(main_alluv$dynamic_cluster_leiden))
palette <- data.frame(
  dynamic_cluster_leiden = main_clusters,
  color = scico::scico(length(main_clusters), palette = "batlow")
)
plot_alluvial(
  main_alluv,
  intertemporal_cluster_column = "dynamic_cluster_leiden",
  node_id = "source_id",
  color_alluvial = TRUE,
  color = palette,
  minimize_crossing = TRUE,
  prepare_label = TRUE,
  cluster_label_column = "cluster_label"
) +
  labs(x = NULL, y = "Share of the window") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![Alluvial plot of the dynamic clusters of explanations of the
stagflation, from the 1975-1984 window to the 2004-2013 window. Several
clusters dominate the first windows and disappear in the late 1980s;
three clusters appear in the 1990s and last until the
end.](following-clusters-over-time_files/figure-html/fig-alluvial-1.png)

Figure 1

The plot draws the history that Goutsmedt ([2021](#ref-goutsmedt2021))
tells with qualitative and quantitative methods. The early windows are
shared between clusters on wages, productivity, controls and
unemployment, the explanations of the late 1970s and early 1980s,
centred on supply shocks and inflation inertia. These clusters fade in
the late 1980s, when windows contain few documents. In the late 1980s
and early 1990s, clusters on disinflation and on time-consistent policy
bridge the two periods. From the 1990s, new clusters appear, on the
“great inflation” and policy rules, on peacetime inflation, and on oil
prices and monetary alternatives, and last until the end of the period:
a new literature, centred on monetary policy, in which the contributions
of new classical economists, ignored by the first explanations, became
major references ([Goutsmedt 2021](#ref-goutsmedt2021)).

Reading the plot requires care. Flows between clusters are documents
that change cluster when the window slides, which may reflect a change
in the literature or simply the instability of a partition of a small
network. Windows of a dozen documents give fragile clusters: their part
of the plot should be read with the documents in hand.

## Coordinates that follow the documents

To draw the networks of successive windows side by side,
`layout_networks(compute_dynamic_coordinates = TRUE)` starts the layout
of each window from the coordinates of the previous one, so that a
document stays in the same region from one plot to the next. Giving the
dynamic clusters the same colours as in the alluvial
([`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
with the same palette) completes the picture: a cluster can be followed
in the alluvial and located in each network.

``` r

set.seed(1)
networks <- layout_networks(
  networks,
  node_id = "source_id",
  layout = "fr",
  compute_dynamic_coordinates = TRUE
)
networks <- color_networks(networks, column_to_color = "dynamic_cluster_leiden", color = palette)
```

## In research

The dynamic workflow was designed for histories of research fields,
where the question is less “what are the communities?” than “when did a
literature appear, what did it come from, and what did it become?”.
Goutsmedt and Truc ([2023](#ref-goutsmedt2023eer)) use bibliographic
coupling and topic models to compare the macroeconomics published in the
*European Economic Review* with the top five journals, between 1969 and
2002. Delcey et al. ([2026](#ref-delcey2026)) follow the discussions of
rationality in a corpus of nearly 260,000 economics articles from 1900
to 2009. Duluins et al. ([2026](#ref-duluins2026)) apply the same tools
to the scientific reception of a report on the environmental impact of
livestock. In each case, the networks do not replace reading: they tell
the historian which documents to read, and which continuities and breaks
need an explanation.

## References

Delcey, Thomas, Aurélien Goutsmedt, and Alexandre Truc. 2026. “One
Sentence at a Time: A Quantitative History of Rationality in Economic
Thought.” *European Journal of the History of Economic Thought*.

Duluins, Océane, Aurélien Goutsmedt, Noé Vandevoorde, and Philippe V.
Baret. 2026. “A Scientometric Retrospective of the Livestock Long Shadow
Report.” *Frontiers in Sustainable Food Systems* 9: 1656562.
<https://doi.org/10.3389/fsufs.2025.1656562>.

Goutsmedt, Aurélien. 2021. “From the Stagflation to the Great Inflation:
Explaining the US Economy of the 1970s.” *Revue d’Économie Politique*
131 (3): 557–82.
<https://www.cairn.info/revue-d-economie-politique-2021-3-page-557.htm>.

Goutsmedt, Aurélien, and Alexandre Truc. 2023. “An Independent European
Macroeconomics? A History of European Macroeconomics Through the Lens of
the European Economic Review.” *European Economic Review* 158: 104559.
<https://doi.org/10.1016/j.euroecorev.2023.104559>.

Rosvall, Martin, and Carl T. Bergstrom. 2010. “Mapping Change in Large
Networks.” *PLoS ONE* 5 (1): e8694.
<https://doi.org/10.1371/journal.pone.0008694>.
