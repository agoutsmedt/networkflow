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
  window_step = 1,
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
          30      110 

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
windows (the default of
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)),
a network of documents shares no document from one window to the next
(each document has a single year), and no cluster can be matched:
sliding windows (`window_step = 1`) are necessary here.

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

### How sensitive are the dynamic clusters to the threshold?

The threshold and the type of similarity decide how easily clusters are
linked, hence how many dynamic clusters the history contains and how
long they live.
[`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
runs the matching for a grid of thresholds and both similarity types,
and
[`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
draws the result.

``` r

diagnostics <- merge_diagnostics(
  window_networks,
  cluster_id = "cluster_leiden",
  node_id = "source_id",
  thresholds = c(0.51, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9),
  min_length = 3,
  verbose = FALSE
)
diagnostics[diagnostics$similarity_type == "partial",
            c("threshold", "n_dynamic_clusters", "share_continued", "median_lifespan",
              "n_persistent", "share_nodes_persistent")]
```

       threshold n_dynamic_clusters share_continued median_lifespan n_persistent
    2       0.51                 19       0.8750000             4.0           11
    4       0.55                 20       0.8653846             3.5           11
    6       0.60                 25       0.8173077             2.0           11
    8       0.65                 29       0.7788462             2.0           11
    10      0.70                 35       0.7211538             2.0           13
    12      0.75                 40       0.6730769             2.0           13
    14      0.80                 45       0.6250000             2.0           11
    16      0.90                 54       0.5384615             1.0           10
       share_nodes_persistent
    2               0.9101917
    4               0.8869828
    6               0.7981837
    8               0.7436932
    10              0.7356206
    12              0.7033300
    14              0.6054490
    16              0.4964682

``` r

plot_merge_diagnostics(diagnostics)
```

[![Four panels against the threshold, from 0.51 to 0.9, one line for
complete and one for partial similarity. The number of dynamic clusters
rises with the threshold, the share of window clusters with a
predecessor, the median lifespan and the share of nodes in clusters of
three windows or more fall; complete similarity is more
sensitive.](following-clusters-over-time_files/figure-html/fig-diagnostics-1.png)](https://agoutsmedt.github.io/networkflow/articles/following-clusters-over-time_files/figure-html/fig-diagnostics-1.png "Figure 1: ")

Figure 1

The 110 window clusters become 25 dynamic clusters at 0.51 and 89 at 0.9
with the complete similarity (19 and 54 with the partial one): the more
demanding the threshold, the more clusters are considered new. The
curves are flat between 0.51 and 0.55, so that values just above 0.5 are
on a plateau: the continuities they produce do not depend on the exact
threshold. They bend after 0.55, and the second view below shows what is
lost. With the complete similarity, the median lifespan falls to one
window at 0.75: every window then has its own clusters, which is the
opposite of a history. The partial similarity is more tolerant, because
it ignores the documents that enter or leave the corpus from one window
to the next. This is the reason for the thresholds just above 0.5 with
partial similarity in the studies that used networkflow: 0.501 for
Delcey et al. ([2026](#ref-delcey2026)), 0.55 for Duluins et al.
([2026](#ref-duluins2026)). Whatever the choice, the continuities that a
narrative relies on should survive close thresholds, and a different
seed of the clustering.

``` r

plot_merge_diagnostics(diagnostics, view = "lifespans")
```

[![Stacked bars, one per threshold, for complete and partial similarity,
showing the share of the nodes of all windows that belong to dynamic
clusters living one window, two, three to five, six to ten, or more than
ten. The share of one-window clusters grows with the
threshold.](following-clusters-over-time_files/figure-html/fig-lifespans-1.png)](https://agoutsmedt.github.io/networkflow/articles/following-clusters-over-time_files/figure-html/fig-lifespans-1.png "Figure 2: ")

Figure 2

The second view shows who pays for a high threshold: the nodes of long
clusters, which become members of short ones. With the complete
similarity, the clusters that live more than ten windows gather about
half of the nodes at 0.55 but 13% at 0.6, and none at 0.65: the longest
continuities of the history are cut into shorter pieces between 0.55 and
0.65. With the partial similarity, such clusters still hold about 13% of
the nodes at 0.9. The sensitivity of a continuity that matters for the
story, here between 0.55 and 0.6, is exactly what the diagnostics are
meant to reveal before it is hidden in a figure.

### Other ways to follow clusters

The rule of
[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
is simple and transparent, with limits worth knowing. It compares a
cluster only with the previous window, so a cluster that vanishes for
one window and comes back gets a new identifier. Splits and merges are
not recorded as such: the part that meets the double majority keeps the
identifier, the others are new clusters. And the clusters of each window
are detected independently, which adds instability between windows.
Other approaches exist, and some could be tested on the same data:

| Approach | Idea | Compared with the double majority |
|----|----|----|
| Overlap matching with a Jaccard threshold ([Greene et al. 2010](#ref-greene2010)) | Match the clusters of successive windows whose Jaccard similarity $`\lvert V_a \cap V_b \rvert / \lvert V_a \cup V_b \rvert`$ exceeds a threshold, and chain the matches into *dynamic communities*; splits and merges show up as one cluster matched to several. | Symmetric by construction, and gives a name to events; a one-to-one assignment (best match first) could replace the majority rule. |
| Multislice community detection ([Mucha et al. 2010](#ref-mucha2010)) | Detect communities in all windows at once, in a network where each node is tied to itself in adjacent windows with a coupling weight $`\omega`$. Identifiers are shared by construction. | The coupling $`\omega`$ plays the role of the threshold, but clusters of one window are influenced by the neighbouring windows: more stable, less independent. It needs nodes present in several windows (co-citation, authors, journals rather than documents). |
| Significance-clustered alluvial maps ([Rosvall and Bergstrom 2010](#ref-rosvall2010)) | Bootstrap the network to find which clusters are robust, then draw the flows between robust clusters. | Addresses the instability of each partition rather than the matching rule; heavier. |

Two simpler checks are available with the tools of the package: running
the whole pipeline with several seeds of the clustering and comparing
the dynamic clusters with
[`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md),
and varying the window and its step (see the [first
guide](https://agoutsmedt.github.io/networkflow/articles/from-references-to-networks.md)),
since instability often comes from windows that are too short to contain
a stable structure. Implementing the first two approaches in the
package, as alternatives to the double majority with the same output
columns, would allow comparing them on real corpora.

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

        dynamic_cluster_leiden
                        <char>
     1:                  cl_50
     2:                  cl_53
     3:                   cl_4
     4:                   cl_1
     5:                  cl_62
     6:                  cl_43
     7:                   cl_2
     8:                  cl_80
     9:                  cl_12
    10:                  cl_15
    11:                  cl_99
    12:                   cl_3
    13:                   cl_6
    14:                  cl_38
    15:                   cl_5
    16:                  cl_29
    17:                  cl_34
    18:                 cl_103
    19:                 cl_107
                                                  cluster_label share_cluster_max
                                                         <char>             <num>
     1:                    oil price, oil, monetary alternative             37.50
     2:                    rise, peacetime, peacetime inflation             40.74
     3:             policy revisit, shock wage, wage stickiness             42.86
     4:               productivity, productivity growth, growth             35.71
     5:                       rule, trap hypothesis, hypothesis             30.56
     6:                           north, north america, america            100.00
     7:                       control, control program, program             22.22
     8:                          origin, great inflation, great             24.49
     9:      acceleration, acceleration evidence, postwar unite             18.03
    10:        unemployment, employment, stagflation persistent             21.43
    11:                          great inflation, great, origin             54.76
    12:             unemployment, inflation unemployment, model             24.59
    13:                employment, inflation employment, import             14.75
    14: comparative, comparative macroeconomics, macroeconomics             33.33
    15:                               acceleration, wage, price             10.94
    16:                  international, world inflation, energy             12.50
    17:                             world inflation, oil, world             22.22
    18:                              rule, policy rule, natural             15.91
    19:                               rule, taylor, taylor rule             31.58
        length_cluster
                 <int>
     1:             18
     2:             17
     3:             11
     4:             10
     5:             10
     6:              9
     7:              7
     8:              5
     9:              4
    10:              4
    11:              4
    12:              2
    13:              2
    14:              2
    15:              1
    16:              1
    17:              1
    18:              1
    19:              1

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

[![Alluvial plot of the dynamic clusters of explanations of the
stagflation, from the 1975-1984 window to the 2004-2013 window. Several
clusters dominate the first windows and disappear in the late 1980s;
three clusters appear in the 1990s and last until the
end.](following-clusters-over-time_files/figure-html/fig-alluvial-1.png)](https://agoutsmedt.github.io/networkflow/articles/following-clusters-over-time_files/figure-html/fig-alluvial-1.png "Figure 3: ")

Figure 3

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

Greene, Derek, Dónal Doyle, and Pádraig Cunningham. 2010. “Tracking the
Evolution of Communities in Dynamic Social Networks.” *2010
International Conference on Advances in Social Networks Analysis and
Mining*, 176–83. <https://doi.org/10.1109/ASONAM.2010.17>.

Mucha, Peter J., Thomas Richardson, Kevin Macon, Mason A. Porter, and
Jukka-Pekka Onnela. 2010. “Community Structure in Time-Dependent,
Multiscale, and Multiplex Networks.” *Science* 328 (5980): 876–78.
<https://doi.org/10.1126/science.1184819>.

Rosvall, Martin, and Carl T. Bergstrom. 2010. “Mapping Change in Large
Networks.” *PLoS ONE* 5 (1): e8694.
<https://doi.org/10.1371/journal.pone.0008694>.
