# networkflow

`networkflow` is a research pipeline for studying networks built from
tabular data. Research data rarely come as networks: they come as tables
of links between two kinds of entities, such as articles and the
references they cite, authors and the papers they sign, or even social
media accounts and what they share. `networkflow` turns such tables into
networks, finds the communities they contain, follows these communities
over time, and helps name, draw and explore them.

The package was written for, and used in, research on the history and
sociology of sciences, where bibliographic coupling networks (articles
linked by the references they share) map the research fronts of a
literature ([Kessler 1963](#ref-kessler1963); [Boyack and Klavans
2010](#ref-boyack2010)).

Its central feature is the *dynamic* workflow: one network per time
window, clusters in each window, and a rule that links the clusters of
successive windows (building “intertemporal” or “dynamic” clusters), so
that a literature can be seen being born, growing, splitting and fading.
Each step is a documented function whose choices (similarity measure,
threshold, time window, clustering algorithm, resolution, matching rule)
are explicit arguments that can be varied and reported. Everything
returns `tbl_graph` objects from
[`tidygraph`](https://tidygraph.data-imaginist.com/), or lists of them,
which can be manipulated with [`dplyr`](https://dplyr.tidyverse.org/)
and drawn with [`ggraph`](https://ggraph.data-imaginist.com/).

The pipeline is not specific to citations: any table of links between
two kinds of entities can be projected onto one of them. Besides
bibliographic coupling, the guides show co-citation (cited works linked
when they are cited together), the coupling of authors instead of
articles, and co-authorship; the same logic applies to accounts and the
hashtags they use, or to members and the boards they sit on.

## Installation

You can install the development version of `networkflow` from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("agoutsmedt/networkflow")
```

The package imports only what the core pipeline needs. Some steps use
packages that are suggested, not imported, and the function that needs
one asks to install it:
[`ggalluvial`](https://corybrunson.github.io/ggalluvial/) for
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md),
[`tidytext`](https://juliasilge.github.io/tidytext/) for the tf-idf
names of
[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
and
[`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md),
[`textstem`](https://github.com/trinker/textstem) to lemmatise terms,
[`backbone`](https://github.com/zpneal/backbone) for the statistical
backbones (`method = "sdsm"`…).

## A first look

The package includes the corpus of Goutsmedt
([2021](#ref-goutsmedt2021)): the books and articles that explained the
US stagflation of the 1970s, published between 1975 and 2013, and the
references they cite.
[`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
links two explanations when they cite the same references (bibliographic
coupling),
[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
detects groups of explanations that share a literature, and
[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
labels each group with the words that distinguish its titles.

``` r

library(networkflow)

nodes <- subset(Nodes_stagflation, source_type == "Stagflation")

network <- build_network(
  nodes = nodes,
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 1
)

network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

network <- name_clusters(
  network,
  method = "tf-idf",
  cluster_id = "cluster_leiden",
  text_columns = "source_title",
  nb_terms_label = 3,
  clean_word_method = "lemmatize"
)
```

[`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md),
[`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md)
and
[`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
then draw it:

![Bibliographic coupling network of the explanations of the 1970s
stagflation, with nodes coloured by cluster and a label made of the
three most distinctive title words of each
cluster.](reference/figures/README-first-plot-light.png#gh-light-mode-only)![Bibliographic
coupling network of the explanations of the 1970s stagflation, with
nodes coloured by cluster and a label made of the three most distinctive
title words of each
cluster.](reference/figures/README-first-plot-dark.png#gh-dark-mode-only)

The clusters already sketch the history that Goutsmedt
([2021](#ref-goutsmedt2021)) tells: explanations of the late 1970s
centred on productivity, oil and wages, and a literature of the 2000s on
the “great inflation” and monetary policy rules. The guides below show
how to follow this change window by window.

## Guides

Five guides, written around the same example, follow the steps of a
study. Each explains the method behind the step, with the literature it
comes from, before showing the code.

- [From references to
  networks](https://agoutsmedt.github.io/networkflow/articles/from-references-to-networks.html)
  explains what a projected network is, how bibliographic coupling is
  measured, which threshold or statistical backbone to choose, and how
  to cut a corpus into time windows.
- [Finding and reading
  clusters](https://agoutsmedt.github.io/networkflow/articles/finding-and-reading-clusters.html)
  presents community detection with the Leiden algorithm, the role of
  the resolution and of the random seed, and how to name clusters and
  characterise the role of their members.
- [Drawing
  networks](https://agoutsmedt.github.io/networkflow/articles/drawing-networks.html)
  explains what a layout does and compares the force-directed layouts
  (Fruchterman-Reingold, Kamada-Kawai, ForceAtlas2) with UMAP, then
  shows how to colour, label and draw the clusters.
- [Following clusters over
  time](https://agoutsmedt.github.io/networkflow/articles/following-clusters-over-time.html)
  links the clusters of successive windows into dynamic clusters and
  draws their history as an alluvial plot.
- [Beyond bibliographic
  coupling](https://agoutsmedt.github.io/networkflow/articles/beyond-bibliographic-coupling.html)
  uses the same pipeline for co-citation, for authors instead of
  articles and for any two-mode table, and explains why the identifiers
  of references matter.

## What is in the package

**Building networks.**
[`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
and
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
project a two-mode table (e.g. articles and their references) onto a
network of the first entities, for the whole corpus or one network per
time window (in years, months or days, sliding by any step). Ties are
weighted with one of the five bibliometric similarity measures of
[`biblionetwork`](https://agoutsmedt.github.io/biblionetwork/)
([Goutsmedt, Truc, and Claveau 2023](#ref-goutsmedt2023biblionetwork)),
or kept only when they are statistically significant under a null model,
with the [`backbone`](https://github.com/zpneal/backbone) package ([Neal
2022](#ref-neal2022)).
[`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
keeps the main component(s).

**Finding and reading clusters.**
[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
detects communities with Leiden ([Traag, Waltman, and Eck
2019](#ref-traag2019)), Louvain, Infomap, walktrap or fast greedy.
[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
links the clusters of successive windows, and
[`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
shows how the result depends on the threshold.
[`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
and
[`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
describe them by their most distinctive words or most central members,
and
[`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
places each node with the within-module degree and participation
coefficient of Guimerà and Nunes Amaral ([2005](#ref-guimera2005)).

**Drawing and exploring.**
[`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
computes coordinates, with an option to keep them stable across windows;
[`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md),
[`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
and
[`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
draw networks;
[`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
[`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
and
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
draw the history of dynamic clusters.

The
[reference](https://agoutsmedt.github.io/networkflow/reference/index.html)
lists every function.

## Speed

`bench/benchmark_functions.R` runs the dynamic pipeline (six sliding
five-year windows) on samples of a Scopus corpus of economics articles
(1995-2004) and compares it with the former version of the package. On
20,000 articles, building the six networks takes about 26 s and 1.1 GB,
and Leiden with a fixed 1,000 iterations (the former default) about 200
s. The default is now `n_iterations = -1` (iterate until the partition
is stable), which gives the same modularity in a fraction of that time.
Colouring edges is now about 90 times faster (0.7 s instead of 59 s).
The other steps take a few seconds, and the results are identical to
those of the former version, except for the intended changes listed in
`NEWS.md`.

## Research using networkflow

`networkflow` grew out of, and was used in, the following studies:

- Goutsmedt ([2021](#ref-goutsmedt2021)) on the explanations of the
  1970s US stagflation, whose data are the examples of the package;
- Goutsmedt and Truc ([2023](#ref-goutsmedt2023eer)) on European
  macroeconomics and the *European Economic Review*;
- Delcey ([forthcoming](#ref-delcey2027dissemination)) on the
  bibliometric analysis of the dissemination of Fama (1970);
- Delcey, Goutsmedt, and Truc ([2026](#ref-delcey2026)) on the history
  of rationality in economic thought;
- Duluins et al. ([2026](#ref-duluins2026)) on the scientific reception
  of the *Livestock’s Long Shadow* report;
- the [Digital History of
  Macroeconomics](https://www.digitalhistoryofscience.org/dhm/) project
  ([Goutsmedt and Truc 2025](#ref-goutsmedt2025dhm)), which maps the
  macroeconomics literature since the 1970s and whose deliverable is
  archived on Zenodo;
- Boucher and Fan ([2025](#ref-boucher2025)) on productivity and
  citations in the field of contemporary China studies.

## Authors

`networkflow` was developed by Aurélien Goutsmedt, Thomas Delcey and
Alexandre Truc. Alexandre Truc initiated the alluvial method for
following clusters over time
([`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
[`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md),
[`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)).
The package is maintained by Thomas Delcey and Aurélien Goutsmedt.

## Citation

If you use `networkflow` in your research, please cite it:

``` R
Goutsmedt A, Truc A, Delcey T (2026). _networkflow: A Research Pipeline
for Projected and Dynamic Networks_. R package version 0.0.9.9001,
<https://agoutsmedt.github.io/networkflow/>.
```

## References

Boucher, Aurélien, and Xiaoguang Fan. 2025. “Scholars Like Any Other? An
Inquiry into Productivity and Citations in the Field of Contemporary
China Studies.” *Chinese Journal of Sociology* 11 (3): 349–73.
<https://doi.org/10.1177/2057150X251345124>.

Boyack, Kevin W., and Richard Klavans. 2010. “Co-Citation Analysis,
Bibliographic Coupling, and Direct Citation: Which Citation Approach
Represents the Research Front Most Accurately?” *Journal of the American
Society for Information Science and Technology* 61 (12): 2389–2404.
<https://doi.org/10.1002/asi.21419>.

Delcey, Thomas. forthcoming. “The Dissemination of Fama (1970): A
Bibliometric Analysis.” *Journal of the History of Economic Thought*,
forthcoming.

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

———. 2025. “Digital History of Macroeconomics.” Zenodo.
<https://doi.org/10.5281/zenodo.14627002>.

Goutsmedt, Aurélien, Alexandre Truc, and François Claveau. 2023.
“Biblionetwork: An R Package for Creating Different Types of
Bibliometric Networks.” Zenodo.
<https://doi.org/10.5281/zenodo.7677369>.

Guimerà, Roger, and Luís A. Nunes Amaral. 2005. “Functional Cartography
of Complex Metabolic Networks.” *Nature* 433 (7028): 895–900.
<https://doi.org/10.1038/nature03288>.

Kessler, M. M. 1963. “Bibliographic Coupling Between Scientific Papers.”
*American Documentation* 14 (1): 10–25.
<https://doi.org/10.1002/asi.5090140103>.

Neal, Zachary P. 2022. “Backbone: An R Package to Extract Network
Backbones.” *PLOS ONE* 17 (5): e0269137.
<https://doi.org/10.1371/journal.pone.0269137>.

Traag, V. A., L. Waltman, and N. J. van Eck. 2019. “From Louvain to
Leiden: Guaranteeing Well-Connected Communities.” *Scientific Reports* 9
(1): 5233. <https://doi.org/10.1038/s41598-019-41695-z>.
