# Changelog

## networkflow 0.0.9.9002 (2026-11-05)

Third step: a second rule to follow clusters across windows, a clearer
name for the naming of dynamic clusters, and a better order for alluvial
plots.

### Breaking changes

- [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  has a required `method`: `"majority"` (the double majority, the only
  rule so far) or `"jaccard"` (new, see below). `threshold_similarity`
  has no default any more (it used to be 0.5001): the rule, its
  threshold and the type of similarity change the dynamic clusters, so
  all three must be given. Calls of earlier versions need
  `method = "majority"` and a threshold.
- [`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
  has a required `method` too, and `thresholds` is `NULL` by default (a
  grid for each rule); its result has a `method` column.
- [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)
  returns `window` as a factor whose levels follow the order of the
  list: windows with numeric names (“2”, “10”) are no longer sorted
  alphabetically in the table and in the plot.

### New features

- `merge_dynamic_clusters(method = "jaccard")`: overlap matching with a
  Jaccard threshold, as in the tracking of dynamic communities of Greene
  et al. (2010). Candidate pairs have a Jaccard index of at least the
  threshold (which can be well below 0.5); they are taken by decreasing
  index, each past identifier going to one cluster at most.
  `similarity_type = "partial"` restricts the union to the nodes present
  in both windows. With `verbose = TRUE`, the function says which rule
  it applies.
  [`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
  and
  [`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
  work with both rules, and the guide “Following clusters over time”
  compares them.
- [`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
  refines its order against the crossings of the flows themselves
  (`refine = TRUE`, `max_passes`): on the stagflation data, the weighted
  crossings fall from about 380 to 90. It reports the crossings before
  and after (`verbose`).
- [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md):
  `name_merged_clusters` is renamed `name_dynamic_clusters` (the old
  name still works, with a deprecation warning).

### Bug fixes

- [`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md)
  pasted the numbers of the components into a string to sort the
  clusters, which mixed “1” “10” and “11” “0” as soon as there were ten
  components or more.

### Documentation

- The guide “Following clusters over time” discusses the risk of
  building communities of documents of the same years rather than
  thematic communities on a long period (coupling, and possibly
  co-citation), says that windows must overlap (`window_step` smaller
  than `time_window`, not only `window_step = 1`) for documents to be
  matched, and presents the multislice community detection and the
  significance-clustered alluvial maps as possible future methods.
- The guide “Following clusters over time” draws three windows with
  `layout_networks(compute_dynamic_coordinates = TRUE)` and the colours
  of the alluvial.
- The guide “Drawing networks” draws on Venturini, Jacomy and Jensen on
  visual network analysis (reading densities rather than distances, no
  axes, the settings of ForceAtlas2) and checks the layouts against the
  clusters with k-means, as they suggest.
- The README lists the study of Boucher and Fan (2025) among those that
  used the package.

## networkflow 0.0.9.9001 (2026-11-01 to 2026-11-04)

Second step: the pipeline goes beyond bibliographic coupling, networks
are built with one explicit `method`, time windows are reworked, and the
package is lighter. The changes are given with respect to 0.0.9.9000.

### Breaking changes

- [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  and
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md):
  one required argument, `method`, replaces `projection_method`,
  `cooccurrence_method` and `model`. It is either one of the five
  similarity measures of biblionetwork (`"angle"`, `"strength"`,
  `"similarity"`, `"jaccard"`, `"vladutz"`) or one of the null models of
  the statistical backbones of the backbone package (`"sdsm"`, `"fdsm"`,
  `"fixedfill"`, `"fixedrow"`, `"fixedcol"`, with `alpha`). There is no
  default. The former arguments give an error that shows the translation
  (`projection_method = "structured", cooccurrence_method = "coupling_angle"`
  becomes `method = "angle"`).
- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  needs `time_variable` and `time_window`, and points to
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  otherwise (it used to build the single network silently).
- `overlapping_window` is replaced by `window_step`, the number of units
  between the starts of two windows (`window_step = 1` replaces
  `overlapping_window = TRUE`). The arguments are reordered, and `...`
  only accepts the arguments of
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).
  Windows of a single time unit are named after the unit (`"1990"`
  instead of `"1990-1990"`).
- [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
  returns a named list of ggplot objects for a list of graphs.
- `launch_network_app()` is removed: the Shiny application moves to the
  package networkflowapp.
- biblionetwork \>= 0.2.0 is required.

### New features

- Beyond bibliographic coupling: co-citation
  (`projection_on = "target"`), time carried by the links
  (`time_in = "edges"`), coupling of entities that publish documents,
  such as authors or journals (`document_id`, through
  [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork/reference/entity_network.html)),
  and a `compute_size` that counts the citing documents of each node.
- Dates (`Date`, `POSIXct`, ISO dates) and finer time units
  (`time_unit`: year, quarter, month, week, day).
- With `verbose = TRUE`, the building functions say which method they
  apply, what it measures and where it comes from.
- [`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
  and
  [`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
  show how the dynamic clusters depend on the threshold of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md).
- [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
  has two new layouts, `"forceatlas2"` (Jacomy et al.,
  2014. and `"umap"` (McInnes et al., 2018).
- [`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
  accepts the list of graphs directly, and keeps the main clusters with
  `min_share_cluster` and `min_windows_cluster`.
- Windows without any edge give an empty network that the following
  steps accept; nodes or links with a missing time are dropped with a
  warning.

### Lighter installation

- 16 packages are imported instead of 22: `forcats`, `stringr` and
  `stopwords` are removed, and `ggalluvial`, `tidytext` and `lifecycle`
  are suggested. The functions that need a suggested package
  ([`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md),
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md),
  the statistical backbones, UMAP) ask to install it.

### Documentation and website

- Five guides on the website: “From references to networks”, “Finding
  and reading clusters”, “Drawing networks”, “Following clusters over
  time” and “Beyond bibliographic coupling”, with the methods, their
  formulas and the literature behind them.
- The documentation of each function explains its method and its
  choices, with formulas displayed on the website and the origin of each
  measure.
- The README is written in Quarto, with an illustration in a light and a
  dark version, and the studies that used the package.
- New theme, logo and favicons.

## networkflow 0.0.9.9000 (2026-11-01)

First step: rewrite of the documentation and of the code of the former
GitHub version (0.0.9), with tests and benchmarks. To be released on
CRAN as 0.1.0.

### Breaking changes

- [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md):
  `similarity_type` must be given (`"complete"` or `"partial"`). Its
  former default could not be used anyway (it failed with “the condition
  has length \> 1”).
- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md):
  cluster identifiers are numbered by decreasing size for every
  algorithm (“01” is the largest cluster). Leiden identifiers were
  previously in the order returned by igraph: partitions are unchanged,
  but labels may differ from those obtained with an earlier version.
- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md):
  Louvain, fast greedy, walktrap and Infomap now use the `weight` edge
  column, as documented. Through tidygraph, they used to ignore it (only
  Leiden used the weights): partitions obtained with these algorithms on
  weighted networks change.
- [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md):
  `model`, `alpha` and `backbone_args` are explicit arguments; `...`
  only goes to
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).
- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  keeps `node_size` computed by `compute_size` instead of duplicating an
  existing `node_size` column.

### Removed

- The deprecated functions are removed: `community_colors()`,
  `community_labels()`, `community_names()`,
  `dynamic_network_cooccurrence()`, `intertemporal_cluster_naming()`,
  `leiden_workflow()`, `tbl_main_component()` and `top_nodes()`. Use
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md),
  [`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md),
  [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md),
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md),
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

### Other changes

- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md):
  the default of `n_iterations` is now `-1` (Leiden iterates until the
  partition is stable) instead of 1000. Results are close but not
  identical to those obtained with 1000 iterations, and much faster on
  large networks.
- `launch_network_app()`: the UI, the server and the plot are now
  separate internal functions, tested with
  [`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html);
  the default layout is `"fr"` (Kamada-Kawai reads similarity weights as
  distances).
- `citation("networkflow")` works (`inst/CITATION`).
- Credits: Alexandre Truc initiated the alluvial method; Thomas Delcey
  and Aurélien Goutsmedt maintain the package.
- GitHub Actions: an R-CMD-check workflow (three systems) was added, and
  the pkgdown workflow installs Quarto.

### Bug fixes

- [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
  computed the within-module degree and the participation coefficient
  wrongly for the node at the `to` end of each edge: all its ties were
  counted as internal to its cluster. Roles computed with earlier
  versions should be recomputed.
- `add_clusters(seed = )` no longer changes the random-number stream of
  the session: the seed is set locally with
  [`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  (CRAN policy).
- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  returns an error instead of `NULL` for an input that is neither a
  `tbl_graph` nor a list of them.
- [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  return graphs with the node table active (they left the edge table
  active).
- [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  can be run again on graphs that already have dynamic clusters, works
  on unnamed lists and on graphs whose edges lack the `*_from`/`*_to`
  cluster columns, and warns when consecutive windows share no node
  (successive windows of documents cannot be matched).
- `build_network(projection_method = "statistical", filter_components = TRUE)`
  failed with “unused arguments”.
- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  no longer fails on a window where no target is shared by two sources
  (it returns a graph without edges).
- `name_clusters(method = "given_column", name_merged_clusters = TRUE)`
  failed; the check that tidygraph was attached (through
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)) is
  removed.
- [`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md)
  ignored `x` and `y` for a single graph.
- `plot_networks(print_plot_code = TRUE)` used a wrong title for lists.
- [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md)
  and
  [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
  returned their result invisibly.
- [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
  no longer triggers ggraph’s warning about existing `x` and `y`
  columns.
- [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
  no longer triggers data.table’s “shallow copy” warning, and missing
  texts no longer produce the term “na”.
- [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md):
  with more than 14 categories, the default palette used 13 colours; it
  now uses 14 and warns that colours are recycled.

## networkflow 0.0.9

Development version, distributed from GitHub (it was numbered 1.0.0).

Deprecated and new functions:

- `dynamic_network_cooccurrence()` is deprecated and is replaced by
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md);
- `tbl_main_component()` is deprecated and is replaced by
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  and
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md);
- `leiden_workflow()` is deprecated and is replaced by
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md);
- `community_names()` is deprecated and is replaced by
  [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md);
- `community_colors()` is deprecated and is replaced by
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md);
- `intertemporal_cluster_naming()` is deprecated and is replaced by
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md);
- A new function computes coordinates with different layout algorithms:
  [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md).

## networkflow 0.0.0.9000

Launch of a development version
