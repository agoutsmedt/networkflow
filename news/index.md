# Changelog

## networkflow 0.0.9.9004 (2026-11-05)

Fifth step: a second rule to follow clusters across windows, a clearer
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
- The README lists the study of Boucher and Fan (2025) among those that
  used the package.

## networkflow 0.0.9.9003 (2026-11-04)

Fourth step: one `method` to build networks, and the Shiny application
moves to its own package.

### Breaking changes

- [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  and
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md):
  a single argument, `method`, replaces `projection_method`,
  `cooccurrence_method` and `model`. There is no default, so that the
  choice stays explicit. The similarity measures of biblionetwork are
  `"angle"`, `"strength"`, `"similarity"`, `"jaccard"` and `"vladutz"`
  (formerly `"coupling_angle"`…); the statistical backbones of the
  backbone package are `"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedrow"`
  and `"fixedcol"` (with `alpha`). Using the former arguments gives an
  error that shows the translation, for example
  `projection_method = "structured", cooccurrence_method = "coupling_angle"`
  becomes `method = "angle"`.
- `launch_network_app()` is removed from networkflow: the application
  moves to the package networkflowapp. `shiny`, `DT`, `ggiraph` and
  `shinycssloaders` are no longer suggested.

### New features

- [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md)
  has two new layouts: `"forceatlas2"` (the layout of Gephi, Jacomy et
  al., 2014, in a compact implementation) and `"umap"` (UMAP of McInnes
  et al., 2018, computed with uwot on the shortest-path distances of the
  network). The package uwot is suggested.
- A new guide, “Drawing networks”, explains what a layout does, compares
  the layouts, and covers colours, labels and
  [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md);
  this part of “Finding and reading clusters” is moved there. In
  “Finding and reading clusters”, the table of cluster sizes is a
  labelled tibble, the CPM example uses the density of the network as
  the scale of the resolution,
  [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md)
  is shown with a centrality (`"tidygraph_functions"`), and the
  interpretation of clusters cites Delcey et al.
- With `verbose = TRUE` (now also the default of
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)),
  the functions say which method they apply, what it measures and where
  it comes from (Sen and Gan, Shen et al., Neal…). The details of
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  document the origin of each method.

## networkflow 0.0.9.9002 (2026-11-04)

Third step of the development: alignment with biblionetwork 0.2.0, a
lighter installation, clearer entry functions and documentation, and a
new README and website theme (changes of 2026-11-02 to 2026-11-04).

### Breaking changes

- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  now stops when `time_variable` or `time_window` is missing, instead of
  silently building the single network of
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md):
  that was not what a user calling a “dynamic” function meant. The error
  says to use
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  and suggests the columns that look like a time variable (dates, or
  numbers named like a year).
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  is unchanged.

### New features

- All the similarity measures of biblionetwork 0.2.0 are available in
  `cooccurrence_method`: `"coupling_jaccard"` and `"coupling_vladutz"`
  join `"coupling_angle"`, `"coupling_strength"` and
  `"coupling_similarity"`. The edges are computed with
  `biblionetwork::biblio_coupling(method = )`, and `document_id` (entity
  coupling) accepts the five measures through
  [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork/reference/entity_network.html)
  (it used to refuse `"coupling_similarity"`).
- [`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md)
  accepts the list of graphs with dynamic clusters directly (it calls
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md)),
  and `min_share_cluster` and `min_windows_cluster` keep the main
  clusters only: the whole alluvial workflow in one call. The separate
  functions remain available.

### Lighter installation

- `ggalluvial` and `tidytext` are moved from Imports to Suggests, with
  `lifecycle` (only used to write the documentation), and `forcats`,
  `stringr` and `stopwords`, which the package barely used, are removed:
  16 packages are imported instead of 22.
  [`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md),
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
  and `name_clusters(method = "tf-idf")` ask to install `ggalluvial` or
  `tidytext` when they are needed
  ([`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)).

### Documentation and website

- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  and
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  document, case by case, which arguments to choose (coupling, backbone,
  one network per period, co-citation, authors, other two-mode tables)
  and what each `projection_method` uses and ignores.
- The five measures are described, with their formulas, in the
  documentation and in the first guide, and linked to the explanations
  of biblionetwork. The first guide compares the coupling angle, the
  Jaccard index and the coupling similarity, and shows how much a
  statistical backbone prunes a network, with the numbers of nodes and
  edges of a coupling angle network.
- The formulas of the function documentation are displayed on the
  website (KaTeX), and the footer names the two maintainers.
- The README is written in Quarto (`README.qmd`); its illustration comes
  in a light and a dark version that follow the theme of the website. It
  mentions the Digital History of Macroeconomics project.
- Package names are written in code font, with a link at first mention,
  in the documentation and the guides.
- New site theme, logo and favicons.

### Other changes

- biblionetwork \>= 0.2.0 is required (`biblio_coupling()` is the
  general function; `coupling_entity()` is deprecated in favour of
  `entity_network()`), which also fixes the coupling similarity (the
  idf-weighted cosine of Shen et al., 2019) and the weights of the
  coupling strength.
- The `Authors@R` field of `DESCRIPTION` is fixed (a missing comma).

## networkflow 0.0.9.9001 (2026-11-01)

Second step: the pipeline is generalised beyond bibliographic coupling,
and time windows are reworked.

### Breaking changes

- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md):
  `overlapping_window` is replaced by `window_step`, the number of units
  between the starts of two windows (`window_step = 1` replaces
  `overlapping_window = TRUE`; by default, windows follow each other).
  Using `overlapping_window` gives an error that says so.
- [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md):
  the arguments are reordered (data, entities, projection, time,
  options) and `...` only accepts the arguments of
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md):
  a misspelt argument is now an error instead of being ignored.
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  follows the same order.
- Windows of a single time unit are named after the unit (`"1990"`,
  `"2001-Q2"`) instead of `"1990-1990"`.
- [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md)
  returns a named list of ggplot objects for a list of graphs (it used
  to store the plots in the graphs, in an element `plot`).
- The research scripts that call
  `build_dynamic_networks(overlapping_window = TRUE)` must use
  `window_step = 1`.

### New features

- Dates and finer time units: `time_variable` can be a `Date`, a
  `POSIXct` or an ISO date written as text, and `time_unit` (`"year"`,
  `"quarter"`, `"month"`, `"week"`, `"day"`) sets the unit in which
  `time_window` and `window_step` are counted. The time column of the
  user is not modified.
- [`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
  and
  [`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
  show how the dynamic clusters change with the threshold and the type
  of similarity of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  (number of clusters, lifespans, share of nodes in persistent
  clusters).
- [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
  documents how to read each role, with examples from the stagflation
  data, and the guide shows how roles change with the resolution.
  Running
  [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
  again replaces the earlier roles instead of duplicating the columns.
- The guides cite the literature on direct citation (Klavans and Boyack,
  Waltman and van Eck, Rivest et al., Sjögårde and Ahlgren, Kleminski et
  al.) and discuss other ways to follow clusters over time (Jaccard
  matching, multislice community detection, significance-clustered
  alluvial maps).
- Figures of the guides open in a lightbox.
- Co-citation:
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
  and
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  gain `projection_on = "target"`, which links the `target_id` entities
  that share `source_id` entities (e.g. cited works linked when cited
  together).
- `time_in = "edges"`: the time variable can be a column of the table of
  links (e.g. the year of the citing document), so that windows follow
  the citing documents (co-citation) or the documents of entities
  without a date (authors). Successive windows can then be matched,
  since nodes persist.
- `document_id`: coupling of entities that publish documents (authors,
  journals, institutions), counting the documents that cite each
  reference, through
  [`biblionetwork::entity_network()`](https://agoutsmedt.github.io/biblionetwork/reference/entity_network.html).
- `compute_size` counts the citing documents of each node when the nodes
  are linked through their own links (co-citation, `time_in = "edges"`).
- A fourth guide, “Beyond bibliographic coupling”, shows co-citation,
  author coupling and co-authorship, and explains why the identifiers of
  references matter.
- Windows without any edge give an empty network (with a warning) that
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md),
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  and the following steps accept; nodes or links with a missing time are
  dropped with a warning (they used to make the function fail).

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
