# networkflow (development version)

## New features

- `name_clusters(method = "llm")` asks a language model for a label of each
  cluster. The model is a chat object of 'ellmer' (suggested, only needed
  for this method), configured by the user: a model running on their
  computer (`ellmer::chat_ollama()`, `chat_lmstudio()`, `chat_vllm()`...) or
  an online service. The model reads a description of each cluster written
  from the node columns given in the new `prompt_columns`: its `nb_nodes`
  main nodes, ranked by `order_by` (e.g. `within_module_degree` from
  `add_node_roles()`), and the columns with one value per cluster (a tf-idf
  label computed before), plus an optional `context`. `prompt_columns`,
  `order_by` and `nb_nodes` have no default. The system prompt is the new
  `instructions` (2 or 3 words by default), and the answer is structured.
  `preview = TRUE` returns the descriptions without calling a model.
- `name_clusters()` warns when `name_dynamic_clusters = TRUE` is used with
  clusters numbered within each window (those of `add_clusters()`), which
  would be named as one cluster across windows, and refuses arguments it
  does not use (a misspelt argument was ignored silently), except for the
  arguments of `extract_tfidf()` with `method = "tf-idf"`.

# networkflow 0.1.0 (first CRAN release)

Earlier development steps are numbered 0.0.9.900x, from the first step
(rewrite) to the latest; they are listed from the most recent to the oldest.

# networkflow 0.0.9.9001 (2026-11-01 to 2026-11-05)

Second step: the pipeline goes beyond bibliographic coupling, networks are
built with one explicit `method`, time windows and dynamic clusters are
reworked, the package is lighter and faster, and it was checked on samples
of the Scopus and Web of Science databases of the authors. The changes are
given with respect to 0.0.9.9000.

## Breaking changes

- `build_network()` and `build_dynamic_networks()`: one required argument,
  `method`, replaces `projection_method`, `cooccurrence_method` and `model`.
  It is either one of the five similarity measures of biblionetwork
  (`"angle"`, `"strength"`, `"similarity"`, `"jaccard"`, `"vladutz"`) or one
  of the null models of the statistical backbones of the backbone package
  (`"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedrow"`, `"fixedcol"`, with
  `alpha`). There is no default. The former arguments give an error that
  shows the translation (`projection_method = "structured",
  cooccurrence_method = "coupling_angle"` becomes `method = "angle"`).
- `build_dynamic_networks()` needs `time_variable` and `time_window`, and
  points to `build_network()` otherwise (it used to build the single network
  silently).
- `overlapping_window` is replaced by `window_step`, the number of units
  between the starts of two windows (`window_step = 1` replaces
  `overlapping_window = TRUE`). The arguments are reordered, and `...` only
  accepts the arguments of `filter_components()`. Windows of a single time
  unit are named after the unit (`"1990"` instead of `"1990-1990"`).
- `merge_dynamic_clusters()` and `merge_diagnostics()` have a required
  `method`: `"majority"` (the former double majority) or `"jaccard"` (new).
  `threshold_similarity` has no default any more: calls of earlier versions
  need `method = "majority"` and a threshold.
- `name_clusters()`: `name_merged_clusters` is renamed `name_dynamic_clusters`
  (the old name still works, with a deprecation warning).
- Argument names are harmonised: `cluster_id` replaces `module_col`
  (`add_node_roles()`) and `intertemporal_cluster_column`
  (`networks_to_alluv()`, `minimize_crossing_alluvial()`, `plot_alluvial()`),
  and `graphs` replaces `list_graph` (`merge_dynamic_clusters()`,
  `merge_diagnostics()`). `add_node_roles()` takes `weights`, as `add_clusters()`
  and tidygraph (`NULL` uses the `weight` edge column if any), and writes the
  roles in a column named `roles` (it was `role_ga`). `extract_tfidf()` keeps `data`, as it also takes
  plain tables.
- `plot_networks()` returns a named list of ggplot objects for a list of
  graphs.
- `networks_to_alluv()` returns `window` as a factor in the order of the list
  of graphs.
- biblionetwork >= 0.2.0 is required.

## New features

- Beyond bibliographic coupling: co-citation (`projection_on = "target"`),
  time carried by the links (`time_in = "edges"`), coupling of entities
  that publish documents, such as authors or journals (`document_id`,
  through `biblionetwork::entity_network()`), and a `compute_size` that
  counts the citing documents of each node.
- Dates (`Date`, `POSIXct`, ISO dates) and finer time units (`time_unit`:
  year, quarter, month, week, day).
- With `verbose = TRUE`, the building functions say which method they
  apply, what it measures and where it comes from.
- `merge_dynamic_clusters(method = "jaccard")`: overlap matching with a
  Jaccard threshold (Greene et al., 2010), each past identifier going to one
  cluster at most.
- `merge_diagnostics()` and `plot_merge_diagnostics()` show how the dynamic
  clusters depend on the rule and the threshold.
- `layout_networks()` has two new layouts, `"forceatlas2"` (Jacomy et al.,
  2014) and `"umap"` (McInnes et al., 2018).
- `minimize_crossing_alluvial()` reduces the crossings of the flows
  themselves (a local search, or the barycentre heuristic for large
  alluvials) and reports them.
- `plot_alluvial()` accepts the list of graphs directly, and keeps the main
  clusters with `min_share_cluster` and `min_windows_cluster`.
- `name_clusters()` with tf-idf skips the terms that overlap a better one
  ("business" after "business cycle").
- Windows without any edge give an empty network that the following steps
  accept; nodes or links with a missing time or identifier are dropped with a
  warning; a target linked to more than half of the sources (such as a code
  for unmatched references) is reported.

## Speed and memory

Measured on samples of 30,000 documents of Scopus and of the Web of
Science:

- `build_dynamic_networks()` is three times faster (24 s instead of 83 s for
  11 windows of the Web of Science sample).
- `merge_diagnostics()` is about seven times faster (8 s instead of 63 s).
- `minimize_crossing_alluvial()` takes about a second instead of 5 to 35
  minutes on alluvials of 250 to 550 clusters, with bounded memory.
- On the whole pipeline, the peak of memory falls from 13.9 GB to 3.9 GB on
  the Web of Science sample.
- `plot_networks()` draws straight edges on graphs of more than `max_arcs`
  edges (20,000): 2 seconds and 1 GB instead of 100 seconds and 11 GB for
  140,000 edges.
- UMAP uses the nearest neighbours instead of all the distances (1.4 GB
  instead of 5.7 GB on 9,600 nodes), and ForceAtlas2 computes its repulsion
  by blocks.
- 16 packages are imported instead of 22: `forcats`, `stringr` and
  `stopwords` are removed, and `ggalluvial`, `tidytext` and `lifecycle` are
  suggested. The functions that need a suggested package ask to install it.

## Bug fixes

- `plot_networks()` failed on PDF devices where the font of
  `ggraph::theme_graph()` (Arial Narrow) is not installed.
- `minimize_crossing_alluvial()` sorted the clusters on a string of
  component numbers, wrong as soon as there were ten components or more.
- Invalid `edges_threshold` (0, a string) failed deep inside biblionetwork; it
  is now checked first.

## Documentation and website

- Five guides on the website: "From references to networks", "Finding and
  reading clusters", "Drawing networks", "Following clusters over time" and
  "Beyond bibliographic coupling", with the methods, their formulas and the
  literature behind them.
- The documentation of each function explains its method and its choices,
  with formulas displayed on the website, the origin of each measure, and the
  cost of the slow methods (statistical backbones, ForceAtlas2, Kamada-Kawai).

# networkflow 0.0.9.9000 (2026-11-01)

First step: rewrite of the documentation and of the code of the former
GitHub version (0.0.9), with tests and benchmarks. To be released on CRAN as
0.1.0.

## Breaking changes

- `merge_dynamic_clusters()`: `similarity_type` must be given (`"complete"`
  or `"partial"`). Its former default could not be used anyway (it failed
  with "the condition has length > 1").
- `add_clusters()`: cluster identifiers are numbered by decreasing size for
  every algorithm ("01" is the largest cluster). Leiden identifiers were
  previously in the order returned by igraph: partitions are unchanged, but
  labels may differ from those obtained with an earlier version.
- `add_clusters()`: Louvain, fast greedy, walktrap and Infomap now use the
  `weight` edge column, as documented. Through tidygraph, they used to
  ignore it (only Leiden used the weights): partitions obtained with these
  algorithms on weighted networks change.
- `build_network()`: `model`, `alpha` and `backbone_args` are explicit
  arguments; `...` only goes to `filter_components()`.
- `build_dynamic_networks()` keeps `node_size` computed by `compute_size`
  instead of duplicating an existing `node_size` column.

## Removed

- The deprecated functions are removed: `community_colors()`,
  `community_labels()`, `community_names()`, `dynamic_network_cooccurrence()`,
  `intertemporal_cluster_naming()`, `leiden_workflow()`,
  `tbl_main_component()` and `top_nodes()`. Use `color_networks()`,
  `prepare_label_networks()`, `name_clusters()`, `build_dynamic_networks()`,
  `merge_dynamic_clusters()`, `add_clusters()` and `filter_components()`.

## Other changes

- `add_clusters()`: the default of `n_iterations` is now `-1` (Leiden
  iterates until the partition is stable) instead of 1000. Results are
  close but not identical to those obtained with 1000 iterations, and much
  faster on large networks.
- `launch_network_app()`: the UI, the server and the plot are now separate
  internal functions, tested with `shiny::testServer()`; the default layout
  is `"fr"` (Kamada-Kawai reads similarity weights as distances).
- `citation("networkflow")` works (`inst/CITATION`).
- Credits: Alexandre Truc initiated the alluvial method; Thomas Delcey and
  Aurélien Goutsmedt maintain the package.
- GitHub Actions: an R-CMD-check workflow (three systems) was added, and the
  pkgdown workflow installs Quarto.

## Bug fixes

- `add_node_roles()` computed the within-module degree and the
  participation coefficient wrongly for the node at the `to` end of each
  edge: all its ties were counted as internal to its cluster. Roles computed
  with earlier versions should be recomputed.
- `add_clusters(seed = )` no longer changes the random-number stream of the
  session: the seed is set locally with `withr::local_seed()` (CRAN policy).
- `add_clusters()` returns an error instead of `NULL` for an input that is
  neither a `tbl_graph` nor a list of them.
- `add_clusters()` and `merge_dynamic_clusters()` return graphs with the
  node table active (they left the edge table active).
- `merge_dynamic_clusters()` can be run again on graphs that already have
  dynamic clusters, works on unnamed lists and on graphs whose edges lack
  the `*_from`/`*_to` cluster columns, and warns when consecutive windows
  share no node (successive windows of documents cannot be matched).
- `build_network(projection_method = "statistical", filter_components = TRUE)`
  failed with "unused arguments".
- `build_dynamic_networks()` no longer fails on a window where no target is
  shared by two sources (it returns a graph without edges).
- `name_clusters(method = "given_column", name_merged_clusters = TRUE)`
  failed; the check that tidygraph was attached (through `sessionInfo()`)
  is removed.
- `prepare_label_networks()` ignored `x` and `y` for a single graph.
- `plot_networks(print_plot_code = TRUE)` used a wrong title for lists.
- `filter_components()` and `layout_networks()` returned their result
  invisibly.
- `layout_networks()` no longer triggers ggraph's warning about existing
  `x` and `y` columns.
- `extract_tfidf()` no longer triggers data.table's "shallow copy" warning,
  and missing texts no longer produce the term "na".
- `color_networks()`: with more than 14 categories, the default palette used
  13 colours; it now uses 14 and warns that colours are recycled.

# networkflow 0.0.9

Development version, distributed from GitHub (it was numbered 1.0.0).

Deprecated and new functions:

- `dynamic_network_cooccurrence()` is deprecated and is replaced by `build_dynamic_networks()`;
- `tbl_main_component()` is deprecated and is replaced by `build_network()` and `filter_components()`;
- `leiden_workflow()` is deprecated and is replaced by `add_clusters()`;
- `community_names()` is deprecated and is replaced by `name_clusters()`;
- `community_colors()` is deprecated and is replaced by `color_networks()`;
- `intertemporal_cluster_naming()` is deprecated and is replaced by `merge_dynamic_clusters()`;
- A new function computes coordinates with different layout algorithms:
  `layout_networks()`.

# networkflow 0.0.0.9000

Launch of a development version
