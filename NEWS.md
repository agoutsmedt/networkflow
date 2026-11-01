# networkflow (development version, to be released on CRAN as 0.1.0)

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

## Performance

- `color_networks()` mixes edge colours in one vectorised call instead of
  one call per edge.
- `extract_tfidf()` lemmatises or stems each distinct word once instead of
  each occurrence.
- `add_clusters()` computes partitions directly with igraph.
- `bench/benchmark_functions.R` compares the time, memory and results of the
  pipeline with the former version on samples of a Scopus corpus.

## Messages and checks

- All messages go through cli: `cli_abort()`, `cli_warn()` and
  `cli_inform()` (which can be silenced), with progress bars over time
  windows; errors from biblionetwork and backbone are chained.
- Arguments are checked with informative errors (missing columns, choices
  that must be explicit, wrong input types).
- `color_networks()` and `color_alluvial()` gain a `verbose` argument.

## Package

- Tests (testthat 3) for every exported function, on hand-computed examples
  and the packaged data.
- DESCRIPTION: new title, R (>= 4.1.0), no `Remotes`, `Author` or `Date`
  fields; ggrepel, stopwords and withr move to `Imports`.

## Documentation

- The documentation was rewritten as the documentation of a research
  pipeline: each step explains the method behind it (projection and
  similarity measures, statistical backbones, modularity and CPM, the
  matching rule of dynamic clusters, Guimerà-Amaral roles, tf-idf), with
  formulas and references to the literature (`inst/REFERENCES.bib`).
- The vignette `networkflow_presentation` is replaced by three Quarto guides
  on the website: "From references to networks", "Finding and reading
  clusters" and "Following clusters over time".
- The README presents the pipeline and the research that used it.
- `build_dynamic_networks()` warns that, in biblionetwork 0.1.x,
  `"coupling_similarity"` is proportional to `"coupling_strength"`.
- `filter_components()` is no longer described as deprecated: it is the
  replacement of `tbl_main_component()`.
- `layout_networks()` documents that Kamada-Kawai reads edge weights as
  distances; examples now use Fruchterman-Reingold.

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
