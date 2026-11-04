# networkflow: a research pipeline for projected and dynamic networks

`networkflow` organises the study of networks built from tabular
research data (articles and their references, authors and their papers,
accounts and what they share) as a sequence of explicit, documented
steps. Each step is a function whose choices are arguments that can be
changed and reported, and each returns a `tbl_graph` (or a list of them,
one per time window) that the next step enriches.

## Details

- **Building networks**:
  [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md),
  [`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)
  (bibliographic coupling, co-citation, coupling of authors and other
  projections of two-mode tables, with similarity measures from
  [`biblionetwork`](https://agoutsmedt.github.io/biblionetwork/) or
  statistical backbones from
  [`backbone`](https://github.com/zpneal/backbone), one network per time
  window),
  [`filter_components()`](https://agoutsmedt.github.io/networkflow/reference/filter_components.md).

- **Finding and reading clusters**:
  [`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md)
  (Leiden, Louvain, Infomap...),
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)
  (following clusters across windows),
  [`merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/merge_diagnostics.md)
  and
  [`plot_merge_diagnostics()`](https://agoutsmedt.github.io/networkflow/reference/plot_merge_diagnostics.md)
  (how the result depends on the threshold),
  [`name_clusters()`](https://agoutsmedt.github.io/networkflow/reference/name_clusters.md),
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md),
  [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md).

- **Drawing and exploring**:
  [`layout_networks()`](https://agoutsmedt.github.io/networkflow/reference/layout_networks.md),
  [`color_networks()`](https://agoutsmedt.github.io/networkflow/reference/color_networks.md),
  [`prepare_label_networks()`](https://agoutsmedt.github.io/networkflow/reference/prepare_label_networks.md),
  [`plot_networks()`](https://agoutsmedt.github.io/networkflow/reference/plot_networks.md),
  [`networks_to_alluv()`](https://agoutsmedt.github.io/networkflow/reference/networks_to_alluv.md),
  [`minimize_crossing_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/minimize_crossing_alluvial.md),
  [`plot_alluvial()`](https://agoutsmedt.github.io/networkflow/reference/plot_alluvial.md).

The guides of the package website
(<https://agoutsmedt.github.io/networkflow/>) explain the methods behind
each step on the example of the literature on the 1970s stagflation
([Nodes_stagflation](https://agoutsmedt.github.io/networkflow/reference/Nodes_stagflation.md),
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md)).

## See also

Useful links:

- <https://github.com/agoutsmedt/networkflow>

- <https://agoutsmedt.github.io/networkflow/>

- Report bugs at <https://github.com/agoutsmedt/networkflow/issues>

## Author

**Maintainer**: Aurélien Goutsmedt <agoutsmedt@hotmail.fr>
([ORCID](https://orcid.org/0000-0002-3788-7237))

Authors:

- Aurélien Goutsmedt <agoutsmedt@hotmail.fr>
  ([ORCID](https://orcid.org/0000-0002-3788-7237))

- Thomas Delcey ([ORCID](https://orcid.org/0000-0003-0546-1474))
  (co-maintainer)

- Alexandre Truc ([ORCID](https://orcid.org/0000-0002-1328-7819))
  (initiated the alluvial method)
