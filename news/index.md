# Changelog

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
