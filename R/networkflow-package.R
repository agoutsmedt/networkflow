#' networkflow: a research pipeline for projected and dynamic networks
#'
#' `networkflow` organises the study of networks built from tabular research
#' data (articles and their references, authors and their papers, accounts
#' and what they share) as a sequence of explicit, documented steps. Each
#' step is a function whose choices are arguments that can be changed and
#' reported, and each returns a `tbl_graph` (or a list of them, one per time
#' window) that the next step enriches.
#'
#' * **Building networks**: [build_network()], [build_dynamic_networks()]
#'   (bibliographic coupling, co-citation, coupling of authors and other
#'   projections of two-mode tables, with similarity measures from
#'   [`biblionetwork`](https://agoutsmedt.github.io/biblionetwork/) or statistical backbones from [`backbone`](https://github.com/zpneal/backbone), one network per
#'   time window), [filter_components()].
#' * **Finding and reading clusters**: [add_clusters()] (Leiden, Louvain,
#'   Infomap...), [merge_dynamic_clusters()] (following clusters across
#'   windows), [merge_diagnostics()] and [plot_merge_diagnostics()] (how the
#'   result depends on the threshold), [name_clusters()], [extract_tfidf()], [add_node_roles()].
#' * **Drawing and exploring**: [layout_networks()], [color_networks()],
#'   [prepare_label_networks()], [plot_networks()], [networks_to_alluv()],
#'   [minimize_crossing_alluvial()], [plot_alluvial()].

#'
#' The guides of the package website (<https://agoutsmedt.github.io/networkflow/>)
#' explain the methods behind each step on the example of the literature on
#' the 1970s stagflation ([Nodes_stagflation], [Ref_stagflation]).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import data.table
#' @importFrom Rdpack reprompt
#' @importFrom rlang !! .data
#' @importFrom utils globalVariables
#' @importFrom tidygraph %E>%
#' @importFrom tidygraph %N>%
#' @importFrom tidygraph .N
## usethis namespace: end
NULL


