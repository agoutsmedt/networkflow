#' Diagnose the threshold of the matching of clusters across windows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' [merge_dynamic_clusters()] links the clusters of successive windows when
#' they share a majority of their nodes, and the result depends on the
#' threshold \eqn{\tau} and on the type of similarity. A low threshold links
#' clusters easily and produces a few long-lived dynamic clusters; a high
#' one breaks continuities and produces many short-lived clusters. There is no
#' right value, but there are *unstable* regions, where a small change of the
#' threshold changes the story. `merge_diagnostics()` runs the matching for a
#' grid of thresholds and similarity types and summarises, for each setting,
#' how the dynamic clusters change: how many there are, how long they live,
#' how many window clusters find a predecessor, and how the nodes are spread
#' between long-lived and short-lived clusters. [plot_merge_diagnostics()]
#' draws the result.
#'
#' @param list_graph A list of at least two `tbl_graph`s in chronological
#'   order, with the clusters of each window (see [add_clusters()]).
#' @param cluster_id Character scalar, the node column with the clusters of
#'   each window (e.g. `"cluster_leiden"`).
#' @param node_id Character scalar, the node column with an identifier that
#'   is stable across windows.
#' @param thresholds Numeric vector of thresholds in \eqn{(0.5, 1]} to try.
#' @param similarity_types Subset of `c("complete", "partial")`: the types to
#'   try (see [merge_dynamic_clusters()]).
#' @param min_length Whole number, the number of windows from which a dynamic
#'   cluster is called *persistent* (3 by default).
#' @param verbose Logical. If `TRUE` (default), reports progress.
#'
#' @details
#' For each setting, with a *window cluster* meaning a cluster of one window
#' and a *dynamic cluster* the set of window clusters that share an
#' identifier, the columns of the result are:
#'
#' - `n_window_clusters`: the number of window clusters (the same for all
#'   settings);
#' - `n_dynamic_clusters`: the number of dynamic clusters;
#' - `share_continued`: the share of the window clusters of the second window
#'   onwards that inherit the identifier of a cluster of the previous window
#'   (high: many continuities);
#' - `median_lifespan`, `max_lifespan`: the number of windows in which a
#'   dynamic cluster exists (median and maximum over dynamic clusters);
#' - `share_single_window`: the share of dynamic clusters that exist in one
#'   window only;
#' - `n_persistent`: the number of dynamic clusters existing in at least
#'   `min_length` windows;
#' - `share_nodes_persistent`: the share of the nodes of all windows (a node
#'   present in two windows counts twice) that belong to persistent dynamic
#'   clusters;
#' - `largest_share`: the share of those nodes in the largest dynamic cluster.
#'
#' The table of the lifespans of each dynamic cluster is stored in the
#' attribute `"lifespans"` and used by [plot_merge_diagnostics()].
#'
#' A good threshold is on a *plateau*: the main dynamic clusters, and the
#' share of nodes in persistent clusters, change little around it. Windows
#' without any shared node cannot be matched whatever the threshold, and are
#' reported in a warning.
#'
#' @return A data frame with one row per threshold and similarity type and
#'   the columns described above, with the table of lifespans in its
#'   `"lifespans"` attribute.
#'
#' @seealso [merge_dynamic_clusters()], [plot_merge_diagnostics()].
#'
#' @examples
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   window_step = 4,
#'   cooccurrence_method = "coupling_angle",
#'   edges_threshold = 2,
#'   verbose = FALSE
#' )
#' networks <- add_clusters(
#'   networks,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' diagnostics <- merge_diagnostics(
#'   networks,
#'   cluster_id = "cluster_leiden",
#'   node_id = "source_id",
#'   thresholds = c(0.51, 0.6, 0.75),
#'   similarity_types = "partial",
#'   verbose = FALSE
#' )
#' diagnostics[, c("similarity_type", "threshold", "n_dynamic_clusters", "median_lifespan")]
#'
#' @export
merge_diagnostics <- function(list_graph,
                              cluster_id,
                              node_id,
                              thresholds = c(0.51, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.9),
                              similarity_types = c("complete", "partial"),
                              min_length = 3,
                              verbose = TRUE) {
  window <- cluster <- node <- dynamic <- lifespan <- n_rows <- NULL

  if (is_single_graph(list_graph, arg = "list_graph") || length(list_graph) < 2) {
    cli::cli_abort("{.arg list_graph} must be a list of at least two {.cls tbl_graph}s.")
  }
  for (graph in list_graph) check_columns(node_table(graph), c(cluster_id, node_id), "list_graph")
  if (!is.numeric(thresholds) || length(thresholds) == 0 || any(thresholds <= 0.5 | thresholds > 1)) {
    cli::cli_abort("{.arg thresholds} must be numbers in (0.5, 1].")
  }
  similarity_types <- rlang::arg_match(similarity_types, c("complete", "partial"), multiple = TRUE)
  min_length <- check_positive_integer(min_length, "min_length")

  # Windows that share no node cannot be matched, whatever the threshold.
  ids <- lapply(list_graph, function(graph) node_table(graph)[[node_id]])
  shared <- vapply(seq_along(ids)[-1], function(i) length(intersect(ids[[i - 1]], ids[[i]])), integer(1))
  if (any(shared == 0)) {
    cli::cli_warn("{sum(shared == 0)} pair{?s} of consecutive windows share no node: {cli::qty(sum(shared == 0))}{?its/their} clusters cannot be matched, whatever the threshold.")
  }

  window_clusters <- data.table::rbindlist(lapply(unname(list_graph), function(graph) {
    nodes <- node_table(graph)
    data.table::data.table(cluster = as.character(nodes[[cluster_id]]))
  }), idcol = "window")
  n_window_clusters <- nrow(unique(window_clusters))
  n_first <- length(unique(window_clusters[window == 1, cluster]))

  settings <- expand.grid(similarity_type = similarity_types, threshold = thresholds, stringsAsFactors = FALSE)
  if (verbose) cli::cli_progress_bar("Matching clusters", total = nrow(settings))
  new_col <- paste0("dynamic_", cluster_id)
  results <- vector("list", nrow(settings))
  lifespans <- vector("list", nrow(settings))
  for (i in seq_len(nrow(settings))) {
    merged <- suppressMessages(suppressWarnings(merge_dynamic_clusters(
      list_graph, cluster_id, node_id,
      threshold_similarity = settings$threshold[i],
      similarity_type = settings$similarity_type[i]
    )))
    rows <- data.table::rbindlist(lapply(unname(merged), function(graph) {
      nodes <- node_table(graph)
      data.table::data.table(dynamic = as.character(nodes[[new_col]]))
    }), idcol = "window")
    per_cluster <- rows[, list(lifespan = data.table::uniqueN(window), n_rows = .N), by = dynamic]
    persistent <- per_cluster$lifespan >= min_length
    results[[i]] <- data.frame(
      similarity_type = settings$similarity_type[i],
      threshold = settings$threshold[i],
      n_window_clusters = n_window_clusters,
      n_dynamic_clusters = nrow(per_cluster),
      share_continued = (n_window_clusters - nrow(per_cluster)) / max(n_window_clusters - n_first, 1),
      median_lifespan = stats::median(per_cluster$lifespan),
      max_lifespan = max(per_cluster$lifespan),
      share_single_window = mean(per_cluster$lifespan == 1),
      n_persistent = sum(persistent),
      share_nodes_persistent = sum(per_cluster$n_rows[persistent]) / sum(per_cluster$n_rows),
      largest_share = max(per_cluster$n_rows) / sum(per_cluster$n_rows)
    )
    lifespans[[i]] <- data.frame(
      similarity_type = settings$similarity_type[i],
      threshold = settings$threshold[i],
      dynamic_cluster = per_cluster$dynamic,
      lifespan = per_cluster$lifespan,
      n_nodes = per_cluster$n_rows
    )
    if (verbose) cli::cli_progress_update()
  }
  out <- do.call(rbind, results)
  attr(out, "lifespans") <- do.call(rbind, lifespans)
  attr(out, "min_length") <- min_length
  out
}

#' Plot the diagnostics of the matching of clusters across windows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Draws the result of [merge_diagnostics()]. Two views are available:
#'
#' - `"summary"` (default): one panel per indicator (number of dynamic
#'   clusters, share of window clusters that find a predecessor, median
#'   lifespan, share of nodes in persistent clusters...), against the
#'   threshold, with one line per type of similarity. Look for plateaus:
#'   thresholds around which the indicators barely move.
#' - `"lifespans"`: for each threshold, the share of the nodes (of all
#'   windows) that belong to dynamic clusters of a given lifespan (1 window,
#'   2, 3 to 5, 6 to 10, more than 10), one panel per type of similarity. It
#'   shows how the weight of short-lived and long-lived clusters changes with
#'   the threshold.
#'
#' @param diagnostics The result of [merge_diagnostics()].
#' @param view `"summary"` or `"lifespans"`.
#' @param metrics For the summary, the columns of `diagnostics` to draw.
#' @param base_size Base font size of the plot.
#'
#' @return A ggplot object.
#'
#' @seealso [merge_diagnostics()].
#'
#' @examples
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   window_step = 4,
#'   cooccurrence_method = "coupling_angle",
#'   edges_threshold = 2,
#'   verbose = FALSE
#' )
#' networks <- add_clusters(
#'   networks,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#' diagnostics <- merge_diagnostics(
#'   networks, "cluster_leiden", "source_id",
#'   thresholds = c(0.51, 0.6, 0.75), similarity_types = "partial", verbose = FALSE
#' )
#' plot_merge_diagnostics(diagnostics)
#' plot_merge_diagnostics(diagnostics, view = "lifespans")
#'
#' @export
plot_merge_diagnostics <- function(diagnostics,
                                   view = c("summary", "lifespans"),
                                   metrics = c("n_dynamic_clusters", "share_continued",
                                               "median_lifespan", "share_nodes_persistent"),
                                   base_size = 16) {
  threshold <- value <- similarity_type <- lifespan_class <- share <- NULL
  view <- rlang::arg_match(view)
  if (!inherits(diagnostics, "data.frame") || is.null(attr(diagnostics, "lifespans"))) {
    cli::cli_abort("{.arg diagnostics} must be the result of {.fn merge_diagnostics}.")
  }
  min_length <- attr(diagnostics, "min_length")
  type_colors <- c(complete = "#1B9E77", partial = "#7570B3")

  if (view == "summary") {
    labels <- c(
      n_window_clusters = "Window clusters",
      n_dynamic_clusters = "Dynamic clusters",
      share_continued = "Window clusters with a predecessor",
      median_lifespan = "Median lifespan (windows)",
      max_lifespan = "Maximum lifespan (windows)",
      share_single_window = "Dynamic clusters in one window",
      n_persistent = sprintf("Dynamic clusters of %d windows or more", min_length),
      share_nodes_persistent = sprintf("Nodes in clusters of %d windows or more", min_length),
      largest_share = "Nodes in the largest cluster"
    )
    check_columns(diagnostics, metrics, "diagnostics")
    long <- do.call(rbind, lapply(metrics, function(metric) {
      data.frame(
        threshold = diagnostics$threshold,
        similarity_type = diagnostics$similarity_type,
        metric = factor(labels[[metric]], levels = unname(labels[metrics])),
        value = diagnostics[[metric]]
      )
    }))
    return(
      ggplot2::ggplot(long, ggplot2::aes(threshold, value, colour = similarity_type)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::facet_wrap(ggplot2::vars(metric), scales = "free_y") +
        ggplot2::scale_colour_manual(values = type_colors, name = "Similarity") +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.08))) +
        ggplot2::labs(x = "Threshold", y = NULL) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  lifespans <- attr(diagnostics, "lifespans")
  classes <- c("1 window", "2 windows", "3 to 5", "6 to 10", "more than 10")
  lifespans$lifespan_class <- cut(lifespans$lifespan, c(0, 1, 2, 5, 10, Inf), labels = classes)
  totals <- stats::aggregate(n_nodes ~ similarity_type + threshold + lifespan_class, lifespans, sum)
  totals$share <- totals$n_nodes / stats::ave(totals$n_nodes, totals$similarity_type, totals$threshold, FUN = sum)
  ggplot2::ggplot(totals, ggplot2::aes(factor(threshold), share, fill = lifespan_class)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(similarity_type)) +
    ggplot2::scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.1, end = 0.9, name = "Lifespan of the cluster") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Threshold", y = "Share of the nodes of all windows") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")
}
