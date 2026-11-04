#' Plot the evolution of dynamic clusters as an alluvial
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `plot_alluvial()` draws the table created by [networks_to_alluv()] with
#' ggalluvial: one column per time window, one block per dynamic cluster,
#' with a height proportional to its share of the window, and flows that
#' follow the nodes from one window to the next. It can colour the clusters
#' ([color_alluvial()]), reorder them to limit crossings
#' ([minimize_crossing_alluvial()]) and position their labels
#' ([prepare_label_alluvial()]) on the way.
#'
#' @param alluv_dt A data frame created with [networks_to_alluv()], or
#'   directly the list of `tbl_graph`s with dynamic clusters (see
#'   [merge_dynamic_clusters()]), which is then converted with
#'   [networks_to_alluv()] (`color_column` and `cluster_label_column` are
#'   the columns kept).
#' @param cluster_id Character scalar, the column with the
#'   dynamic clusters (e.g. `"dynamic_cluster_leiden"`).
#' @param node_id Character scalar, the column with the node identifiers.
#' @param window_column Character scalar, the column with the time windows
#'   (`"window"` by default).
#' @param color_column Character scalar, the column with the colours of the
#'   clusters (`"color"` by default). If absent or `NULL`, [`ggplot2`](https://ggplot2.tidyverse.org/) chooses
#'   the colours.
#' @param color_alluvial Logical. If `TRUE`, runs [color_alluvial()] first.
#' @param color With `color_alluvial = TRUE`, the colours passed to
#'   [color_alluvial()].
#' @param minimize_crossing Logical. If `TRUE`, runs
#'   [minimize_crossing_alluvial()] first. Clusters are also reordered when
#'   `alluv_dt` already has a `minimize_crossing_order` column.
#' @param prepare_label Logical. If `TRUE`, runs [prepare_label_alluvial()]
#'   first, which adds a `label_x` column with one label per cluster.
#' @param cluster_label_column Character scalar, the column with the labels
#'   used by [prepare_label_alluvial()] (`"cluster_label"` by default).
#' @param min_share_cluster,min_windows_cluster Optional numbers to keep the
#'   main clusters only: the clusters whose largest share of a window
#'   (`share_cluster_max`, in per cent) is at least `min_share_cluster`,
#'   and which exist in at least `min_windows_cluster` windows
#'   (`length_cluster`). `NULL` (default) keeps all the clusters.
#' @param print_plot_code Logical. If `TRUE`, prints the `ggplot2` code of the
#'   plot.
#'
#' @details
#' Labels are drawn with [`ggrepel`](https://ggrepel.slowkow.com/), which must be installed, when `alluv_dt`
#' has a `label_x` column. Small clusters clutter alluvials: filtering the
#' table on `share_cluster_max` or `length_cluster` (see
#' [networks_to_alluv()]) before plotting keeps the main ones.
#'
#' The alluvial method was initiated by Alexandre Truc.
#'
#' @return A ggplot object.
#'
#' @examples
#' \donttest{
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   method = "similarity",
#'   edges_threshold = 1,
#'   verbose = FALSE
#' )
#' networks <- add_clusters(
#'   networks,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#' networks <- merge_dynamic_clusters(
#'   networks,
#'   cluster_id = "cluster_leiden",
#'   node_id = "source_id",
#'   method = "majority", threshold_similarity = 0.51,
#'   similarity_type = "partial"
#' )
#' alluv_dt <- networks_to_alluv(
#'   networks,
#'   cluster_id = "dynamic_cluster_leiden",
#'   node_id = "source_id"
#' )
#' plot_alluvial(
#'   alluv_dt,
#'   cluster_id = "dynamic_cluster_leiden",
#'   node_id = "source_id",
#'   color_alluvial = TRUE,
#'   minimize_crossing = TRUE
#' )
#' }
#'
#' @export
plot_alluvial <- function(alluv_dt,
                          cluster_id,
                          node_id,
                          window_column = "window",
                          color_column = "color",
                          color_alluvial = FALSE,
                          color = NULL,
                          minimize_crossing = FALSE,
                          prepare_label = FALSE,
                          cluster_label_column = "cluster_label",
                          min_share_cluster = NULL,
                          min_windows_cluster = NULL,
                          print_plot_code = FALSE)
{
  . <- N <- y_alluv <- label_x <- share_cluster_max <- length_cluster <- NULL
  rlang::check_installed("ggalluvial", reason = "to draw alluvial plots.")
  # A list of graphs is converted first: the whole workflow in one call
  if (!is.data.frame(alluv_dt)) {
    is_single_graph(alluv_dt, arg = "alluv_dt") # error if neither a table nor a list of graphs
    alluv_dt <- networks_to_alluv(
      alluv_dt,
      cluster_id = cluster_id,
      node_id = node_id,
      color_column = color_column,
      cluster_label_column = cluster_label_column
    )
  }
  if (!is.null(min_share_cluster) || !is.null(min_windows_cluster)) {
    check_columns(alluv_dt, c("share_cluster_max", "length_cluster"), "alluv_dt")
    min_share <- if (is.null(min_share_cluster)) 0 else min_share_cluster
    min_windows <- if (is.null(min_windows_cluster)) 0 else min_windows_cluster
    alluv_dt <- data.table::as.data.table(alluv_dt)[
      share_cluster_max >= min_share & length_cluster >= min_windows
    ]
  }
  check_columns(alluv_dt, c(cluster_id, node_id, window_column), "alluv_dt")

  if(color_alluvial){
    alluv_dt <- color_alluvial(alluv_dt,
                               column_to_color = cluster_id,
                               color = color,
                               verbose = FALSE)
    color_column <- "color"
  }
  if(minimize_crossing){
    alluv_dt <- minimize_crossing_alluvial(alluv_dt = alluv_dt,
                                           cluster_id = cluster_id,
                                           node_id = node_id,
                                           verbose = FALSE)
  }

  if(prepare_label){
    alluv_dt <- prepare_label_alluvial(alluv_dt,
                                       cluster_label_column = cluster_label_column)
  }

  window_column <- rlang::ensym(window_column)
  stratum_column <- rlang::ensym(cluster_id)
  alluvium_column <- rlang::ensym(node_id)

  if("label_x" %in% colnames(alluv_dt)) {
    label_column <- rlang::ensym(cluster_label_column)
  }

  if(is.null(color_column)){
    color_variable <- stratum_column
  } else{
    if(color_column %in% colnames(alluv_dt)){
      color_variable <- rlang::ensym(color_column)
    } else {
      cli::cli_inform(c("i" = "No {.field {color_column}} column: ggplot2 chooses the colours (or use {.code color_alluvial = TRUE})."))
      color_variable <- stratum_column
      color_column <- NULL # That was a false color column that we can remove
    }
  }

  if("minimize_crossing_order" %in% colnames(alluv_dt)){
    # levels ordered by decreasing minimal order (the former forcats::fct_reorder())
    alluv_dt[[cluster_id]] <- stats::reorder(
      factor(alluv_dt[[cluster_id]]),
      alluv_dt[["minimize_crossing_order"]],
      FUN = function(x) -min(x)
    )

  }

  plot_alluvial <- rlang::expr(
    ggplot2::ggplot(alluv_dt, ggplot2::aes(x = !!window_column,
                                           y = y_alluv,
                                           stratum = !!stratum_column,
                                           alluvium = !!alluvium_column,
                                           fill = !!color_variable)) +
      ggalluvial::geom_stratum(alpha = 1, show.legend = FALSE) +
      ggalluvial::geom_flow(show.legend = FALSE) +
      {if(! is.null(color_column)) ggplot2::scale_fill_identity()} +
      {if("label_x" %in% colnames(alluv_dt)) ggrepel::geom_label_repel(ggplot2::aes(label = label_x),
                                                                       stat = ggalluvial::StatStratum,
                                                                       show.legend = FALSE, na.rm = TRUE)} +
      ggplot2::theme_minimal())


  if (print_plot_code) {
    cli::cli_h2("ggplot2 code of the plot")
    cli::cli_code(deparse(plot_alluvial, width.cutoff = 80))
  }

  eval(plot_alluvial)
}
