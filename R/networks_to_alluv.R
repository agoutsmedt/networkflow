#' Turn a list of networks into the table of an alluvial plot
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An alluvial plot shows the evolution of dynamic clusters: each time
#' window is a column, each cluster a block whose height is its share of the
#' nodes of the window, and the flows between blocks follow the nodes that
#' stay in the network from one window to the next
#' \insertCite{rosvall2010}{networkflow}. `networks_to_alluv()` stacks the
#' node tables of a list of networks into the long table that such a plot
#' needs (one row per node and window) and adds summary statistics on the
#' clusters.
#'
#' @param graphs A list of `tbl_graph`s with dynamic clusters (see
#'   [merge_dynamic_clusters()]).
#' @param intertemporal_cluster_column Character scalar, the node column with
#'   the dynamic clusters (e.g. `"dynamic_cluster_leiden"`).
#' @param node_id Character scalar, the node column with a stable identifier;
#'   it is the "alluvium" that the flows follow.
#' @param summary_cluster_stats Logical. If `TRUE` (default), adds the
#'   statistics described in the return value, useful to keep only the main
#'   clusters in the plot.
#' @param keep_color Logical. If `TRUE` (default), keeps `color_column`.
#' @param color_column Character scalar, the colour column (`"color"`, from
#'   [color_networks()]).
#' @param keep_cluster_label Logical. If `TRUE` (default), keeps
#'   `cluster_label_column`.
#' @param cluster_label_column Character scalar, the label column
#'   (`"cluster_label"`, from [name_clusters()]).
#'
#' @return A `data.table` with one row per node and window: `window` (the
#'   name of the graph in the list), `node_id`, the dynamic cluster, the
#'   colour and label columns if kept, `y_alluv` (\eqn{1/n_t}, \eqn{n_t}
#'   being the number of nodes of the window, so that each window has a
#'   total height of 1), and, with `summary_cluster_stats = TRUE`:
#'
#'   - `share_cluster_alluv`: the share of all rows that belong to the
#'     cluster, in per cent;
#'   - `share_cluster_window`: the share of the window's nodes in the
#'     cluster, in per cent;
#'   - `share_cluster_max`: the maximum of `share_cluster_window` over the
#'     windows;
#'   - `length_cluster`: the number of windows where the cluster exists.
#'
#' @details
#' The alluvial method (following clusters over time with `networks_to_alluv()`,
#' `minimize_crossing_alluvial()` and `plot_alluvial()`) was initiated by
#' Alexandre Truc.
#'
#' @seealso [minimize_crossing_alluvial()], [prepare_label_alluvial()],
#'   [color_alluvial()] and [plot_alluvial()].
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
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
#'
#' alluv_dt <- networks_to_alluv(
#'   networks,
#'   intertemporal_cluster_column = "dynamic_cluster_leiden",
#'   node_id = "source_id"
#' )
#' alluv_dt[1:5]
#'
#' @export
networks_to_alluv <- function(graphs,
                              intertemporal_cluster_column,
                              node_id,
                              summary_cluster_stats = TRUE,
                              keep_color = TRUE,
                              color_column = "color",
                              keep_cluster_label = TRUE,
                              cluster_label_column = "cluster_label"){


  . <- y_alluv <- window <- share_cluster_alluv <- intertemporal_name <- tot_window_leiden <- tot_window <- share_cluster_window <- share_cluster_max <- head <- N <- nodes <- NULL

  columns <- c(node_id,
               intertemporal_cluster_column,
               if(!is.null(color_column) & keep_color) color_column,
               if(!is.null(cluster_label_column) & keep_cluster_label) cluster_label_column)

  alluv_dt <- lapply(graphs,
                     function(tbl)(tbl %N>% data.table::as.data.table())) %>%
    data.table::rbindlist(idcol = "window") %>%
    dplyr::select(window, dplyr::any_of(columns)) # We keep all the need columns

  # Windows are kept in the order of the list (chronological), not in
  # alphabetical order ("10" would come before "2" for unnamed windows)
  alluv_dt[, window := factor(window, levels = unique(window))]

  if(is.null(color_column) | keep_color == FALSE){
    NULL
  }
  if(! color_column %in% colnames(alluv_dt) & keep_color){
    cli::cli_inform(c("i" = "No {.field {color_column}} column in the graphs: no colour kept (see {.fn color_alluvial})."))
  }
  if(! cluster_label_column %in% colnames(alluv_dt) & keep_cluster_label){
    cli::cli_inform(c("i" = "No {.field {cluster_label_column}} column in the graphs: no label kept (see {.fn name_clusters})."))
  }

  if(summary_cluster_stats){
    alluv_dt[,share_cluster_alluv:= round(.N/alluv_dt[,.N], 4) * 100, intertemporal_cluster_column,
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # share of cl in alluv
    alluv_dt[,tot_window_leiden := .N, .(window,intertemporal_cluster_column),
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    alluv_dt[,tot_window := .N, .(window)]
    alluv_dt[,share_cluster_window := round(tot_window_leiden/tot_window, 4) * 100] # share of cl in time window
    alluv_dt[,share_cluster_max := max(share_cluster_window), intertemporal_cluster_column,
             env = list(intertemporal_cluster_column = intertemporal_cluster_column)] # max share of cl in all time windows
    alluv_dt[,tot_window_leiden := NULL]
    alluv_dt[,tot_window := NULL]

    n_years <- alluv_dt[, head(.SD, 1), .(window, intertemporal_cluster_column),
                        env = list(intertemporal_cluster_column = intertemporal_cluster_column)][,.N, intertemporal_cluster_column, env = list(intertemporal_cluster_column = intertemporal_cluster_column)]
    n_years <- n_years %>%
      dplyr::rename(length_cluster = N)

    alluv_dt <- merge(alluv_dt,
                      n_years,
                      by = intertemporal_cluster_column,
                      all.x = TRUE) %>%  # length of cl
      .[order(window)]
  }
  alluv_dt[, y_alluv := 1/.N, window]

  return (alluv_dt)
}


