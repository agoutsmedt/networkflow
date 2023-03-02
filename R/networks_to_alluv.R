networks_to_alluv <- function(list_graph = NA,
                              intertemporal_cluster_column = "intertemporal_name",
                              node_id = NA,
                              summary_cl_stats = TRUE,
                              keep_color = TRUE,
                              color_column = "color",
                              keep_cluster_label = TRUE,
                              cluster_label_column = "cluster_label"){

  #' Create a data.frame suitable for alluvial graph projection
  #'
  #' This function creates a data.frame that can be easily plotted with ggalluvial from a list of networks.
  #'
  #' @param list_graph
  #' Your list with all networks
  #'
  #' @param intertemporal_cluster_column
  #' The column with the identifier of the inter-temporal cluster. If you have used
  #' [add_clusters()][networkflow::add_clusters()] and [merge_dynamic_clusters()][networkflow::merge_dynamic_clusters()],
  #' it is of the form `dynamic_cluster_{clustering_method}`.
  #'
  #' @param node_id
  #' The column with the unique identifier of each node.
  #'
  #' @param summary_cl_stats
  #' If set to `TRUE`, the data.frame will contain a list of variable that summarize cluster statistics of the alluvial. These variables
  #' can be particularly useful to filter smaller communities when plotting according to different variables:
  #'
  #' - `share_cluster_alluv` is the percentage share of a given cluster across all time windows;
  #' - `share_cluster_window` is the percentage share of a given cluster in a given time window;
  #' - `share_cluster_max` is the highest value of `share_cluster_window` for a given cluster across all individual time windows;
  #' - `length_cluster` is the number of time windows a cluster exists.
  #'
  #' @param keep_color
  #' Set to `TRUE` (by default) if you want to keep the column with the color associated to
  #' the different categories of `intertemporal_cluster_column`. Such a column exists in your
  #' list of tibble graphs if you have use [color_networks()][networkflow::color_networks()].
  #'
  #' @param color_column
  #' The name of the column with the colors of the categories in `intertemporal_cluster_column`.
  #' By default, "color", as it is the column name resulting from the use of
  #' [color_networks()][networkflow::color_networks()].
  #'
  #' @param keep_cluster_label
  #' Set to `TRUE` if you want to keep the column with a name/label associated to the
  #' different categories of `intertemporal_cluster_column`. Such a column exists in your
  #' list of tibble graphs if you have use [name_clusters()][networkflow::name_clusters()].
  #'
  #' @param cluster_label_column
  #' The name of the column with the name/label associated to the categories in
  #' `intertemporal_cluster_column`. By default, "cluster_label", as it is the column name
  #' resulting from the use of [name_clusters()][networkflow::name_clusters()].
  #'
  #' @examples
  #' library(networkflow)
  #'
  #' nodes <- Nodes_stagflation |>
  #' dplyr::rename(ID_Art = ItemID_Ref) |>
  #' dplyr::filter(Type == "Stagflation")
  #'
  #' references <- Ref_stagflation |>
  #' dplyr::rename(ID_Art = Citing_ItemID_Ref)
  #'
  #' temporal_networks <- build_dynamic_networks(nodes = nodes,
  #' directed_edges = references,
  #' source_id = "ID_Art",
  #' target_id = "ItemID_Ref",
  #' time_variable = "Year",
  #' cooccurrence_method = "coupling_similarity",
  #' time_window = 20,
  #' edges_threshold = 1,
  #' overlapping_window = TRUE,
  #' filter_components = TRUE,
  #' verbose = FALSE)
  #'
  #' temporal_networks <- add_clusters(temporal_networks,
  #' objective_function = "modularity",
  #' clustering_method = "leiden",
  #' verbose = FALSE)
  #'
  #' temporal_networks <- merge_dynamic_clusters(temporal_networks,
  #' cluster_id = "cluster_leiden",
  #' node_id = "ID_Art",
  #' threshold_similarity = 0.51,
  #' similarity_type = "partial")
  #'
  #' temporal_networks <- name_clusters(graphs = temporal_networks,
  #' method = "tf-idf",
  #' name_merged_clusters = TRUE,
  #' cluster_id = "dynamic_cluster_leiden",
  #' text_columns = "Title",
  #' nb_terms_label = 5,
  #' clean_word_method = "lemmatise")
  #'
  #' temporal_networks <- color_networks(graphs = temporal_networks,
  #' column_to_color = "dynamic_cluster_leiden",
  #' color = NULL)
  #'
  #' alluv_dt <- networks_to_alluv(temporal_networks,
  #' intertemporal_cluster_column = "dynamic_cluster_leiden",
  #' node_id = "ID_Art")
  #'
  #' alluv_dt[1:5]
  #'
  #' @export

  . <- y_alluv <- window <- share_cluster_alluv <- intertemporal_name <- tot_window_leiden <- tot_window <- share_cluster_window <- share_cluster_max <- head <- N <- nodes <- NULL

  columns <- c(node_id,
               intertemporal_cluster_column,
               if(!is.null(color_column) & keep_color) color_column,
               if(!is.null(cluster_label_column) & keep_cluster_label) cluster_label_column)

  alluv_dt <- lapply(list_graph,
                     function(tbl)(tbl %N>% data.table::as.data.table())) %>%
    data.table::rbindlist(idcol = "window") %>%
    dplyr::select(window, dplyr::any_of(columns)) # We keep all the need columns

  if(!is.null(color_column) | keep_color == FALSE){
    cli::cli_alert_info("You did not use any {.emph color} column. If you want to plot the alluvial, you can use {.fn color_alluvial}.")
  }
  if(! color_column %in% colnames(alluv_dt) & keep_color){
    cli::cli_alert_info("The column \"{.emph {color_column}}\" does not exist in the list of graphs provided. No color kept for clusters.")
  }
  if(! cluster_label_column %in% colnames(alluv_dt) & keep_cluster_label){
    cli::cli_alert_info("The column \"{.emph {cluster_label_column}}\" does not exist in the list of graphs provided. No name kept for clusters.")
  }

  if(summary_cl_stats == TRUE){
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
  alluv_dt[, y_alluv :=1/.N, window]

  return (alluv_dt)
}

