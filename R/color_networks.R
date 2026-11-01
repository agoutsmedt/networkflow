#' Colour the nodes and edges of networks, or the clusters of an alluvial
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `color_networks()` gives a colour to each category of a node column
#' (typically a cluster column) and stores it in a `color` column of the
#' nodes. Each edge gets a colour mixing the colours of its two ends, so that
#' edges within a cluster take the colour of the cluster and edges between
#' clusters a blend of both.
#'
#' `color_alluvial()` does the same for the table of an alluvial plot
#' created with [networks_to_alluv()].
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s.
#' @param alluv_dt A data frame created with [networks_to_alluv()].
#' @param column_to_color Character scalar, the column with the categories
#'   to colour, e.g. `cluster_leiden` ([add_clusters()]) or
#'   `dynamic_cluster_leiden` ([merge_dynamic_clusters()]).
#' @param color The colours: `NULL` (default palettes, see the details), a
#'   character vector of colours, given to the categories by decreasing
#'   number of nodes, or a data frame whose first column holds the categories
#'   and the second column, named `color`, their colours.
#' @param unique_color_across_list Logical, for a list of graphs. `FALSE`
#'   (default) gives a category the same colour in every graph: use it for
#'   dynamic clusters. `TRUE` treats the same value in two graphs as two
#'   categories (cluster "01" of two windows gets two colours): use it for
#'   clusters detected separately in each window.
#' @param verbose Logical. If `TRUE` (default), reports when colours are
#'   recycled or truncated.
#'
#' @details
#' Giving the same colour to a cluster in every figure (network plots,
#' alluvial, tables) is what lets a reader follow it. Pass the same
#' data frame of colours to `color_networks()` and `color_alluvial()` for
#' this.
#'
#' Without `color`, the function uses the ggplot2 palette (without black)
#' for up to 7 categories, completed by the Okabe-Ito palette (without black
#' and grey) for up to 14; beyond 14 categories, the colours are recycled and
#' several clusters share a colour. For many clusters, give a palette
#' explicitly (e.g. from the scico or paletteer packages), or colour only the
#' largest clusters and give the others a neutral grey.
#'
#' A vector with fewer colours than categories is recycled, a longer one is
#' truncated. A data frame of colours that misses some categories leaves
#' their colour missing.
#'
#' @return `color_networks()`: the same object as `graphs` with a `color`
#'   column for nodes and edges. `color_alluvial()`: `alluv_dt` with a
#'   `color` column.
#'
#' @examples
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1
#' )
#' network <- add_clusters(
#'   network,
#'   clustering_method = "leiden",
#'   objective_function = "modularity",
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' network <- color_networks(network, column_to_color = "cluster_leiden")
#' unique(tidygraph::as_tibble(network, active = "nodes")[c("cluster_leiden", "color")])
#'
#' @export
color_networks <- function(graphs,
                           column_to_color,
                           color = NULL,
                           unique_color_across_list = FALSE,
                           verbose = TRUE) {
  . <- to <- from <- window <- N <- data <- NULL
  single <- is_single_graph(graphs)
  if (single) graphs <- list(graphs)
  for (graph in graphs) check_columns(node_table(graph), column_to_color, "graphs")

  # 1. The categories, ordered by decreasing number of nodes, so that the
  #    first colours go to the largest clusters.
  categories <- lapply(graphs, function(graph) {
    data.table::as.data.table(node_table(graph))[, .SD, .SDcols = column_to_color]
  })
  categories <- data.table::rbindlist(categories, idcol = "window")
  if (unique_color_across_list && !single) {
    categories[, (column_to_color) := paste(window, get(column_to_color), sep = "\x1f")]
  }
  categories <- categories[, .N, by = column_to_color][order(-N)][[column_to_color]]
  n_colors <- length(categories)

  # 2. The colour of each category.
  colors_table <- category_colors(categories, color, column_to_color, verbose = verbose)

  # 3. Node colours, then edge colours as a mix of both ends.
  if (unique_color_across_list && !single) {
    colors_table[, window := sub("\x1f.*$", "", get(column_to_color))]
    colors_table[, (column_to_color) := sub("^[^\x1f]*\x1f", "", get(column_to_color))]
    for (i in seq_along(graphs)) {
      table_i <- colors_table[window == names(graphs)[i], .SD, .SDcols = c(column_to_color, "color")]
      graphs[[i]] <- join_node_colors(graphs[[i]], table_i, column_to_color)
    }
  } else {
    graphs <- lapply(graphs, join_node_colors, colors_table = colors_table, column_to_color = column_to_color)
  }
  graphs <- lapply(graphs, function(graph) {
    graph %E>%
      dplyr::mutate(color = mixcolor(.N()$color[to], .N()$color[from], amount1 = 0.5)) %>%
      tidygraph::activate("nodes")
  })
  if (single) graphs <- graphs[[1]]
  graphs
}

# Join the colour table to the nodes, keeping the type of the category column
# (cluster identifiers may be character or numeric).
join_node_colors <- function(graph, colors_table, column_to_color) {
  keys <- as.character(colors_table[[column_to_color]])
  graph %N>%
    dplyr::mutate(color = colors_table$color[match(as.character(.data[[column_to_color]]), keys)])
}

# Table with one colour per category: from a vector of colours, from a data
# frame (category, color), or from the default palettes.
category_colors <- function(categories, color, column_to_color, verbose = TRUE,
                            call = rlang::caller_env()) {
  n_colors <- length(categories)
  if (is.character(color)) {
    if (length(color) != n_colors && verbose) {
      cli::cli_inform(c(
        "i" = "{length(color)} colour{?s} for {n_colors} categor{?y/ies}: colours are {if (length(color) < n_colors) 'recycled' else 'truncated'}."
      ))
    }
    colors_table <- data.table::data.table(categories, color = rep(color, length.out = n_colors))
  } else if (is.data.frame(color)) {
    colors_table <- data.table::as.data.table(color)
    if (!"color" %in% names(colors_table)) {
      data.table::setnames(colors_table, 2, "color")
    }
    data.table::setnames(colors_table, 1, "categories")
    missing_categories <- setdiff(categories, colors_table$categories)
    if (length(missing_categories) > 0) {
      cli::cli_warn(c(
        "{length(missing_categories)} categor{?y/ies} of {.field {column_to_color}} {cli::qty(length(missing_categories))}{?has/have} no colour in {.arg color}.",
        "i" = "{cli::qty(length(missing_categories))}{?Its/Their} colour is missing."
      ), call = call)
    }
    colors_table <- colors_table[, .(categories, color)]
  } else if (is.null(color)) {
    palette <- c(
      grDevices::palette.colors(8, "ggplot2")[-1],
      as.character(grDevices::palette.colors(8, "Okabe-Ito")[-c(1, 9)])
    )
    palette <- unname(palette[!duplicated(palette)])
    if (n_colors > length(palette)) {
      cli::cli_warn(c(
        "{n_colors} categories but {length(palette)} default colours: colours are recycled.",
        "i" = "Give a palette in {.arg color} (e.g. {.code scico::scico({n_colors}, palette = \"batlow\")})."
      ), call = call)
    }
    colors_table <- data.table::data.table(categories, color = rep(palette, length.out = n_colors))
  } else {
    cli::cli_abort(
      "{.arg color} must be {.code NULL}, a character vector of colours or a data frame.",
      call = call
    )
  }
  data.table::setnames(colors_table, "categories", column_to_color)
  colors_table
}

#' @rdname color_networks
#' @export
#'
color_alluvial <- function(alluv_dt,
                           column_to_color,
                           color = NULL,
                           verbose = TRUE) {
  . <- N <- NULL
  if (!is.data.frame(alluv_dt)) {
    cli::cli_abort("{.arg alluv_dt} must be a data frame, as created by {.fn networks_to_alluv}.")
  }
  check_columns(alluv_dt, column_to_color, "alluv_dt")
  categories <- data.table::as.data.table(alluv_dt)[, .N, by = column_to_color][order(-N)][[column_to_color]]
  colors_table <- category_colors(categories, color, column_to_color, verbose = verbose)
  alluv_dt <- data.table::copy(data.table::as.data.table(alluv_dt))
  keys <- as.character(colors_table[[column_to_color]])
  alluv_dt[, color := colors_table$color[match(as.character(get(column_to_color)), keys)]]
  alluv_dt[]
}
