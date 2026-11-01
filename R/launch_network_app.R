#' Launch an Interactive Shiny App to Explore a Network Graph
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'
#'
#' Interpreting clusters means going back and forth between the structure of
#' the network and its content: which documents form this cluster, who wrote
#' them, when, which ones sit at its border? `launch_network_app()` opens a
#' local Shiny application for this reading. The network is drawn with
#' interactive nodes (with tooltips) and cluster labels; clicking a cluster
#' displays the metadata of its nodes (`cluster_information`) in a
#' searchable table. Node size, edge width, labels and the visibility of
#' edges can be adjusted to make dense networks readable. For a list of
#' networks (one per time window), a menu selects the window.
#'
#' If `layout` is given, coordinates are computed with [layout_networks()],
#' replacing any existing `x` and `y`; with `layout = NULL`, the existing
#' `x` and `y` columns are used. The app needs the shiny, ggiraph, DT and
#' shinycssloaders packages.
#'
#' @param graph_tbl A `tbl_graph` object with or without layout coordinates.
#' @param cluster_id Column name in the node data identifying clusters. It can be the cluster ID or a unique label for each cluster.
#' @param cluster_information Character vector of node metadata columns to display in the data table.
#' @param cluster_tooltip Optional. Fixed text string to display when hovering over cluster labels (not a column name).
#' @param node_id Column name identifying node IDs.
#' @param node_tooltip Optional. Column name for tooltips shown when hovering over nodes.
#' @param node_size Optional. Column name used to scale node size.
#' @param color Optional. Column name used to color nodes. If `NULL`, colors are automatically assigned using `networkflow::color_networks(column_to_color = cluster_id)`.
#' @param layout Character. Name of a layout to compute node coordinates (e.g. `"fr"`, `"kk"`). If specified, this replaces any existing layout. If `NULL`, existing `x`/`y` coordinates are used.
#'
#' @return A Shiny app interface for exploring the network.
#' @export
#'
#' @examples
#' \dontrun{
#' network <- build_network(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_angle",
#'   edges_threshold = 1
#' )
#' network <- add_clusters(network, clustering_method = "leiden",
#'                         objective_function = "modularity", seed = 123)
#'
#' launch_network_app(
#'   graph_tbl = network,
#'   cluster_id = "cluster_leiden",
#'   cluster_information = c("source_author", "source_title", "source_year", "source_journal"),
#'   node_id = "source_id",
#'   node_tooltip = "source_label",
#'   layout = "fr"
#' )
#'
#' # One network per time window: the app shows a menu of windows
#' networks <- build_dynamic_networks(
#'   nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 20,
#'   window_step = 1,
#'   cooccurrence_method = "coupling_angle"
#' )
#' networks <- add_clusters(networks, clustering_method = "leiden",
#'                          objective_function = "modularity", seed = 123)
#' launch_network_app(
#'   graph_tbl = networks,
#'   cluster_id = "cluster_leiden",
#'   cluster_information = c("source_author", "source_title", "source_year"),
#'   node_id = "source_id",
#'   layout = "fr"
#' )
#' }
launch_network_app <- function(
  graph_tbl,
  cluster_id,
  cluster_information,
  cluster_tooltip = NULL,
  node_id,
  node_tooltip = NULL,
  node_size = NULL,
  color = NULL,
  layout = "fr"
) {
  rlang::check_installed(c("shiny", "ggiraph", "DT", "shinycssloaders"),
                         reason = "to run the network app.")
  settings <- app_settings(
    graph_tbl, cluster_id, cluster_information, cluster_tooltip, node_id,
    node_tooltip, node_size, color, layout
  )
  shiny::shinyApp(
    ui = app_ui(settings),
    server = app_server(settings)
  )
}

# ---------------------------------------------------------------------------
# Internal pieces of the app. They are separate functions so that they can be
# tested without starting a server: `app_settings()` (checks and preparation
# of the graphs), `app_graph()` (the graph of one window), `app_plot()` (the
# ggplot of the network), `app_cluster_nodes()` (the nodes of the selected
# cluster), `app_ui()` and `app_server()` (testable with shiny::testServer()).
# ---------------------------------------------------------------------------

# Check the arguments, compute the layout and the colours when needed, and
# return everything the UI and the server need in one list.
app_settings <- function(graph_tbl, cluster_id, cluster_information, cluster_tooltip,
                         node_id, node_tooltip, node_size, color, layout) {
  single <- is_single_graph(graph_tbl, arg = "graph_tbl")
  graphs <- if (single) list(graph_tbl) else graph_tbl
  if (is.null(names(graphs))) names(graphs) <- as.character(seq_along(graphs))
  if (is.null(cluster_information) || length(cluster_information) == 0) {
    cli::cli_abort("{.arg cluster_information} must be a non-empty character vector of node columns to display.")
  }

  if (is.null(layout)) {
    without_xy <- names(graphs)[!vapply(graphs, function(g) all(c("x", "y") %in% names(node_table(g))), logical(1))]
    if (length(without_xy) > 0) {
      cli::cli_abort(c(
        "The nodes have no layout coordinates {.field x} and {.field y}.",
        "i" = "Compute them with {.fn layout_networks}, or set {.arg layout}."
      ))
    }
  } else {
    if (is.null(node_id) || !nzchar(node_id)) {
      cli::cli_abort("{.arg node_id} is required when {.arg layout} is not {.code NULL}.")
    }
    cli::cli_inform(c("i" = "Computing the {.val {layout}} layout with {.fn layout_networks}."))
    graphs <- layout_networks(graphs, node_id = node_id, layout = layout)
  }
  if (is.null(color)) {
    graphs <- color_networks(graphs, column_to_color = cluster_id, verbose = FALSE)
    color <- "color"
  }
  if (is.null(node_tooltip)) {
    cli::cli_inform(c("i" = "No {.arg node_tooltip}: nodes have no tooltip."))
  }
  graphs <- lapply(graphs, app_graph, cluster_id = cluster_id, node_id = node_id,
                   cluster_information = cluster_information, node_size = node_size,
                   color = color)

  list(
    graphs = graphs, single = single, cluster_id = cluster_id,
    cluster_information = cluster_information, cluster_tooltip = cluster_tooltip,
    node_tooltip = node_tooltip, color = color
  )
}

# One graph of the app: constant or given node size, constant edge weight when
# absent, and a check that the needed columns exist.
app_graph <- function(graph, cluster_id, node_id, cluster_information, node_size, color) {
  nodes <- node_table(graph)
  if (!is.null(node_size)) check_columns(nodes, node_size, "graph_tbl")
  graph <- graph %N>%
    dplyr::mutate(size = if (is.null(node_size)) 1 else .data[[node_size]])
  if (!"weight" %in% names(tidygraph::as_tibble(graph, active = "edges"))) {
    graph <- graph %E>% dplyr::mutate(weight = 1) %>% tidygraph::activate("nodes")
  }
  nodes <- node_table(graph)
  missing_main <- setdiff(c(cluster_id, node_id, color, "size", "x", "y"), names(nodes))
  missing_info <- setdiff(cluster_information, names(nodes))
  if (length(missing_main) > 0 || length(missing_info) > 0) {
    cli::cli_abort(c(
      "The nodes miss some columns needed by the app.",
      "x" = if (length(missing_main) > 0) "Missing: {.field {missing_main}}.",
      "x" = if (length(missing_info) > 0) "Missing from {.arg cluster_information}: {.field {missing_info}}."
    ))
  }
  graph
}

# The nodes of the selected cluster(s), or all the nodes when none is selected.
app_cluster_nodes <- function(nodes, cluster_id, selected) {
  if (is.null(selected) || length(selected) == 0) {
    return(nodes)
  }
  nodes[as.character(nodes[[cluster_id]]) %in% selected, , drop = FALSE]
}

# The interactive ggplot of one network.
app_plot <- function(graph, settings, edge_width_range = c(0.1, 1),
                     node_size_range = c(2, 6), show_edges = FALSE,
                     max_overlaps = 10) {
  nodes <- node_table(graph)
  color_sym <- rlang::sym(settings$color)
  cluster_sym <- rlang::sym(settings$cluster_id)
  aes_args <- list(x = quote(x), y = quote(y), fill = color_sym, size = quote(size))
  if (!is.null(settings$node_tooltip)) {
    aes_args$tooltip <- rlang::sym(settings$node_tooltip)
  }
  label_df <- nodes %>%
    dplyr::group_by(!!cluster_sym) %>%
    dplyr::summarise(
      label_x = mean(x),
      label_y = mean(y),
      color = dplyr::first(!!color_sym),
      cluster_label = as.character(dplyr::first(!!cluster_sym)),
      .groups = "drop"
    )
  label_df$tooltip <- if (is.null(settings$cluster_tooltip)) "" else settings$cluster_tooltip

  plot <- ggraph::ggraph(graph, layout = "manual", x = x, y = y) +
    ggiraph::geom_point_interactive(
      do.call(ggplot2::aes, aes_args),
      shape = 21, alpha = 0.8, show.legend = FALSE
    ) +
    ggiraph::geom_label_repel_interactive(
      data = label_df,
      ggplot2::aes(x = label_x, y = label_y, label = cluster_label,
                   data_id = cluster_label, fill = color, tooltip = tooltip),
      alpha = 0.9, size = 4, fontface = "bold", max.overlaps = max_overlaps,
      show.legend = FALSE
    ) +
    ggraph::scale_edge_width_continuous(range = edge_width_range) +
    ggplot2::scale_size_continuous(range = node_size_range) +
    ggraph::scale_edge_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
  if (show_edges) {
    plot <- plot +
      ggraph::geom_edge_link0(ggplot2::aes(color = !!color_sym, width = weight),
                              alpha = 0.3, show.legend = FALSE)
  }
  plot
}

# A label with an information tooltip, for the sidebar.
app_panel_title <- function(title, tooltip) {
  shiny::div(
    title,
    shiny::span("\u24D8", title = tooltip,
                style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;")
  )
}

app_ui <- function(settings) {
  windows <- names(settings$graphs)
  shiny::fluidPage(
    shiny::titlePanel("Network Explorer"),
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 2,
        shiny::div(
          style = "max-height: 75vh; overflow-y: auto;",
          shiny::h4("Graph Settings"),
          shiny::wellPanel(
            app_panel_title("Edges", "Disabled by default to keep the app lightweight; enable if you need to inspect connections."),
            shiny::checkboxInput("show_edges", "Show edges", value = FALSE)
          ),
          if (!settings$single) {
            shiny::wellPanel(
              app_panel_title("Time window", "Consider changing if you want to compare different time slices."),
              shiny::selectInput("time_window", "Time window", choices = windows, selected = windows[1])
            )
          },
          shiny::wellPanel(
            app_panel_title("Edge width", "Consider increasing if edges are too faint or hard to see."),
            shiny::sliderInput("min_edge_width", "Min edge width", min = 0.01, max = 5, value = 0.1, step = 0.05),
            shiny::sliderInput("max_edge_width", "Max edge width", min = 0.01, max = 5, value = 1, step = 0.1)
          ),
          shiny::wellPanel(
            app_panel_title("Node size", "Consider increasing if nodes are too small or hard to select."),
            shiny::sliderInput("min_node_size", "Min node size", min = 0.1, max = 10, value = 2, step = 0.5),
            shiny::sliderInput("max_node_size", "Max node size", min = 0.1, max = 15, value = 6, step = 0.5)
          ),
          shiny::wellPanel(
            app_panel_title("Labels", "Consider increasing if too many labels are hidden. Consider enabling 'Show all labels' to force all labels even with overlaps."),
            shiny::sliderInput("label_max_overlaps", "Label max overlaps", min = 0, max = 50, value = 10, step = 1),
            shiny::checkboxInput("label_allow_overlap", "Show all labels (ignore overlaps)", value = FALSE)
          )
        )
      ),
      mainPanel = shiny::mainPanel(
        width = 10,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::div(
              style = "border: 1px solid #ccc; padding: 8px; border-radius: 5px; background-color: transparent; height: 75vh;",
              shiny::h4("Interactive network"),
              shinycssloaders::withSpinner(
                ggiraph::girafeOutput("network_plot", width = "100%", height = "60vh")
              )
            )
          ),
          shiny::column(
            width = 6,
            shiny::div(
              style = "border: 1px solid #ccc; padding: 8px; border-radius: 5px; background-color: transparent; height: 75vh; overflow-y: auto;",
              shiny::uiOutput("cluster_header"),
              DT::DTOutput("cluster_docs")
            )
          )
        )
      )
    )
  )
}

# The server function of the app. `settings` comes from app_settings(). The
# reactive values (`graph_selected`, `nodes_shown`) are returned in the
# session's `returned` list so that shiny::testServer() can inspect them.
app_server <- function(settings) {
  function(input, output, session) {
    graph_selected <- shiny::reactive({
      if (settings$single) settings$graphs[[1]] else settings$graphs[[input$time_window]]
    })

    selected_clusters <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$network_plot_selected, {
      selected_clusters(input$network_plot_selected)
    }, ignoreInit = TRUE)

    nodes_shown <- shiny::reactive({
      app_cluster_nodes(node_table(graph_selected()), settings$cluster_id, selected_clusters())
    })

    output$network_plot <- ggiraph::renderGirafe({
      plot <- app_plot(
        graph_selected(), settings,
        edge_width_range = c(input$min_edge_width, input$max_edge_width),
        node_size_range = c(input$min_node_size, input$max_node_size),
        show_edges = isTRUE(input$show_edges),
        max_overlaps = if (isTRUE(input$label_allow_overlap)) Inf else input$label_max_overlaps
      )
      ggiraph::girafe(
        ggobj = plot, width_svg = 10, height_svg = 6,
        options = list(
          ggiraph::opts_selection(type = "single"),
          ggiraph::opts_zoom(min = 1, max = 12),
          ggiraph::opts_toolbar(position = "topright")
        )
      )
    })

    output$cluster_header <- shiny::renderUI({
      header <- paste0("Documents in selected cluster: ", nrow(nodes_shown()))
      if (!settings$single) header <- paste0(header, " from ", input$time_window)
      shiny::h4(header)
    })

    output$cluster_docs <- DT::renderDT({
      nodes_shown() %>%
        dplyr::select(dplyr::all_of(settings$cluster_information)) %>%
        DT::datatable(options = list(pageLength = 10))
    })
  }
}
