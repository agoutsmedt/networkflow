#' Launch an Interactive Shiny App to Explore a Network Graph
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#'
#'
#' This function launches an interactive Shiny application for visualizing and exploring a network graph represented as a `tbl_graph` object.
#' It supports node and cluster interactivity with tooltips, dynamic sizing, and customizable coloring.
#' Users can inspect clusters interactively and explore associated node metadata in a searchable datatable.
#' The app is designed for cluster/module exploration, so `cluster_id` and `cluster_information` are required.
#'
#' If the graph does not contain a layout (columns `x` and `y`) the function will compute coordinates using
#' `networkflow::layout_networks()` with the specified layout algorithm.  If a layout name is provided, it will replace any existing layout in the graph.
#'
#' @param graph_tbl A `tbl_graph` object with or without layout coordinates.
#' @param cluster_id Column name in the node data identifying clusters. It can be the cluster ID or a unique label for each cluster.
#' @param cluster_information Character vector of node metadata columns to display in the data table.
#' @param cluster_tooltip Optional. Fixed text string to display when hovering over cluster labels (not a column name).
#' @param node_id Column name identifying node IDs.
#' @param node_tooltip Optional. Column name for tooltips shown when hovering over nodes.
#' @param node_size Optional. Column name used to scale node size.
#' @param color Optional. Column name used to color nodes. If `NULL`, colors are automatically assigned using `networkflow::color_networks(column_to_color = cluster_id)`.
#' @param layout Character. Name of a layout to compute node coordinates (e.g. `"kk"`, `"fr"`). If specified, this replaces any existing layout. If `NULL`, existing `x`/`y` coordinates are used.
#'
#' @return A Shiny app interface for exploring the network.
#' @export
#'
#' @examples
#' library(networkflow)
#' library(dplyr)
#'
#' nodes <- Nodes_stagflation |>
#'   dplyr::filter(Type == "Stagflation") |>
#'   dplyr::rename(ID_Art = ItemID_Ref)
#'
#' references <- Ref_stagflation |>
#'   dplyr::rename(ID_Art = Citing_ItemID_Ref)
#'
#' g <- build_network(
#'   nodes = nodes,
#'   directed_edges = references,
#'   source_id = "ID_Art",
#'   target_id = "ItemID_Ref",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1,
#'   compute_size = FALSE,
#'   keep_singleton = FALSE
#' )
#'
#' g <- add_clusters(
#'   g,
#'   clustering_method = "leiden",
#'   objective_function = "modularity"
#' )
#'
#' \dontrun{
#' launch_network_app(
#'   graph_tbl = g,
#'   cluster_id = "cluster_leiden",
#'   cluster_information = c("Author", "Title", "Year", "Journal"),
#'   cluster_tooltip = "Cluster",
#'   node_id = "ID_Art",
#'   node_tooltip = "Author_date",
#'   node_size = NULL,
#'   color = NULL,
#'   layout = "kk"
#' )
#' }
#'
#' # Dynamic networks
#' g_list <- build_dynamic_networks(
#'   nodes = nodes,
#'   directed_edges = references,
#'   source_id = "ID_Art",
#'   target_id = "ItemID_Ref",
#'   time_variable = "Year",
#'   time_window = 20,
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1,
#'   overlapping_window = TRUE,
#'   compute_size = FALSE,
#'   keep_singleton = FALSE
#' )
#'
#' g_list <- add_clusters(
#'   g_list,
#'   clustering_method = "leiden",
#'   objective_function = "modularity"
#' )
#'
#' \dontrun{
#' launch_network_app(
#'   graph_tbl = g_list,
#'   cluster_id = "cluster_leiden",
#'   cluster_information = c("Author", "Title", "Year", "Journal"),
#'   node_id = "ID_Art",
#'   node_tooltip = "Author_date",
#'   node_size = NULL,
#'   color = NULL,
#'   layout = "kk"
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
  layout = "kk"
) {
  required_app_pkgs <- c("shiny", "ggiraph", "DT", "shinycssloaders")
  missing_app_pkgs <- required_app_pkgs[
    !vapply(required_app_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_app_pkgs) > 0) {
    cli::cli_abort(
      paste0(
        "Missing packages for the app: ",
        paste(missing_app_pkgs, collapse = ", "),
        ".\nPlease install them with: install.packages(c(",
        paste(sprintf('"%s"', missing_app_pkgs), collapse = ", "),
        "))"
      )
    )
  }

  stopifnot(
    requireNamespace("ggplot2"),
    requireNamespace("dplyr"),
    requireNamespace("ggraph"),
    requireNamespace("rlang"),
    requireNamespace("tidygraph"),
    requireNamespace("cli")
  )

  is_tbl_graph <- inherits(graph_tbl, "tbl_graph")
  is_list <- is.list(graph_tbl) && !is_tbl_graph
  if (!is_tbl_graph && !is_list) {
    cli::cli_abort(
      "{.arg graph_tbl} must be a {.cls tbl_graph} or a list of {.cls tbl_graph} objects."
    )
  }

  if (is.null(layout)) {
    has_xy <- function(graph) {
      nodes_df <- tidygraph::activate(graph, "nodes") %>% as.data.frame()
      all(c("x", "y") %in% names(nodes_df))
    }
    if (is_list) {
      missing_xy <- names(graph_tbl)[!vapply(graph_tbl, has_xy, logical(1))]
      if (length(missing_xy) > 0) {
        cli::cli_abort(
          "Missing layout coordinates in nodes for: {paste(missing_xy, collapse = ', ')}. Provide {.code x} and {.code y} columns or set {.arg layout}."
        )
      }
    } else if (!has_xy(graph_tbl)) {
      cli::cli_abort(
        "Missing layout coordinates in nodes. Provide {.code x} and {.code y} columns or set {.arg layout}."
      )
    }
  }

  if (!is.null(layout)) {
    if (is.null(node_id) || !nzchar(node_id)) {
      cli::cli_abort(
        "{.arg node_id} is required when {.arg layout} is not {.code NULL}."
      )
    }
    cli::cli_alert_info(
      "Applying layout via {.fn networkflow::layout_networks} with layout = '{layout}'..."
    )
    graph_tbl <- networkflow::layout_networks(
      graphs = graph_tbl,
      node_id = node_id,
      layout = layout
    )
  }

  if (is.null(color)) {
    cli::cli_alert_info(
      "Coloring nodes using {.fn networkflow::color_networks}..."
    )
    graph_tbl <- networkflow::color_networks(
      graph_tbl,
      column_to_color = cluster_id,
      unique_color_across_list = FALSE
    )
    color <- "color"
  }

  color_sym <- rlang::sym(color)
  cluster_sym <- rlang::sym(cluster_id)
  tooltip_sym <- if (!is.null(node_tooltip)) rlang::sym(node_tooltip) else NULL
  if (is.null(tooltip_sym)) {
    cli::cli_alert_info(
      "No {.arg node_tooltip} provided: nodes will have no tooltip."
    )
  }

  if (is_list && is.null(names(graph_tbl))) {
    names(graph_tbl) <- as.character(seq_along(graph_tbl))
  }

  prepare_graph <- function(graph) {
    graph <- tidygraph::activate(graph, "nodes")
    if (is.null(node_size)) {
      cli::cli_alert_info(
        "No {.arg node_size} provided: using a constant size for all nodes."
      )
      graph <- dplyr::mutate(graph, size = 1)
    } else {
      if (!(node_size %in% colnames(as.data.frame(graph)))) {
        cli::cli_abort(
          "The {.arg node_size} column was not found in node data: '{node_size}'."
        )
      }
      graph <- dplyr::mutate(graph, size = !!rlang::sym(node_size))
    }

    graph <- tidygraph::activate(graph, "edges")
    if (!"weight" %in% colnames(as.data.frame(graph))) {
      cli::cli_alert_info(
        "No {.code weight} column found in edges. Assuming an unweighted graph and setting all weights to 1."
      )
      graph <- dplyr::mutate(graph, weight = 1)
    }

    nodes_df <- tidygraph::activate(graph, "nodes") %>% as.data.frame()
    if (is.null(cluster_information) || length(cluster_information) == 0) {
      cli::cli_abort(
        "{.arg cluster_information} must be a non-empty character vector of node columns to display."
      )
    }
    all_req <- c(cluster_id, node_id, color, "size", "x", "y")
    missing_main <- setdiff(all_req, names(nodes_df))
    missing_info <- setdiff(cluster_information, names(nodes_df))
    if (length(missing_main) > 0 || length(missing_info) > 0) {
      parts <- c()
      if (length(missing_main) > 0) {
        parts <- c(
          parts,
          paste0("required: ", paste(missing_main, collapse = ", "))
        )
      }
      if (length(missing_info) > 0) {
        parts <- c(
          parts,
          paste0("cluster_information: ", paste(missing_info, collapse = ", "))
        )
      }
      cli::cli_abort(
        "Missing columns in nodes: {paste(parts, collapse = ' | ')}"
      )
    }

    list(graph = graph, nodes_df = nodes_df)
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("Network Explorer"),

    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 2,
        shiny::div(
          style = "max-height: 75vh; overflow-y: auto;",
          shiny::h4("Graph Settings"),
          shiny::wellPanel(
            shiny::div(
              "Edges",
              shiny::span(
                "\u24D8",
                title = "Disabled by default to keep the app lightweight; enable if you need to inspect connections.",
                style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;"
              )
            ),
            shiny::checkboxInput(
              "show_edges",
              "Show edges",
              value = FALSE
            )
          ),
          if (is_list) {
            shiny::wellPanel(
              shiny::div(
                "Time window",
                shiny::span(
                  "\u24D8",
                  title = "Consider changing if you want to compare different time slices.",
                  style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;"
                )
              ),
              shiny::selectInput(
                "time_window",
                "Time window",
                choices = names(graph_tbl),
                selected = names(graph_tbl)[1]
              )
            )
          },
          shiny::wellPanel(
            shiny::div(
              "Edge width",
              shiny::span(
                "\u24D8",
                title = "Consider increasing if edges are too faint or hard to see.",
                style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;"
              )
            ),
            shiny::sliderInput(
              "min_edge_width",
              "Min edge width",
              min = 0.01,
              max = 5,
              value = 0.1,
              step = 0.05
            ),
            shiny::sliderInput(
              "max_edge_width",
              "Max edge width",
              min = 0.01,
              max = 5,
              value = 1,
              step = 0.1
            )
          ),
          shiny::wellPanel(
            shiny::div(
              "Node size",
              shiny::span(
                "\u24D8",
                title = "Consider increasing if nodes are too small or hard to select.",
                style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;"
              )
            ),
            shiny::sliderInput(
              "min_node_size",
              "Min node size",
              min = 0.1,
              max = 10,
              value = 2,
              step = 0.5
            ),
            shiny::sliderInput(
              "max_node_size",
              "Max node size",
              min = 0.1,
              max = 15,
              value = 6,
              step = 0.5
            )
          ),
          shiny::wellPanel(
            shiny::div(
              "Labels",
              shiny::span(
                "\u24D8",
                title = "Consider increasing if too many labels are hidden. Consider enabling 'Show all labels' to force all labels even with overlaps.",
                style = "cursor: help; margin-left: 6px; font-size: 18px; color: #555;"
              )
            ),
            shiny::sliderInput(
              "label_max_overlaps",
              "Label max overlaps",
              min = 0,
              max = 50,
              value = 10,
              step = 1
            ),
            shiny::checkboxInput(
              "label_allow_overlap",
              "Show all labels (ignore overlaps)",
              value = FALSE
            )
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
                ggiraph::girafeOutput(
                  "network_plot",
                  width = "100%",
                  height = "60vh"
                )
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

  server <- function(input, output, session) {
    if (is_list) {
      graph_selected <- shiny::eventReactive(
        input$time_window,
        {
          graph_tbl[[input$time_window]]
        },
        ignoreInit = FALSE
      )
    } else {
      graph_selected <- shiny::reactive(graph_tbl)
    }

    graph_ready <- shiny::reactive({
      prepare_graph(graph_selected())
    })

    selected_clusters <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      input$network_plot_selected,
      {
        selected_clusters(input$network_plot_selected)
      },
      ignoreInit = TRUE
    )

    output$network_plot <- ggiraph::renderGirafe({
      graph_data <- graph_ready()
      graph_obj <- graph_data$graph
      nodes_df <- graph_data$nodes_df

      edge_width_range <- c(input$min_edge_width, input$max_edge_width)
      node_size_range <- c(input$min_node_size, input$max_node_size)

      aes_args <- list(
        x = quote(x),
        y = quote(y),
        fill = color_sym,
        size = quote(size)
      )
      if (!is.null(tooltip_sym)) {
        aes_args$tooltip <- tooltip_sym
      }

      label_df <- nodes_df %>%
        dplyr::group_by(!!cluster_sym) %>%
        dplyr::summarise(
          label_x = mean(x),
          label_y = mean(y),
          color = first(!!color_sym),
          cluster_label = as.character(first(!!cluster_sym)),
          .groups = "drop"
        )

      label_df$tooltip <- if (is.null(cluster_tooltip)) "" else cluster_tooltip

      max_overlaps <- if (isTRUE(input$label_allow_overlap)) {
        Inf
      } else {
        input$label_max_overlaps
      }

      g <- ggraph::ggraph(graph_obj, layout = "manual", x = x, y = y) +
        ggiraph::geom_point_interactive(
          do.call(ggplot2::aes, aes_args),
          shape = 21,
          alpha = 0.8,
          show.legend = FALSE
        ) +
        ggiraph::geom_label_repel_interactive(
          data = label_df,
          ggplot2::aes(
            x = label_x,
            y = label_y,
            label = cluster_label,
            data_id = cluster_label,
            fill = color,
            tooltip = tooltip
          ),
          alpha = 0.9,
          size = 4,
          fontface = "bold",
          max.overlaps = max_overlaps,
          show.legend = FALSE
        ) +
        ggraph::scale_edge_width_continuous(range = edge_width_range) +
        ggplot2::scale_size_continuous(range = node_size_range) +
        ggraph::scale_edge_colour_identity() +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void()

      if (isTRUE(input$show_edges)) {
        g <- g +
          ggraph::geom_edge_link0(
            ggplot2::aes(color = !!color_sym, width = weight),
            alpha = 0.3,
            show.legend = FALSE
          )
      }

      ggiraph::girafe(
        ggobj = g,
        width_svg = 10,
        height_svg = 6,
        options = list(
          ggiraph::opts_selection(type = "single"),
          ggiraph::opts_zoom(min = 1, max = 12),
          ggiraph::opts_toolbar(position = "topright")
        )
      )
    })

    output$cluster_header <- shiny::renderUI({
      nodes_df <- graph_ready()$nodes_df
      selected <- selected_clusters()
      if (!is.null(selected) && length(selected) > 0) {
        nodes_df <- nodes_df[
          as.character(nodes_df[[cluster_id]]) %in% selected,
          ,
          drop = FALSE
        ]
      }
      doc_count <- nrow(nodes_df)
      header <- if (is_list) {
        paste0(
          "Documents in selected cluster: ",
          doc_count,
          " from ",
          input$time_window
        )
      } else {
        paste0("Documents in selected cluster: ", doc_count)
      }
      shiny::h4(header)
    })

    output$cluster_docs <- DT::renderDT({
      nodes_df <- graph_ready()$nodes_df
      selected <- selected_clusters()
      if (!is.null(selected) && length(selected) > 0) {
        nodes_df <- nodes_df[
          as.character(nodes_df[[cluster_id]]) %in% selected,
          ,
          drop = FALSE
        ]
      }
      nodes_df %>%
        dplyr::select(dplyr::all_of(cluster_information)) %>%
        DT::datatable(options = list(pageLength = 10))
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
