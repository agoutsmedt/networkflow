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
#'
#' If the graph does not contain a layout (columns `x` and `y`) the function will compute coordinates using
#' `networkflow::layout_networks()` with the specified layout algorithm.  If a layout name is provided, it will replace any existing layout in the graph.
#'
#' @param graph_tbl A `tbl_graph` object with or without layout coordinates.
#' @param cluster_id Column name in the node data identifying clusters. It can be the cluster ID or a unique label for each cluster.
#' @param cluster_information Character vector of node metadata columns to display in the data table.
#' @param cluster_tooltip Optional. Text string to display when hovering over cluster labels.
#' @param node_id Column name identifying node IDs.
#' @param node_tooltip Optional. Column name for tooltips shown when hovering over nodes.
#' @param node_size Optional. Column name used to scale node size.
#' @param color Optional. Column name used to color nodes. If `NULL`, colors are automatically assigned using `networkflow::color_networks(column_to_color = cluster_id)`.
#' @param layout Character. Name of a layout to compute node coordinates (e.g. `"kk"`, `"fr"`). If specified, this replaces any existing layout.
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
  stopifnot(
    requireNamespace("shiny"),
    requireNamespace("ggiraph"),
    requireNamespace("ggplot2"),
    requireNamespace("DT"),
    requireNamespace("dplyr"),
    requireNamespace("ggraph"),
    requireNamespace("rlang"),
    requireNamespace("shinycssloaders"),
    requireNamespace("tidygraph"),
    requireNamespace("cli")
  )

  if (!is.null(layout)) {
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
  id_sym <- rlang::sym(node_id)
  tooltip_sym <- if (!is.null(node_tooltip)) rlang::sym(node_tooltip) else NULL

  is_list <- is.list(graph_tbl) && !inherits(graph_tbl, "tbl_graph")
  if (is_list && is.null(names(graph_tbl))) {
    names(graph_tbl) <- as.character(seq_along(graph_tbl))
  }

  prepare_graph <- function(graph) {
    graph <- tidygraph::activate(graph, "nodes")
    if (is.null(node_size)) {
      graph <- dplyr::mutate(graph, size = 1)
    } else {
      if (!(node_size %in% colnames(as.data.frame(graph)))) {
        cli::cli_abort(
          "The column specified in {.arg node_size} does not exist in the node data."
        )
      }
      graph <- dplyr::mutate(graph, size = !!rlang::sym(node_size))
    }

    graph <- tidygraph::activate(graph, "edges")
    if (!"weight" %in% colnames(as.data.frame(graph))) {
      cli::cli_alert_info(
        "No column `weight` found in edges data. All weight will equal 1."
      )
      graph <- dplyr::mutate(graph, weight = 1)
    }

    nodes_df <- tidygraph::activate(graph, "nodes") %>% as.data.frame()
    all_req <- c(cluster_id, node_id, color, "size", "x", "y")
    missing_main <- setdiff(all_req, names(nodes_df))
    missing_info <- setdiff(cluster_information, names(nodes_df))
    if (length(missing_main) > 0 || length(missing_info) > 0) {
      cli::cli_abort(
        "Missing required columns in nodes: {paste(c(missing_main, missing_info), collapse = ', ')}"
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
          style = "height: 800px; overflow-y: auto;",
          shiny::h4("Graph Settings"),
          shiny::wellPanel(
            shiny::checkboxInput(
              "show_edges",
              "Afficher les liens",
              value = FALSE
            )
          ),
          if (is_list) {
            shiny::wellPanel(
              shiny::selectInput(
                "time_window",
                "Time window:",
                choices = names(graph_tbl),
                selected = names(graph_tbl)[1]
              )
            )
          },
          shiny::wellPanel(
            shiny::sliderInput(
              "min_edge_width",
              "Min Edge Width:",
              min = 0.01,
              max = 5,
              value = 0.1,
              step = 0.05
            ),
            shiny::sliderInput(
              "max_edge_width",
              "Max Edge Width:",
              min = 0.01,
              max = 5,
              value = 1,
              step = 0.1
            )
          ),
          shiny::wellPanel(
            shiny::sliderInput(
              "min_node_size",
              "Min Node Size:",
              min = 0.1,
              max = 10,
              value = 2,
              step = 0.5
            ),
            shiny::sliderInput(
              "max_node_size",
              "Max Node Size:",
              min = 0.1,
              max = 15,
              value = 6,
              step = 0.5
            )
          )
        )
      ),
      mainPanel = shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::div(
              style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: transparent; height: 800px;",
              shiny::h4("Interactive networks"),
              shinycssloaders::withSpinner(
                ggiraph::girafeOutput(
                  "network_plot",
                  width = "100%",
                  height = "600px"
                )
              )
            )
          ),
          shiny::column(
            width = 6,
            shiny::div(
              style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: transparent; height: 800px; overflow-y: auto;",
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

      g <- ggraph::ggraph(graph_obj, layout = "manual", x = x, y = y) +
        ggiraph::geom_point_interactive(
          do.call(ggplot2::aes, aes_args),
          shape = 21,
          alpha = 0.8,
          show.legend = FALSE
        ) +
        ggiraph::geom_label_repel_interactive(
          data = nodes_df %>%
            dplyr::group_by(!!cluster_sym) %>%
            dplyr::summarise(
              label_x = mean(x),
              label_y = mean(y),
              color = first(!!color_sym),
              cluster_label = as.character(first(!!cluster_sym)),
              .groups = "drop"
            ),
          ggplot2::aes(
            x = label_x,
            y = label_y,
            label = cluster_label,
            data_id = cluster_label,
            fill = color
          ) %>%
            {
              if (!is.null(cluster_tooltip)) {
                . + ggplot2::aes(tooltip = I(cluster_tooltip))
              } else {
                .
              }
            },
          alpha = 0.9,
          size = 4,
          fontface = "bold",
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
        options = list(ggiraph::opts_selection(type = "multiple"))
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
