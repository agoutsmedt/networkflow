#' Add Node Roles to Graphs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Adds node-level roles following
#' \insertCite{guimera2005}{networkflow} to a tidygraph
#' or a list of tidygraphs. The function computes within-module degree,
#' participation coefficient, within-module z-score, and the seven role
#' categories, and adds them as new node attributes.
#'
#' @details
#' This function treats the graph as undirected. If your graph is directed, the function will ignore edge directions and compute roles based on the undirected version of the graph.
#' The role classification is based on the within-module degree z-score and the participation coefficient, following the method proposed by Guimera and Amaral (2005).
#' The `z_threshold` parameter can be adjusted to change the sensitivity of hub detection.
#'
#' @param graphs
#' A tibble graph from [tidygraph](https://tidygraph.data-imaginist.com/) or a
#' list of tibble graphs.
#'
#' @param module_col
#' Name of the node column containing module/cluster identifiers.
#'
#' @param weight_col
#' Name of the edge column containing weights.
#'
#' @param z_threshold
#' Threshold for hub detection in the within-module z-score.
#'
#' @return The same tibble graph or list of tibble graphs as input, with
#' added node columns: `within_module_degree` (within-module edge weight sum),
#' `within_module_z` (within-module degree z-score),
#' `participation_coeff` (participation coefficient across modules),
#' `role_ga` (Guimera-Amaral role label).
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
#' clustering_method = "leiden",
#' objective_function = "modularity",
#' verbose = FALSE)
#'
#' temporal_networks <- add_node_roles(temporal_networks,
#' module_col = "cluster_leiden")
#'
#' temporal_networks[[1]]
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
add_node_roles <- function(
  graphs,
  module_col = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
) {
  if (inherits(graphs, "list")) {
    return(lapply(graphs, function(graph) {
      add_node_roles_one(
        graph,
        module_col = module_col,
        weight_col = weight_col,
        z_threshold = z_threshold
      )
    }))
  }

  if (inherits(graphs, "tbl_graph")) {
    return(add_node_roles_one(
      graphs,
      module_col = module_col,
      weight_col = weight_col,
      z_threshold = z_threshold
    ))
  }

  cli::cli_abort(
    "Your {.field graphs} data is neither a tibble graph, nor a list of tibble graphs."
  )
}

add_node_roles_one <- function(
  graph,
  module_col = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
) {
  . <- .node_id <- module <- module_from <- module_to <- node_id <- w <- k_i <- k_is <- kappa <- participation <- NULL
  within_module_z <- within_module_degree <- participation_coeff <- role_ga <- NULL
  kappa_mean <- kappa_sd <- NULL

  # Check if the module column exists in the node data
  if (
    !module_col %in%
      names(graph %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
  ) {
    cli::cli_abort("Column {.field {module_col}} is missing from node data.")
  }

  # use name col as variable in dplyr
  module_sym <- rlang::sym(module_col)

  nodes_tbl <- graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(.node_id = dplyr::row_number()) %>%
    dplyr::select(.node_id, module = !!module_sym)

  edges_tbl <- graph %>%
    tidygraph::activate(edges) %>%
    dplyr::as_tibble()

  # checking for from and to columns in edges data
  from_col <- if ("from" %in% names(edges_tbl)) {
    "from"
  } else if (".from" %in% names(edges_tbl)) {
    ".from"
  } else {
    cli::cli_abort("No {.field from} or {.field .from} column found in edges.")
  }

  to_col <- if ("to" %in% names(edges_tbl)) {
    "to"
  } else if (".to" %in% names(edges_tbl)) {
    ".to"
  } else {
    cli::cli_abort("No {.field to} or {.field .to} column found in edges.")
  }

  edges_tbl <- edges_tbl %>%
    dplyr::mutate(
      .from = as.integer(.data[[from_col]]),
      .to = as.integer(.data[[to_col]])
    )

  # checking for weight column in edges data
  if (!weight_col %in% names(edges_tbl)) {
    cli::cli_abort("Column {.field {weight_col}} is missing from edge data.")
  }
  edges_tbl$..weight <- edges_tbl[[weight_col]]

  # join nodes id to edges to get module information for both ends of the edges
  edges_tbl <- edges_tbl %>%
    dplyr::left_join(nodes_tbl, by = c(".from" = ".node_id")) %>%
    dplyr::rename(module_from = module) %>%
    dplyr::left_join(nodes_tbl, by = c(".to" = ".node_id")) %>%
    dplyr::rename(module_to = module)

  # long format of edges to compute indicators
  edges_long <- dplyr::bind_rows(
    edges_tbl %>%
      dplyr::transmute(
        node_id = .from,
        module_from = module_from,
        module_to = module_to,
        w = ..weight
      ),
    edges_tbl %>%
      dplyr::transmute(
        node_id = .to,
        module_from = module_to,
        module_to = module_from,
        w = ..weight
      )
  )

  # for each node, compute k_i
  # that is the total number of edges of node i
  k_i <- edges_long %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(k_i = sum(w), .groups = "drop")

  # for each node and each module, compute k_is
  # that is the number of edges from node i to nodes in module s
  k_is <- edges_long %>%
    dplyr::group_by(node_id, module_to) %>%
    dplyr::summarise(k_is = sum(w), .groups = "drop")

  # compute ki_s for the same module (kappa)
  kappa <- edges_long %>%
    dplyr::filter(module_to == module_from) %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(kappa = sum(w), .groups = "drop")

  # join everything to nodes table
  roles_tbl <- nodes_tbl %>%
    dplyr::left_join(k_i, by = c(".node_id" = "node_id")) %>%
    dplyr::left_join(kappa, by = c(".node_id" = "node_id")) %>%
    dplyr::mutate(
      k_i = tidyr::replace_na(k_i, 0),
      kappa = tidyr::replace_na(kappa, 0)
    )

  # compute the participation coefficient for each node
  participation_tbl <- k_is %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      participation = 1 - sum((k_is / sum(k_is))^2),
      .groups = "drop"
    )

  # join participation coefficient to roles table
  roles_tbl <- roles_tbl %>%
    dplyr::left_join(participation_tbl, by = c(".node_id" = "node_id")) %>%
    # if a node has no edges, its participation coefficient is 0
    dplyr::mutate(participation = tidyr::replace_na(participation, 0))

  # within-module degree z-score
  roles_tbl <- roles_tbl %>%
    dplyr::group_by(module) %>%
    dplyr::mutate(
      kappa_mean = mean(kappa, na.rm = TRUE),
      kappa_sd = stats::sd(kappa, na.rm = TRUE),
      within_module_z = dplyr::if_else(
        kappa_sd == 0,
        0,
        (kappa - kappa_mean) / kappa_sd
      )
    ) %>%
    dplyr::ungroup()

  # classify the nodes into 7 roles following Guimera and Amaral (2005)
  roles_tbl <- roles_tbl %>%
    dplyr::mutate(
      role_ga = dplyr::case_when(
        # classify hubs first
        within_module_z >= z_threshold &
          participation < 0.30 ~ "R5 provincial hub",
        within_module_z >= z_threshold &
          participation < 0.75 ~ "R6 connector hub",
        within_module_z >= z_threshold ~ "R7 kinless hub",
        # the rest of the nodes are non-hubs and are classified
        # into 4 roles based on their participation coefficient
        participation < 0.05 ~ "R1 ultra-peripheral",
        participation < 0.62 ~ "R2 peripheral",
        participation < 0.80 ~ "R3 non-hub connector",
        TRUE ~ "R4 non-hub kinless"
      )
    ) %>%
    # select only the columns we need to join back to the graph
    dplyr::select(
      .node_id,
      within_module_degree = kappa,
      within_module_z,
      participation_coeff = participation,
      role_ga
    )

  # final join to the graph
  graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(.node_id = dplyr::row_number()) %>%
    dplyr::left_join(roles_tbl, by = ".node_id") %>%
    dplyr::select(-.node_id)
}
