#' Characterise the role of nodes within and between clusters
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Two nodes of the same cluster can play very different roles: one may be
#' tied to almost every member of its cluster, another may sit at the border
#' and connect several clusters. `add_node_roles()` computes the two measures
#' of the "functional cartography" of
#' \insertCite{guimera2005;textual}{networkflow} and classifies each node into
#' one of their seven roles.
#'
#' With \eqn{\kappa_i} the weight of the edges of node \eqn{i} towards its own
#' cluster \eqn{s_i}, the **within-module degree** z-score measures how
#' strongly \eqn{i} is connected to its cluster compared with the other
#' members:
#' \deqn{z_i = \frac{\kappa_i - \bar{\kappa}_{s_i}}{\sigma_{\kappa_{s_i}}}.}
#' With \eqn{k_{is}} the weight of the edges of \eqn{i} towards cluster
#' \eqn{s} and \eqn{k_i} its total weight, the **participation coefficient**
#' is close to 0 when all the ties of \eqn{i} stay within one cluster, and
#' close to 1 when they are spread evenly across clusters:
#' \deqn{P_i = 1 - \sum_s \left( \frac{k_{is}}{k_i} \right)^2.}
#'
#' @param graphs A `tbl_graph` or a list of `tbl_graph`s with a cluster
#'   column.
#' @param module_col Character scalar, the node column with the clusters.
#'   The default, `"dynamic_cluster_leiden"`, fits the output of
#'   [merge_dynamic_clusters()]; use `"cluster_leiden"` after
#'   [add_clusters()] alone.
#' @param weight_col Character scalar, the edge column with the weights
#'   (`"weight"` by default). Use a column of ones for an unweighted
#'   analysis.
#' @param z_threshold Number, the within-module z-score above which a node
#'   is a hub (2.5, the value of Guimerà and Amaral).
#'
#' @details
#' The roles combine the two measures, with the thresholds of
#' \insertCite{guimera2005;textual}{networkflow}:
#'
#' | Role | \eqn{z_i} | \eqn{P_i} |
#' |---|---|---|
#' | R1 ultra-peripheral | below `z_threshold` | below 0.05 |
#' | R2 peripheral | below `z_threshold` | 0.05 to 0.62 |
#' | R3 non-hub connector | below `z_threshold` | 0.62 to 0.80 |
#' | R4 non-hub kinless | below `z_threshold` | above 0.80 |
#' | R5 provincial hub | at least `z_threshold` | below 0.30 |
#' | R6 connector hub | at least `z_threshold` | 0.30 to 0.75 |
#' | R7 kinless hub | at least `z_threshold` | above 0.75 |
#'
#' The original measures count edges; here they use the edge weights
#' (strengths), which is the natural generalisation for weighted projected
#' networks. The graph is treated as undirected. The thresholds were
#' calibrated on biological networks: in small bibliometric networks, few
#' nodes pass \eqn{z \geq 2.5}, and the measures are best read as continuous
#' indicators (who are the central members of a literature, which articles
#' bridge two literatures) rather than as categories.
#'
#' @return The same object as `graphs` with four new node columns:
#'   `within_module_degree` (\eqn{\kappa_i}), `within_module_z`
#'   (\eqn{z_i}, 0 when all members of the cluster have the same
#'   \eqn{\kappa}), `participation_coeff` (\eqn{P_i}, 0 for isolated nodes)
#'   and `role_ga` (the role).
#'
#' @references
#'   \insertAllCited{}
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
#' network <- add_node_roles(network, module_col = "cluster_leiden")
#' table(tidygraph::as_tibble(network, active = "nodes")$role_ga)
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
  kappa_mean <- kappa_sd <- swapped_from <- swapped_to <- NULL

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
    # The reversed copy of each edge. transmute() evaluates its arguments in
    # order, so the two modules are swapped through new names, not in place.
    edges_tbl %>%
      dplyr::transmute(
        node_id = .to,
        swapped_from = module_to,
        swapped_to = module_from,
        w = ..weight
      ) %>%
      dplyr::rename(module_from = swapped_from, module_to = swapped_to)
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

