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
#' @param cluster_id Character scalar, the node column with the clusters.
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
#' **Reading the roles.** The two measures answer two questions: *how
#' central is this node in its own cluster?* (\eqn{z_i}) and *where do its
#' ties go?* (\eqn{P_i}). What a role means depends on what the nodes and
#' the ties are. For the stagflation data of the package (documents linked by
#' bibliographic coupling, Leiden clusters, resolution 1), the examples are:
#'
#' - *Provincial hub* (R5): the central document of a literature, tied to most
#'   of its members and to almost nothing else. In a coupling network, it
#'   shares references with most documents of its cluster: Lansing's
#'   "Exploring the causes of the great inflation" (2000) in the cluster on the
#'   great inflation, Nordhaus's "Who's afraid of a big bad oil shock?" (2007)
#'   in the cluster on oil. In a co-citation network, the same role is played
#'   by the classics that are cited with most works of a literature.
#' - *Connector hub* (R6): central in its cluster and tied to other clusters:
#'   a document that is embedded in a literature while drawing on others.
#'   Pindyck's "Energy price increases and macroeconomic policy" (1980) is
#'   the only one: central in the cluster on productivity, oil and supply, it
#'   shares references with several other clusters.
#' - *Kinless hub* (R7): central in its cluster, with ties spread evenly over
#'   all clusters. It requires a cluster that is itself not very separate
#'   from the others; there is none in the stagflation example.
#' - *Non-hub connector* (R3): a document whose ties are spread over several
#'   clusters without being central in any: a bridge. Goldstein's "Downward
#'   price inflexibility, ratchet effects, and the inflationary impact of
#'   import price changes" (1977), or Blinder's "The supply-shock explanation
#'   of the great stagflation revisited" (2013), which belongs to the oil
#'   cluster but shares references with the other clusters, are such
#'   bridges. In a co-citation network, they are works used by several
#'   literatures.
#' - *Non-hub kinless* (R4): ties spread almost evenly over all clusters,
#'   which can mark an outlier as well as a bridge. In the example, the
#'   only one, Nitzan's "Regimes of differential accumulation: mergers,
#'   stagflation and the logic of globalization" (2001), shares references
#'   with five clusters in almost equal parts (17 to 20% of its tie weight
#'   each) and is below the average of its own cluster for within-module
#'   degree: it is tied to everybody a little, and to nobody much.
#' - *Peripheral* (R1, R2): all or most ties stay within the cluster. R1
#'   (participation below 0.05) are documents whose ties all stay in their own
#'   cluster, such as Phelps's "Commodity-supply shock and full-employment
#'   monetary policy" (1978), in the cluster on imports.
#'
#' The same classification means other things for other nodes. For authors
#' coupled through their references (see [build_dynamic_networks()]), a
#' connector is an author who cites several literatures, a hub an author who
#' cites like most of the group. For networks that are not bibliographic, the
#' roles describe how much each node holds its community together (hubs) and
#' how much it links communities (connectors).
#'
#' **Cautions.** (1) Roles depend on the clusters, hence on the algorithm and
#' on the resolution: on the stagflation data, the mean participation
#' coefficient is 0.31, 0.39 and 0.52 for Leiden resolutions of 0.75, 1 and
#' 1.5, because smaller clusters leave more ties between clusters. Check that
#' the nodes you interpret keep their role when the resolution changes a
#' little. (2) The thresholds of \insertCite{guimera2005;textual}{networkflow}
#' come from metabolic networks. Projected networks are dense (here, a quarter
#' of the pairs of documents are tied) and most nodes have a participation
#' above 0.05: R2 is the usual role, and hubs are rare. (3) With the weights
#' of similarity measures, long bibliographies and well-cited references
#' influence the weights (see [build_dynamic_networks()]). (4) A role is a
#' position in a network, not a judgement on the importance or the quality
#' of a document; reading the document is necessary.
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
#'   method = "similarity",
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
#' network <- add_node_roles(network, cluster_id = "cluster_leiden")
#' table(tidygraph::as_tibble(network, active = "nodes")$role_ga)
#'
#' @export
add_node_roles <- function(
  graphs,
  cluster_id = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
) {
  if (inherits(graphs, "list")) {
    return(lapply(graphs, function(graph) {
      add_node_roles_one(
        graph,
        cluster_id = cluster_id,
        weight_col = weight_col,
        z_threshold = z_threshold
      )
    }))
  }

  if (inherits(graphs, "tbl_graph")) {
    return(add_node_roles_one(
      graphs,
      cluster_id = cluster_id,
      weight_col = weight_col,
      z_threshold = z_threshold
    ))
  }

  cli::cli_abort(
    "{.arg graphs} must be a {.cls tbl_graph} or a list of {.cls tbl_graph}s, not {.obj_type_friendly {graphs}}."
  )
}

add_node_roles_one <- function(
  graph,
  cluster_id = "dynamic_cluster_leiden",
  weight_col = "weight",
  z_threshold = 2.5
) {
  . <- .node_id <- module <- module_from <- module_to <- node_id <- w <- k_i <- k_is <- kappa <- participation <- NULL
  within_module_z <- within_module_degree <- participation_coeff <- role_ga <- NULL
  kappa_mean <- kappa_sd <- swapped_from <- swapped_to <- NULL

  # Check if the module column exists in the node data
  if (
    !cluster_id %in%
      names(graph %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
  ) {
    cli::cli_abort("Column {.field {cluster_id}} is missing from node data.")
  }

  # use name col as variable in dplyr
  module_sym <- rlang::sym(cluster_id)

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

  # final join to the graph (roles of an earlier run are replaced)
  graph %>%
    tidygraph::activate(nodes) %>%
    dplyr::select(-dplyr::any_of(c("within_module_degree", "within_module_z", "participation_coeff", "role_ga"))) %>%
    dplyr::mutate(.node_id = dplyr::row_number()) %>%
    dplyr::left_join(roles_tbl, by = ".node_id") %>%
    dplyr::select(-.node_id)
}

