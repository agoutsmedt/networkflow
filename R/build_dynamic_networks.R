#' Build One or Multiple Networks from Bipartite Links
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `build_dynamic_networks()` builds one or several `tbl_graph` networks from a
#' node table (`source_id`) and a bipartite link table (`source_id` -> `target_id`). `build_network()` is a wrapper for a single network.
#'
#' It supports two backbone extraction methods:
#' - structured filtering using coupling/cooccurrence measures from
#'   [biblionetwork](https://agoutsmedt.github.io/biblionetwork/);
#' - statistical filtering using null models from
#'   [backbone](https://github.com/zpneal/backbone) \insertCite{neal2022}{networkflow}.
#'
#' The function can build a single network or multiple networks across time windows.
#'
#' @param nodes Table of nodes and their metadata. One row per node. For example, a table
#' of articles with identifiers, authors, publication year, etc.
#'
#' @param directed_edges Table of bipartite links between `source_id` nodes and
#' `target_id` entities (e.g., article -> reference, author -> paper).
#'
#' @param source_id Quoted name of the source-side node identifier.
#'
#' @param target_id Quoted name of the target-side identifier linked to each source node.
#'
#' @param time_variable Optional name of the column with a temporal variable (e.g., publication year).
#'
#' @param time_window Optional size of the time window (in units of `time_variable`) to construct temporal networks.
#'
#' @param projection_method method used to extract the network backbone. Choose between:
#' - `"structured"`: uses cooccurrence measures from the [biblionetwork](https://agoutsmedt.github.io/biblionetwork/) package;
#' - `"statistical"`: uses statistical models from the [backbone](https://github.com/zpneal/backbone) package.
#' Defaults to `"structured"`. The `"statistical"` method can be computationally slow on large networks.

#'
#' @param model Statistical null model from [backbone](https://github.com/zpneal/backbone):
#' one of `"sdsm"`, `"fdsm"`, `"fixedfill"`, `"fixedrow"`, `"fixedcol"`.
#' Required if `projection_method = "statistical"`.
#'
#'
#' @param alpha Significance threshold for statistical backbone filtering. Required if
#' `projection_method = "statistical"`. Lower values keep fewer edges.
#'
#' @param cooccurrence_method For `projection_method = "structured"`, choose the coupling method:
#' - `"coupling_angle"`
#' - `"coupling_strength"`;
#' - `"coupling_similarity"`.
#'
#' @param edges_threshold Threshold used to filter weak edges in structured mode.
#'
#' @param overlapping_window Logical. If `TRUE`, builds networks using rolling time windows.
#'
#' @param compute_size Logical. If `TRUE`, computes the number of incoming edges per node (e.g., citation count).
#'
#' @param keep_singleton Logical. If `FALSE`, removes nodes with no edges in the final network.
#'
#' @param filter_components Logical. If `TRUE`, keeps only the main component(s) using `networkflow::filter_components()`.
#'
#' @param ... Additional arguments passed to `filter_components()`.
#'
#' @param backbone_args Optional list of additional arguments passed to the
#' backbone extraction call. If `backbone_args` includes `alpha` or `model`,
#' those values override function arguments.
#'
#' @param verbose Logical. If `TRUE`, displays progress messages.
#'
#' @details
#' The function uses bipartite links (`source_id` -> `target_id`) to produce
#' source-side networks.
#'
#' If `time_variable` and `time_window` are provided, it builds one network per
#' time window (rolling or non-overlapping). Otherwise it builds a single network.
#'
#' `projection_method = "structured"` applies coupling/cooccurrence filtering.
#' `projection_method = "statistical"` applies a statistical backbone model.
#'
#' @examples
#' library(networkflow)
#'
#' nodes <- networkflow::Nodes_stagflation |>
#'   dplyr::filter(source_type == "Stagflation")
#'
#' references <- networkflow::Ref_stagflation
#'
#' # Structured backbone (cooccurrence)
#' net_structured <- build_dynamic_networks(
#' nodes = nodes,
#' directed_edges = references,
#' source_id = "source_id",
#' target_id = "target_id",
#' time_variable = "source_year",
#' time_window = 20,
#' projection_method = "structured",
#' cooccurrence_method = "coupling_similarity",
#' edges_threshold = 1
#' )
#'
#' # Statistical backbone (backbone package)
#' net_statistical <- build_dynamic_networks(
#' nodes = nodes,
#' directed_edges = references,
#' source_id = "source_id",
#' target_id = "target_id",
#' time_variable = "source_year",
#' time_window = 20,
#' projection_method = "statistical",
#' model = "sdsm",
#' alpha = 0.05,
#' backbone_args = list(mtc = "holm")
#' )
#'
#' @return
#' - A single tidygraph object if `time_window` is `NULL`;
#' - A list of tidygraph objects (one per time window) otherwise.
#'
#' @seealso [biblionetwork::biblio_coupling()], [backbone::backbone()]
#'
#' @references
#'   \insertAllCited{}
#'
#' @export
#'

build_dynamic_networks <- function(
  nodes,
  directed_edges,
  source_id,
  target_id,
  time_variable = NULL,
  time_window = NULL,
  projection_method = c("structured", "statistical"),
  model = c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
  alpha = NULL,
  cooccurrence_method = c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  ),
  edges_threshold = 1,
  overlapping_window = FALSE,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...,
  backbone_args = list(),
  verbose = TRUE
) {
  size <- node_size <- N <- method <- NULL

  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)

  # Checking the methods
  projection_methods <- c("structured", "statistical")

  cooccurrence_methods <- c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  )

  statistical_methods <- c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol")

  if (length(projection_method) > 1) {
    projection_method <- match.arg(projection_method, projection_methods)
    if (verbose == TRUE && missing(projection_method)) {
      cli::cli_alert_info(
        "No projection_method provided. Defaulting to {.val {projection_method}}."
      )
    }
  } else {
    projection_method <- match.arg(projection_method, projection_methods)
  }

  # check various setting for the structured/statistical methods
  if (projection_method == "structured") {
    # Checking various problems: lacking method,
    if (length(cooccurrence_method) > 1) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }

    if (!cooccurrence_method %in% cooccurrence_methods) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }
  } else if (projection_method == "statistical") {
    if (is.null(model) || length(model) > 1) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }

    model <- match.arg(model, statistical_methods)

    # check if alpha is given
    if (is.null(alpha) && is.null(backbone_args$alpha)) {
      cli::cli_abort(
        "For statistical backbone extraction, you have to choose a significance level alpha."
      )
    }
  }

  # warning if the source_id is not unique
  if (
    nodes[, .N, source_id, env = list(source_id = source_id)][N > 1, .N] > 0
  ) {
    cli::cli_alert_warning(
      "Some identifiers in your column {.field {source_id}} in your nodes table are not unique. You need only one row per node."
    )
  }

  # check settings for intertemporal networks
  if (!is.null(time_window) & is.null(time_variable)) {
    cli::cli_abort(
      "You cannot have a {.emph time_window} if you don't give any column with a temporal variable. Put a column in {.emph time_variable} or remove the {.emph time_window}."
    )
  }

  # VERBOSE

  if (verbose == TRUE) {
    if (!missing(projection_method)) {
      cli::cli_alert_info(paste(
        "Backbone method selected:",
        projection_method
      ))
    }

    if (keep_singleton == FALSE) {
      cli::cli_alert_info(
        "Keep_singleton == FALSE: removing the nodes that are alone with no edge. \n\n"
      )
    }
  }

  # CHECKING THE DATA

  # NODES
  nodes_coupling <- data.table::copy(nodes)
  nodes_coupling[,
    source_id := as.character(source_id),
    env = list(source_id = source_id)
  ]

  if (is.null(time_variable)) {
    time_variable <- "fake_column"
    nodes_coupling[,
      time_variable := 1,
      env = list(time_variable = time_variable)
    ]
  }

  if (
    !target_id %in% colnames(nodes_coupling) &
      compute_size == TRUE
  ) {
    cli::cli_abort(
      "You don't have the column {.field {target_id}} in your nodes table. Set {.emph compute_size} to {.val FALSE}."
    )
  }

  if (compute_size == TRUE) {
    nodes_coupling[,
      target_id := as.character(target_id),
      env = list(target_id = target_id)
    ]
  }

  # EDGES

  edges <- data.table::copy(directed_edges)
  edges <- data.table::data.table(
    from = as.character(edges[[source_id]]),
    to = as.character(edges[[target_id]])
  ) # canonical edge columns
  edges <- unique(edges)

  ######################### Dynamics networks *********************

  # define the time window
  nodes_coupling <- nodes_coupling[
    order(time_variable),
    env = list(time_variable = time_variable)
  ]
  nodes_coupling[,
    time_variable := as.integer(time_variable),
    env = list(time_variable = time_variable)
  ]

  first_year <- nodes_coupling[,
    min(as.integer(time_variable)),
    env = list(time_variable = time_variable)
  ]
  last_year <- nodes_coupling[,
    max(as.integer(time_variable)),
    env = list(time_variable = time_variable)
  ]

  if (!is.null(time_window)) {
    if (last_year - first_year + 1 < time_window) {
      cli::cli_alert_warning(
        "Your time window is larger than the number of distinct values of {.field {time_variable}}"
      )
    }
  }

  if (is.null(time_window)) {
    all_years <- first_year
    time_window <- last_year - first_year + 1
  } else {
    if (overlapping_window == TRUE) {
      last_year <- last_year - time_window + 1
      all_years <- first_year:last_year
    } else {
      all_years <- seq(first_year, last_year, by = time_window)
      if (all_years[length(all_years)] + (time_window - 1) > last_year) {
        cli::cli_warn(
          "Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is {.val {last_year}}, but the upper limit of your last time window is
                {.val {all_years[length(all_years)] + (time_window - 1)}}."
        )
      }
    }
  }

  # Prepare our list
  tbl_coup_list <- list()

  for (year in all_years) {
    nodes_of_the_year <- nodes_coupling[
      time_variable >= year &
        time_variable < (year + time_window),
      env = list(time_variable = time_variable, year = year)
    ]

    if (time_variable != "fake_column") {
      nodes_of_the_year[,
        time_window := paste0(year, "-", year + time_window - 1),
        env = list(year = year)
      ]

      if (verbose == TRUE) {
        cli::cli_h1(
          "Generation of the network for the {.val {year}}-{.val {year + time_window - 1}} time window."
        )
      }
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }

    node_ids <- nodes_of_the_year[[source_id]]
    edges_of_the_year <- edges[from %in% node_ids]

    # size of nodes
    if (compute_size == TRUE) {
      nb_cit <- edges_of_the_year[from %in% node_ids, .N, by = to]
      data.table::setnames(nb_cit, "to", target_id)

      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"

      if ("node_size" %in% colnames(nodes_coupling) == TRUE) {
        cli::cli_warn(
          "You already have a column name {.field node_size}. The content of the column will be replaced."
        )
      }

      nodes_of_the_year <- data.table::merge.data.table(
        nodes_of_the_year,
        nb_cit,
        by = target_id,
        all.x = TRUE
      )

      nodes_of_the_year[is.na(node_size), node_size := 0]
    }

    # backbone

    if (projection_method == "statistical") {
      # Evaluate the expression and catch internal errors to backbone package
      tryCatch(
        {
          from_pref <- paste0("A:", edges_of_the_year$from)
          to_pref <- paste0("B:", edges_of_the_year$to)
          bip_graph <- igraph::graph_from_data_frame(
            data.frame(from = from_pref, to = to_pref),
            directed = FALSE
          )
          node_ids_pref <- paste0("A:", node_ids)
          igraph::V(bip_graph)$type <- !(igraph::V(bip_graph)$name %in%
            node_ids_pref)

          backbone_graph <- do.call(
            backbone::backbone_from_projection,
            c(list(B = bip_graph, alpha = alpha, model = model), backbone_args)
          )

          if (inherits(backbone_graph, "igraph")) {
            edges_of_the_year <- igraph::as_data_frame(
              backbone_graph,
              what = "edges"
            )
          } else if (!is.null(backbone_graph$backbone)) {
            edges_of_the_year <- igraph::as_data_frame(
              backbone_graph$backbone,
              what = "edges"
            )
          } else {
            stop("The backbone function returned an unexpected object type.")
          }

          edges_of_the_year <- data.table::as.data.table(edges_of_the_year)
          edges_of_the_year[, from := sub("^A:", "", from)]
          edges_of_the_year[, to := sub("^A:", "", to)]
          edges_of_the_year[, from := sub("^B:", "", from)]
          edges_of_the_year[, to := sub("^B:", "", to)]
        },
        error = function(e) {
          stop(
            "The backbone function failed with an error. Read the backbone documentation for more information. Error message: ",
            e$message
          )
        }
      )
    }

    # coupling
    if (projection_method == "structured") {
      biblio_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(biblionetwork::biblio_coupling),
            rlang::expr(biblionetwork::coupling_strength),
            rlang::expr(biblionetwork::coupling_similarity)
          ),
          method = c(
            "coupling_angle",
            "coupling_strength",
            "coupling_similarity"
          )
        )

      biblio_function <- biblio_functions[method == cooccurrence_method][[
        "biblio_function"
      ]][[1]]

      # evaluate the expression and catch internal errors to biblionetwork package

      tryCatch(
        {
          edges_for_biblio <- data.table::copy(edges_of_the_year)
          data.table::setnames(
            edges_for_biblio,
            c("from", "to"),
            c(source_id, target_id)
          )
          edges_of_the_year <-
            rlang::expr((!!biblio_function)(
              dt = edges_for_biblio,
              source = rlang::inject(source_id),
              ref = rlang::inject(target_id),
              weight_threshold = rlang::inject(edges_threshold)
            )) %>%
            eval()
        },
        error = function(e) {
          stop(
            "The coupling function failed with an error. Read the biblionetwork documentation for more information. Error message: ",
            e$message
          )
        }
      )
    }

    # remove nodes with no edges
    if (keep_singleton == FALSE) {
      nodes_of_the_year <- nodes_of_the_year[
        source_id %in%
          edges_of_the_year$from |
          source_id %in% edges_of_the_year$to,
        env = list(source_id = source_id)
      ]
    }

    # make tbl
    if (length(all_years) == 1) {
      tbl_coup_list <- tidygraph::tbl_graph(
        nodes = nodes_of_the_year,
        edges = edges_of_the_year,
        directed = FALSE,
        node_key = source_id
      )
    } else {
      tbl_coup_list[[paste0(year, "-", year + time_window - 1)]] <-
        tidygraph::tbl_graph(
          nodes = nodes_of_the_year,
          edges = edges_of_the_year,
          directed = FALSE,
          node_key = source_id
        )
    }
  }

  if (filter_components == TRUE) {
    tbl_coup_list <- filter_components(tbl_coup_list, ...)
  }
  return(tbl_coup_list)
}

#' Build a single network
#'
#' Convenience wrapper around [build_dynamic_networks()] for a single network.
#'
#' @inheritParams build_dynamic_networks
#' @param projection_method Method used to build the single network. Must be
#' one of `"structured"` or `"statistical"`.
#' @param cooccurrence_method Cooccurrence method used by the structured workflow.
#' @export
build_network <- function(
  nodes,
  directed_edges,
  source_id,
  target_id,
  projection_method,
  cooccurrence_method = c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  ),
  edges_threshold = 1,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...
) {
  if (missing(projection_method)) {
    cli::cli_abort(
      "Please provide {.arg projection_method}: either {.val structured} or {.val statistical}."
    )
  }
  projection_method <- match.arg(
    projection_method,
    c("structured", "statistical")
  )

  graph <- build_dynamic_networks(
    nodes = nodes,
    directed_edges = directed_edges,
    source_id = source_id,
    target_id = target_id,
    projection_method = projection_method,
    cooccurrence_method = cooccurrence_method,
    edges_threshold = edges_threshold,
    compute_size = compute_size,
    keep_singleton = keep_singleton,
    filter_components = FALSE,
    ...,
    verbose = FALSE
  )
  if (filter_components == TRUE) {
    graph <- filter_components(graph, ...)
  }
  graph
}

