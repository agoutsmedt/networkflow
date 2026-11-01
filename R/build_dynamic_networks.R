#' Build one network, or one network per time window, from bipartite links
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The first step of the networkflow pipeline. Research data rarely come as a
#' network: they come as a table of links between two kinds of entities, such
#' as articles and the references they cite, authors and the papers they sign,
#' or accounts and the hashtags they use. `build_dynamic_networks()` *projects*
#' this two-mode (bipartite) relation onto the first kind of entity: two
#' `source_id` entities (e.g. two articles) are linked when they share
#' `target_id` entities (e.g. references). With `time_variable` and
#' `time_window`, it builds one such network per time window, which is the
#' starting point of a dynamic analysis.
#'
#' Write \eqn{a_{ij} = 1} when source \eqn{i} is linked to target \eqn{j}. The
#' raw strength of the tie between sources \eqn{i} and \eqn{k} is the number
#' of targets they share,
#' \deqn{c_{ik} = \sum_j a_{ij} a_{kj} = |R_i \cap R_k|,}
#' with \eqn{R_i} the set of targets of \eqn{i}. When sources are citing
#' documents and targets their references, this is bibliographic coupling
#' \insertCite{kessler1963}{networkflow}. Swapping the roles (cited documents
#' as `source_id`, citing documents as `target_id`) gives a co-citation
#' network \insertCite{small1973}{networkflow}.
#'
#' Raw counts favour sources with many targets (articles with long
#' bibliographies), and most pairs share a target or two by chance. The two
#' `projection_method`s answer this problem in different ways:
#'
#' - `"structured"` normalises the counts with a bibliometric similarity
#'   measure computed by [biblionetwork](https://agoutsmedt.github.io/biblionetwork/)
#'   and keeps the pairs sharing at least `edges_threshold` targets;
#' - `"statistical"` keeps only the ties that are stronger than expected
#'   under a null model of random co-occurrence, using the
#'   [backbone](https://github.com/zpneal/backbone) package
#'   \insertCite{neal2014,neal2022}{networkflow}.
#'
#' @param nodes A data frame with one row per source entity (e.g. one row per
#'   article) and its metadata (authors, year, title...). Not modified.
#' @param directed_edges A data frame of bipartite links, one row per
#'   `source_id` -> `target_id` link (e.g. article -> cited reference).
#'   Duplicated links are counted once. Not modified.
#' @param source_id Character scalar, the column identifying the entities that
#'   become the nodes of the network. It must exist in both `nodes` and
#'   `directed_edges`, with one row per value in `nodes`.
#' @param target_id Character scalar, the column of `directed_edges` with the
#'   entities that create the links (e.g. references).
#' @param time_variable Optional character scalar, a column of `nodes` with an
#'   integer time unit (e.g. the publication year). `NULL` builds a single
#'   network.
#' @param time_window Optional integer, the width of each window, in units of
#'   `time_variable`.
#' @param projection_method `"structured"` (default) or `"statistical"`, see
#'   the description and the details.
#' @param model For `projection_method = "statistical"`, the null model of
#'   [backbone::backbone_from_projection()]: `"sdsm"`, `"fdsm"`,
#'   `"fixedfill"`, `"fixedrow"` or `"fixedcol"`.
#' @param alpha For `projection_method = "statistical"`, the significance
#'   level of the test of each tie. Lower values keep fewer edges.
#' @param cooccurrence_method For `projection_method = "structured"`, the
#'   similarity measure used as edge weight: `"coupling_angle"`,
#'   `"coupling_strength"` or `"coupling_similarity"` (see the details).
#'   There is no default: the choice must be explicit.
#' @param edges_threshold For `projection_method = "structured"`, the minimum
#'   number of shared targets for two sources to be linked (1 by default).
#' @param overlapping_window Logical. `FALSE` (default) cuts the period into
#'   successive windows (1975-1984, 1985-1994...); `TRUE` slides the window
#'   by one time unit (1975-1984, 1976-1985...).
#' @param compute_size Logical. If `TRUE`, adds a `node_size` column: the
#'   number of times each node is a target of the sources of the same window
#'   (for a coupling network, the number of citations received within the
#'   corpus). It requires `nodes` to contain the `target_id` column.
#' @param keep_singleton Logical. If `FALSE` (default), nodes without any
#'   edge are removed.
#' @param filter_components Logical. If `TRUE`, keeps only the main
#'   component(s) with [filter_components()].
#' @param ... Arguments passed to [filter_components()] (e.g.
#'   `nb_components`).
#' @param backbone_args Optional list of further arguments for
#'   [backbone::backbone_from_projection()] (e.g. `mtc = "holm"` to correct
#'   for multiple tests). `alpha` or `model` given here override the
#'   arguments of the function.
#' @param verbose Logical. If `TRUE` (default), reports the steps.
#'
#' @details
#' **Similarity measures** (`projection_method = "structured"`). With
#' \eqn{L_i = |R_i|} the number of targets of source \eqn{i}, \eqn{N} the
#' number of sources and \eqn{f_j} the number of sources linked to target
#' \eqn{j}, the three measures computed by biblionetwork are:
#'
#' - `"coupling_angle"`, the cosine of \insertCite{sen1983;textual}{networkflow}:
#'   \deqn{w_{ik} = \frac{|R_i \cap R_k|}{\sqrt{L_i L_k}};}
#' - `"coupling_strength"` \insertCite{vladutz1984,shen2019}{networkflow},
#'   where a shared target counts less when it is linked to many sources,
#'   in the spirit of an inverse document frequency:
#'   \deqn{w_{ik} = \frac{\sum_{j \in R_i \cap R_k} \log(N / f_j)}{L_i L_k};}
#' - `"coupling_similarity"`, inspired by \insertCite{shen2019;textual}{networkflow},
#'   which normalises the same weighted sum by the weighted size of each
#'   bibliography, \eqn{S_i = \sum_{j \in R_i} \log(N / f_j)}:
#'   \deqn{w_{ik} = \frac{\sum_{j \in R_i \cap R_k} \log(N / f_j)}{S_i S_k}.}
#'   Warning: in biblionetwork 0.1.x, \eqn{S_i} is not computed as intended
#'   (all references get the weight of the first one, so that
#'   \eqn{S_i \propto L_i}), which makes `"coupling_similarity"`
#'   proportional to `"coupling_strength"`. Until this is fixed in
#'   biblionetwork, prefer `"coupling_strength"` or `"coupling_angle"`.
#'
#' Targets linked to a single source cannot create a tie and are dropped
#' before the computation. The threshold applies to the raw count
#' \eqn{|R_i \cap R_k|}, not to the weight: `edges_threshold = 2` keeps the
#' pairs of articles that share at least two references, a common choice to
#' remove ties that one shared classic is enough to create. The measures
#' differ in how they treat very cited targets and long target lists; in
#' practice, clusters are often robust to the choice, but it is worth
#' checking on one's own data.
#'
#' **Statistical backbones** (`projection_method = "statistical"`). Instead
#' of fixing a threshold, the backbone approach asks, for each pair, whether
#' the observed number of shared targets is larger than in random bipartite
#' networks that preserve some properties of the observed one: the degrees of
#' sources and targets on average (`"sdsm"`, the stochastic degree sequence
#' model) or exactly (`"fdsm"`), or only the total number of links
#' (`"fixedfill"`) \insertCite{neal2014,domagalski2021}{networkflow}. Edges
#' are kept when the test is significant at level `alpha`; the resulting
#' network is unweighted. This is slower, but each edge can be read as a tie
#' unlikely to be due to chance.
#'
#' **Time windows.** A window starting in year \eqn{s} gathers the nodes with
#' \eqn{s \le t < s + w}, \eqn{w} being `time_window`, and only the links of
#' these nodes: each network is computed as if the corpus were limited to the
#' window. Sliding windows (`overlapping_window = TRUE`) smooth the evolution
#' of the networks, at the price of many networks sharing most of their
#' nodes; successive windows give independent snapshots. When the period is
#' not a multiple of `time_window`, the last successive window is shorter and
#' a warning is given. The choice of the window is substantive: it should be
#' long enough for each network to have enough nodes (a few dozen at least)
#' to show a structure, and short enough for changes to be visible.
#'
#' @return A `tbl_graph` (undirected) if `time_variable` is `NULL`;
#'   otherwise a list of `tbl_graph`s named after their window (e.g.
#'   `"1975-1984"`), in chronological order, whose nodes have a
#'   `time_window` column. Edges have a `weight` column for the structured
#'   method.
#'
#' @seealso [build_network()] for a single network;
#'   [biblionetwork::biblio_coupling()], [biblionetwork::coupling_strength()],
#'   [biblionetwork::coupling_similarity()], [backbone::backbone_from_projection()].
#'   The next steps of the pipeline are [add_clusters()] and
#'   [merge_dynamic_clusters()].
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
#'
#' # Bibliographic coupling networks of the articles explaining the 1970s
#' # stagflation, one per decade
#' networks <- build_dynamic_networks(
#'   nodes = nodes,
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 10,
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1,
#'   verbose = FALSE
#' )
#' names(networks)
#' networks[[1]]
#'
#' \donttest{
#' # Statistical backbone: keep the ties that random networks with the same
#' # degrees would rarely produce
#' if (requireNamespace("backbone", quietly = TRUE)) {
#' backbones <- build_dynamic_networks(
#'   nodes = nodes,
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   time_variable = "source_year",
#'   time_window = 20,
#'   projection_method = "statistical",
#'   model = "sdsm",
#'   alpha = 0.05,
#'   backbone_args = list(mtc = "holm"),
#'   verbose = FALSE
#' )
#' }
#' }
#'
#' @export
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

  # Checks of the arguments. `projection_method` has a default (structured),
  # but the similarity measure and the null model must be chosen explicitly:
  # they change the network, and a silent default would hide that choice.
  projection_missing <- missing(projection_method)
  projection_method <- rlang::arg_match(projection_method)
  check_columns(nodes, source_id, "nodes")
  check_columns(directed_edges, c(source_id, target_id), "directed_edges")

  if (projection_method == "structured") {
    cooccurrence_method <- check_explicit_choice(
      cooccurrence_method,
      c("coupling_angle", "coupling_strength", "coupling_similarity"),
      "cooccurrence_method",
      context = "With {.code projection_method = \"structured\"}"
    )
  } else {
    rlang::check_installed("backbone", reason = "for statistical backbones.")
    model <- check_explicit_choice(
      model,
      c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
      "model",
      context = "With {.code projection_method = \"statistical\"}"
    )
    if (is.null(alpha) && is.null(backbone_args$alpha)) {
      cli::cli_abort(c(
        "{.arg alpha} is missing.",
        "i" = "With {.code projection_method = \"statistical\"}, give the significance level of the test of each edge (e.g. {.code alpha = 0.05})."
      ))
    }
  }

  if (!is.null(time_window) && is.null(time_variable)) {
    cli::cli_abort(c(
      "{.arg time_window} needs {.arg time_variable}.",
      "i" = "Give the column with the time unit in {.arg time_variable}, or remove {.arg time_window}."
    ))
  }

  # data.table() copies: the user's tables are never modified by reference.
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)

  n_duplicated <- nodes[, .N, by = source_id, env = list(source_id = source_id)][N > 1, .N]
  if (n_duplicated > 0) {
    cli::cli_warn(c(
      "{n_duplicated} identifier{?s} of {.field {source_id}} appear{?s/} more than once in {.arg nodes}.",
      "i" = "{.arg nodes} must have one row per node."
    ))
  }

  if (verbose) {
    if (projection_missing) {
      cli::cli_inform(c("i" = "No {.arg projection_method} given: using {.val {projection_method}}."))
    }
    if (!keep_singleton) {
      cli::cli_inform(c("i" = "Nodes without any edge are removed ({.code keep_singleton = FALSE})."))
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

  if (compute_size && "node_size" %in% colnames(nodes_coupling)) {
    cli::cli_warn("{.arg nodes} already has a {.field node_size} column: it is replaced.")
    nodes_coupling[, node_size := NULL]
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
      cli::cli_warn(
        "{.arg time_window} ({time_window}) is longer than the period covered by {.field {time_variable}} ({first_year}-{last_year})."
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
        cli::cli_warn(c(
          "The last window is shorter than the others.",
          "i" = "The data end in {.val {last_year}}, but the last window ends in {.val {all_years[length(all_years)] + (time_window - 1)}}."
        ))
      }
    }
  }

  # Prepare our list
  tbl_coup_list <- list()
  show_progress <- verbose && length(all_years) > 1
  if (show_progress) {
    cli::cli_progress_bar("Building the networks", total = length(all_years))
  }

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

      nodes_of_the_year <- data.table::merge.data.table(
        nodes_of_the_year,
        nb_cit,
        by = target_id,
        all.x = TRUE
      )

      nodes_of_the_year[is.na(node_size), node_size := 0]
    }

    # backbone

    if (projection_method == "statistical" && !any(duplicated(edges_of_the_year$to))) {
      edges_of_the_year <- data.table::data.table(from = character(0), to = character(0))
    } else if (projection_method == "statistical") {
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
            cli::cli_abort("{.fn backbone::backbone_from_projection} returned an unexpected object.")
          }

          edges_of_the_year <- data.table::as.data.table(edges_of_the_year)
          edges_of_the_year[, from := sub("^A:", "", from)]
          edges_of_the_year[, to := sub("^A:", "", to)]
          edges_of_the_year[, from := sub("^B:", "", from)]
          edges_of_the_year[, to := sub("^B:", "", to)]
        },
        error = function(e) {
          cli::cli_abort(
            "The statistical backbone failed for the window starting in {.val {year}}.",
            parent = e
          )
        }
      )
    }

    # A window where no target is shared by two sources has no edge (and
    # biblionetwork fails on it): build an empty edge table instead.
    no_shared_target <- !any(duplicated(edges_of_the_year$to))

    # coupling
    if (projection_method == "structured" && no_shared_target) {
      edges_of_the_year <- data.table::data.table(
        from = character(0), to = character(0), weight = numeric(0)
      )
    } else if (projection_method == "structured") {
      biblio_function <- switch(
        cooccurrence_method,
        coupling_angle = biblionetwork::biblio_coupling,
        coupling_strength = biblionetwork::coupling_strength,
        coupling_similarity = biblionetwork::coupling_similarity
      )

      # evaluate the expression and catch internal errors to biblionetwork package

      tryCatch(
        {
          edges_for_biblio <- data.table::copy(edges_of_the_year)
          data.table::setnames(
            edges_for_biblio,
            c("from", "to"),
            c(source_id, target_id)
          )
          edges_of_the_year <- biblio_function(
            dt = edges_for_biblio,
            source = source_id,
            ref = target_id,
            weight_threshold = edges_threshold
          )
        },
        error = function(e) {
          cli::cli_abort(
            "The computation of {.val {cooccurrence_method}} failed for the window starting in {.val {year}}.",
            parent = e
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
    if (show_progress) cli::cli_progress_update()
  }

  if (filter_components == TRUE) {
    tbl_coup_list <- filter_components(tbl_coup_list, ...)
  }
  return(tbl_coup_list)
}

#' Build a single network from bipartite links
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `build_network()` projects a two-mode relation (e.g. articles and the
#' references they cite) onto a one-mode network of the source entities, for
#' the whole corpus at once. It is [build_dynamic_networks()] without time
#' windows: see that function for the measures, the thresholds and the
#' literature behind them.
#'
#' @inheritParams build_dynamic_networks
#' @param projection_method `"structured"` or `"statistical"`. Required.
#' @param cooccurrence_method For `projection_method = "structured"`,
#'   `"coupling_angle"`, `"coupling_strength"` or `"coupling_similarity"`.
#' @param ... With `filter_components = TRUE`, arguments of
#'   [filter_components()] (e.g. `nb_components`).
#'
#' @details
#' `build_network()` runs silently (`verbose = FALSE`).
#'
#' @return A `tbl_graph`, undirected, with the columns of `nodes` as node
#'   attributes and, for the structured method, a `weight` edge column.
#'
#' @seealso [build_dynamic_networks()]; [filter_components()],
#'   [add_clusters()] for the next steps.
#'
#' @examples
#' nodes <- subset(Nodes_stagflation, source_type == "Stagflation")
#'
#' network <- build_network(
#'   nodes = nodes,
#'   directed_edges = Ref_stagflation,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 2
#' )
#' network
#'
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
  model = c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
  alpha = NULL,
  backbone_args = list(),
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...
) {
  if (missing(projection_method)) {
    cli::cli_abort(c(
      "{.arg projection_method} is missing.",
      "i" = "Choose {.val structured} or {.val statistical}."
    ))
  }
  projection_method <- rlang::arg_match(projection_method, c("structured", "statistical"))

  # `...` is reserved for filter_components(): the arguments of the projection
  # are explicit, so that they cannot be sent to the wrong function.
  graph <- build_dynamic_networks(
    nodes = nodes,
    directed_edges = directed_edges,
    source_id = source_id,
    target_id = target_id,
    projection_method = projection_method,
    model = model,
    alpha = alpha,
    cooccurrence_method = cooccurrence_method,
    edges_threshold = edges_threshold,
    compute_size = compute_size,
    keep_singleton = keep_singleton,
    filter_components = FALSE,
    backbone_args = backbone_args,
    verbose = FALSE
  )
  if (filter_components) {
    graph <- filter_components(graph, ...)
  }
  graph
}
