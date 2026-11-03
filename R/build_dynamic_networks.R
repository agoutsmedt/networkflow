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
#' @param time_variable Optional name of a column with the time of each node
#'   (or of each link, with `time_in = "edges"`): a number (e.g. a year), a
#'   `Date`, a `POSIXct`, or an ISO date written as text (`"2001-03"`,
#'   `"2001-03-15"`). Years written as text (`"2001"`) are numbers. Without
#'   it, a single network is built.
#' @param time_in `"nodes"` (default): `time_variable` is a column of `nodes`
#'   and a window selects the nodes of the period. `"edges"`: `time_variable`
#'   is a column of `directed_edges` (e.g. the year of the citing document)
#'   and a window selects the links of the period, the nodes being those with
#'   a link in the window. Use `"edges"` for co-citation, whose period is the
#'   one of the citing documents, and for entities without a date of their
#'   own (authors).
#' @param time_unit For dates only: the unit in which windows are counted,
#'   `"year"` (default), `"quarter"`, `"month"`, `"week"` (starting on
#'   Mondays) or `"day"`. For numbers, the unit is the one of the variable
#'   and `time_unit` must stay `NULL`.
#' @param time_window Optional whole number, the width of each window in
#'   `time_unit`s (or in units of the variable): 10 for ten years, 12 with
#'   `time_unit = "month"` for a year. `NULL` builds a single network.
#' @param window_step Optional whole number, the number of units between the
#'   starts of two windows. `NULL` (default) gives successive windows
#'   (`window_step = time_window`); `1` slides the window one unit at a time
#'   (1975-1984, 1976-1985...); `3` with `time_unit = "month"` and a window
#'   of 12 gives a yearly window every quarter.
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
#' @param projection_on `"source"` (default) links the `source_id` entities
#'   that share `target_id` entities (bibliographic coupling: articles
#'   sharing references). `"target"` links the `target_id` entities that
#'   share `source_id` entities (co-citation: cited documents linked when
#'   they are cited by the same documents). With `"target"`, `nodes` is keyed
#'   by `target_id`.
#' @param document_id Optional column of `directed_edges` identifying the
#'   document through which each link exists. With it, `source_id` entities
#'   are not documents but *entities that publish documents* (authors,
#'   journals, institutions) and a reference cited by several documents of an
#'   entity counts several times, following
#'   [biblionetwork::entity_network()]. Only for `projection_method =
#'   "structured"` with `"coupling_angle"` or `"coupling_strength"`.
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
#' - `"coupling_similarity"`, the idf-weighted cosine of
#'   \insertCite{shen2019;textual}{networkflow}: each source is described by
#'   a vector with one component per target, equal to
#'   \eqn{\sqrt{\log(N / f_j)}}, and the weight is the cosine of the vectors
#'   of two sources, with \eqn{S_i = \sum_{j \in R_i} \log(N / f_j)} the
#'   weighted size of the bibliography:
#'   \deqn{w_{ik} = \frac{\sum_{j \in R_i \cap R_k} \log(N / f_j)}{\sqrt{S_i S_k}}.}
#'   Like the coupling angle, it lies between 0 and 1; like the coupling
#'   strength, it discounts the targets linked to many sources. (Versions
#'   0.1.1 and earlier of biblionetwork computed this measure wrongly: use
#'   biblionetwork 0.1.1.9000 or later.)
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
#' **Time windows.** Time is counted in whole units: the values of a numeric
#' variable, or years, quarters, months, weeks or days for dates. A window
#' starting at unit \eqn{s} and \eqn{w} units wide (`time_window`) gathers
#' the nodes (or links) with \eqn{s \le t < s + w}, and the network is
#' computed as if the corpus were limited to them. A new window starts every
#' `window_step` units: successive windows (the default) give independent
#' snapshots, a step of 1 slides the window and smooths the evolution of the
#' networks, at the price of many networks sharing most of their nodes. The
#' last window may be shorter than the others when the period is not a
#' multiple of the step (a warning is given); windows are labelled by their
#' first and last unit (`"1975-1984"`, `"2001-01 to 2001-12"`). The choice of
#' the window is substantive: long enough for each network to have enough
#' nodes (a few dozen at least) to show a structure, and short enough for
#' changes to be visible. For documents that follow each other quickly
#' (central bank statements, news, posts), months or weeks make sense, with
#' a window wide enough to contain enough documents.
#'
#' **Beyond bibliographic coupling.** Nothing in the function is specific to
#' citations. The two sides of the links can be any two kinds of entities:
#'
#' - *Co-citation* \insertCite{small1973}{networkflow}: with `projection_on =
#'   "target"`, cited documents are linked when the same documents cite them.
#'   Windows should follow the citing documents: put their year in
#'   `directed_edges` and use `time_in = "edges"`.
#' - *Author (or journal, institution) coupling*
#'   \insertCite{zhao2008}{networkflow}: with `document_id`, authors are linked
#'   when the documents they sign cite the same references.
#' - *Co-authorship and other co-occurrences*: authors and the documents they
#'   sign, accounts and the hashtags they use, members and the boards they
#'   sit on: any two-mode table.
#'
#' **Identifiers of references.** Every measure assumes that one work has one
#' identifier. With raw bibliographic data, the same reference is often
#' written in several ways (spelling, abbreviations, missing years), each
#' spelling becomes a different `target_id`, and two documents citing the
#' same work in different ways no longer share it. This matters most for
#' co-citation, where cited works are the nodes: a work split into several
#' spellings is split into several nodes, each with a fraction of its
#' citations. It matters less when the database gives an identifier to each
#' cited reference (e.g. the identifiers of Scopus, OpenAlex or Web of
#' Science), but even then references that the database could not match are
#' missing or duplicated. Clean and match references before building
#' networks, and check the most cited nodes for duplicates.
#'
#' @return A `tbl_graph` (undirected) if `time_variable` is `NULL`;
#'   otherwise a list of `tbl_graph`s named after their window (e.g.
#'   `"1975-1984"`), in chronological order, whose nodes have a
#'   `time_window` column. Edges have a `weight` column for the structured
#'   method.
#'
#' @seealso [build_network()] for a single network;
#'   [biblionetwork::coupling_angle()], [biblionetwork::coupling_strength()],
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
#'   time_window = 13,
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_similarity",
#'   edges_threshold = 1,
#'   verbose = FALSE
#' )
#' names(networks)
#' networks[[1]]
#'
#' # Co-citation: cited documents linked when the same documents cite them,
#' # with windows on the year of the citing documents
#' citations <- merge(
#'   transform(Ref_stagflation[, c("source_id", "target_id")],
#'             source_id = as.character(source_id), target_id = as.character(target_id)),
#'   nodes[, c("source_id", "source_year")],
#'   by = "source_id"
#' )
#' cited <- Ref_stagflation[!duplicated(Ref_stagflation$target_id),
#'                          c("target_id", "target_author", "target_year")]
#' cocitation <- build_dynamic_networks(
#'   nodes = transform(cited, target_id = as.character(target_id)),
#'   directed_edges = citations,
#'   source_id = "source_id",
#'   target_id = "target_id",
#'   projection_on = "target",
#'   time_in = "edges",
#'   time_variable = "source_year",
#'   time_window = 13,
#'   cooccurrence_method = "coupling_angle",
#'   edges_threshold = 2,
#'   compute_size = TRUE,
#'   verbose = FALSE
#' )
#' cocitation[[1]]
#'
#' # Authors linked by the references of the documents they sign
#' by_author <- merge(
#'   transform(Authors_stagflation, source_id = as.character(source_id)),
#'   citations, by = "source_id"
#' )
#' author_coupling <- build_network(
#'   nodes = data.frame(author_name = unique(by_author$author_name)),
#'   directed_edges = by_author,
#'   source_id = "author_name",
#'   target_id = "target_id",
#'   document_id = "source_id",
#'   projection_method = "structured",
#'   cooccurrence_method = "coupling_strength",
#'   edges_threshold = 2
#' )
#' author_coupling
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
  document_id = NULL,
  projection_on = c("source", "target"),
  projection_method = c("structured", "statistical"),
  cooccurrence_method = c(
    "coupling_angle",
    "coupling_strength",
    "coupling_similarity"
  ),
  edges_threshold = 1,
  model = c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol"),
  alpha = NULL,
  backbone_args = list(),
  time_variable = NULL,
  time_in = c("nodes", "edges"),
  time_unit = NULL,
  time_window = NULL,
  window_step = NULL,
  compute_size = FALSE,
  keep_singleton = FALSE,
  filter_components = FALSE,
  ...,
  verbose = TRUE
) {
  size <- node_size <- N <- method <- edge_time <- document <- V1 <- NULL

  # Checks of the arguments. `projection_method` has a default (structured),
  # but the similarity measure and the null model must be chosen explicitly:
  # they change the network, and a silent default would hide that choice.
  projection_missing <- missing(projection_method)
  projection_method <- rlang::arg_match(projection_method)
  projection_on <- rlang::arg_match(projection_on)
  time_in <- rlang::arg_match(time_in)

  # `...` is only for filter_components(): anything else is an error, and the
  # former `overlapping_window` is explained.
  dots <- rlang::list2(...)
  if ("overlapping_window" %in% names(dots)) {
    cli::cli_abort(c(
      "{.arg overlapping_window} no longer exists.",
      "i" = "Use {.arg window_step}: {.code window_step = 1} slides the window by one time unit (the former {.code window_step = 1}); by default, windows follow each other ({.code window_step = time_window})."
    ))
  }
  if (!filter_components && length(dots) > 0) {
    cli::cli_abort(c(
      "Unused argument{?s}: {.arg {names(dots)}}.",
      "i" = "Extra arguments are only passed to {.fn filter_components}, when {.code filter_components = TRUE}."
    ))
  }
  if (!is.null(time_unit)) {
    time_unit <- rlang::arg_match(time_unit, time_units)
  }

  if (!is.null(document_id)) {
    if (projection_method != "structured" || projection_on != "source") {
      cli::cli_abort(c(
        "{.arg document_id} (coupling of entities such as authors) needs {.code projection_method = \"structured\"} and {.code projection_on = \"source\"}.",
        "i" = "Statistical backbones and co-citation do not count the documents of each entity."
      ))
    }
  }

  # Projecting on the targets (e.g. co-citation: cited documents linked when
  # they are cited together) is the same computation with the two sides of
  # the links swapped: from here on, `source_id` is the column of the nodes.
  if (projection_on == "target") {
    swapped <- source_id
    source_id <- target_id
    target_id <- swapped
  }
  check_columns(nodes, source_id, "nodes")
  check_columns(directed_edges, c(source_id, target_id, document_id), "directed_edges")
  if (!is.null(time_variable)) {
    check_columns(if (time_in == "nodes") nodes else directed_edges, time_variable,
                  if (time_in == "nodes") "nodes" else "directed_edges")
  }

  if (projection_method == "structured") {
    cooccurrence_method <- check_explicit_choice(
      cooccurrence_method,
      if (is.null(document_id)) c("coupling_angle", "coupling_strength", "coupling_similarity")
      else c("coupling_angle", "coupling_strength"),
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

  # Time: carried by the nodes (default) or by the links (e.g. the year of
  # the citing document, for co-citation). Without time_variable, a single
  # window gathers everything. Dates (Date, POSIXct, "2001-03-15") are cut in
  # `time_unit`; numbers are in the units of the variable.
  no_time <- is.null(time_variable)
  use_edge_time <- time_in == "edges" && !no_time
  link_size <- use_edge_time || projection_on == "target"
  if (no_time) {
    time_variable <- "fake_column"
    nodes_coupling[, time_variable := 1, env = list(time_variable = time_variable)]
  }
  time_values <- if (use_edge_time) directed_edges[[time_variable]] else nodes_coupling[[time_variable]]
  time_is_date <- !no_time && is_date_variable(time_values)
  if (time_is_date && is.null(time_unit)) {
    time_unit <- "year"
    if (verbose) {
      cli::cli_inform(c("i" = "{.field {time_variable}} contains dates: windows are counted in years. Set {.arg time_unit} for finer units ({.val {time_units}})."))
    }
  }
  if (!time_is_date && !is.null(time_unit)) {
    cli::cli_abort(c(
      "{.arg time_unit} only applies to dates.",
      "i" = "{.field {time_variable}} is not a date: its values are used as they are ({.code time_window} is in units of the variable)."
    ))
  }
  unit <- if (time_is_date) time_unit else NULL

  if (compute_size && !link_size && !target_id %in% colnames(nodes_coupling)) {
    cli::cli_abort(c(
      "{.arg nodes} has no column {.field {target_id}}.",
      "i" = "{.code compute_size = TRUE} counts the links received by each node through this column: add it, or set {.code compute_size = FALSE}."
    ))
  }

  if (compute_size && "node_size" %in% colnames(nodes_coupling)) {
    cli::cli_warn("{.arg nodes} already has a {.field node_size} column: it is replaced.")
    nodes_coupling[, node_size := NULL]
  }

  if (compute_size && !link_size) {
    nodes_coupling[, target_id := as.character(target_id), env = list(target_id = target_id)]
  }

  # EDGES
  edges <- data.table::data.table(
    from = as.character(directed_edges[[source_id]]),
    to = as.character(directed_edges[[target_id]])
  ) # canonical edge columns
  if (!is.null(document_id)) {
    edges[, document := as.character(directed_edges[[document_id]])]
  }
  if (use_edge_time) {
    edges[, edge_time := time_index(directed_edges[[time_variable]], unit)]
  }
  edges <- unique(edges)

  ######################### Dynamics networks *********************

  if (use_edge_time) {
    n_missing <- sum(is.na(edges$edge_time))
    if (n_missing > 0) {
      cli::cli_warn("{n_missing} link{?s} {?has/have} no valid {.field {time_variable}} in {.arg directed_edges}: {cli::qty(n_missing)}{?it is/they are} dropped.")
      edges <- edges[!is.na(edge_time)]
    }
    n_unknown <- sum(!edges$from %in% nodes_coupling[[source_id]])
    if (n_unknown > 0) {
      cli::cli_warn("{n_unknown} link{?s} {?has/have} a node absent from {.arg nodes}: {cli::qty(n_unknown)}{?it is/they are} dropped.")
      edges <- edges[edges$from %in% nodes_coupling[[source_id]]]
    }
    first_time <- min(edges$edge_time)
    last_time <- max(edges$edge_time)
  } else {
    node_time <- time_index(nodes_coupling[[time_variable]], unit)
    n_missing <- sum(is.na(node_time))
    if (n_missing > 0) {
      cli::cli_warn("{n_missing} node{?s} {?has/have} no valid {.field {time_variable}} in {.arg nodes}: {cli::qty(n_missing)}{?it is/they are} dropped.")
      nodes_coupling <- nodes_coupling[!is.na(node_time)]
      node_time <- node_time[!is.na(node_time)]
    }
    first_time <- min(node_time)
    last_time <- max(node_time)
  }

  # Windows: `time_window` units wide, one every `window_step` units.
  if (is.null(time_window)) {
    starts <- first_time
    window <- last_time - first_time + 1L
  } else {
    window <- check_positive_integer(time_window, "time_window")
    step <- if (is.null(window_step)) window else check_positive_integer(window_step, "window_step")
    if (last_time - first_time + 1 < window) {
      cli::cli_warn(
        "{.arg time_window} ({window}) is longer than the period covered by {.field {time_variable}} ({index_label(first_time, unit)} to {index_label(last_time, unit)})."
      )
    }
    starts <- window_starts(first_time, last_time, window, step)
    if (utils::tail(starts, 1) + window - 1 > last_time) {
      cli::cli_warn(c(
        "The last window is shorter than the others.",
        "i" = "The data end in {.val {index_label(last_time, unit)}}, but the last window ends in {.val {index_label(utils::tail(starts, 1) + window - 1, unit)}}."
      ))
    }
  }

  # Prepare our list
  tbl_coup_list <- list()
  show_progress <- verbose && length(starts) > 1
  if (show_progress) {
    cli::cli_progress_bar("Building the networks", total = length(starts))
  }

  for (start in starts) {
    label <- window_label(start, window, unit)
    if (use_edge_time) {
      edges_window <- edges[edge_time >= start & edge_time < (start + window)]
      in_window <- nodes_coupling[[source_id]] %in% edges_window$from
      nodes_of_the_year <- nodes_coupling[in_window]
      data.table::set(nodes_of_the_year, j = "time_window", value = rep(label, nrow(nodes_of_the_year)))
      edges_of_the_year <- unique(edges_window[, !"edge_time"])
    } else {
      in_window <- node_time >= start & node_time < (start + window)
      nodes_of_the_year <- nodes_coupling[in_window]
      if (!no_time) {
        data.table::set(nodes_of_the_year, j = "time_window", value = rep(label, nrow(nodes_of_the_year)))
      } else {
        nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
      }
      edges_of_the_year <- edges[from %in% nodes_of_the_year[[source_id]]]
    }
    node_ids <- nodes_of_the_year[[source_id]]

    # size of nodes: the links received through `target_id` (nodes of a
    # coupling network that also appear as targets), or, when nodes are
    # linked through their own links (co-citation, time carried by the links),
    # the number of distinct targets of each node (e.g. citing documents).
    if (compute_size && link_size) {
      n_links <- edges_of_the_year[, list(node_size = data.table::uniqueN(to)), by = from]
      data.table::setnames(n_links, "from", source_id)
      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year, n_links, by = source_id, all.x = TRUE)
      nodes_of_the_year[is.na(node_size), node_size := 0]
    } else if (compute_size) {
      nb_cit <- edges_of_the_year[from %in% node_ids, .N, by = to]
      data.table::setnames(nb_cit, "to", target_id)
      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"
      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year, nb_cit, by = target_id, all.x = TRUE)
      nodes_of_the_year[is.na(node_size), node_size := 0]
    }

    # A window where no target is shared by two sources has no edge (and
    # biblionetwork fails on it): build an empty edge table instead.
    no_shared_target <- edges_of_the_year[, data.table::uniqueN(from), by = to][V1 > 1, .N] == 0

    # backbone

    if (projection_method == "statistical" && no_shared_target) {
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
            "The statistical backbone failed for the window {.val {label}}.",
            parent = e
          )
        }
      )
    }

    # coupling
    if (projection_method == "structured" && no_shared_target) {
      edges_of_the_year <- data.table::data.table(
        from = character(0), to = character(0), weight = numeric(0)
      )
    } else if (projection_method == "structured") {
      biblio_function <- switch(
        cooccurrence_method,
        coupling_angle = biblionetwork::coupling_angle,
        coupling_strength = biblionetwork::coupling_strength,
        coupling_similarity = biblionetwork::coupling_similarity
      )

      # evaluate the expression and catch internal errors to biblionetwork package

      tryCatch(
        {
          edges_for_biblio <- data.table::copy(edges_of_the_year)
          if (is.null(document_id)) {
            data.table::setnames(edges_for_biblio, c("from", "to"), c(source_id, target_id))
            edges_of_the_year <- biblio_function(
              dt = edges_for_biblio,
              source = source_id,
              ref = target_id,
              weight_threshold = edges_threshold
            )
          } else {
            # Entities (e.g. authors) cite through their documents: a
            # reference cited by several documents of an entity counts more.
            data.table::setnames(edges_for_biblio, c("from", "to", "document"),
                                 c("entity", "reference", "citing_document"))
            edges_of_the_year <- biblionetwork::entity_network(
              dt = edges_for_biblio,
              source = "citing_document",
              ref = "reference",
              entity = "entity",
              weight_threshold = edges_threshold,
              output_in_character = TRUE,
              method = sub("^coupling_", "", cooccurrence_method)
            )
          }
        },
        error = function(e) {
          cli::cli_abort(
            "The computation of {.val {cooccurrence_method}} failed for the window {.val {label}}.",
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

    if (nrow(nodes_of_the_year) == 0 && !keep_singleton) {
      cli::cli_warn("The network of the window {.val {label}} is empty: no pair of nodes has {edges_threshold} shared link{?s} or more.")
    }

    # make tbl
    if (length(starts) == 1) {
      tbl_coup_list <- tidygraph::tbl_graph(
        nodes = nodes_of_the_year,
        edges = edges_of_the_year,
        directed = FALSE,
        node_key = source_id
      )
    } else {
      tbl_coup_list[[label]] <-
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
  document_id = NULL,
  projection_on = c("source", "target"),
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
    document_id = document_id,
    projection_on = projection_on,
    projection_method = projection_method,
    cooccurrence_method = cooccurrence_method,
    edges_threshold = edges_threshold,
    model = model,
    alpha = alpha,
    backbone_args = backbone_args,
    compute_size = compute_size,
    keep_singleton = keep_singleton,
    filter_components = FALSE,
    verbose = FALSE
  )
  if (filter_components) {
    graph <- filter_components(graph, ...)
  } else if (...length() > 0) {
    cli::cli_abort(c(
      "Unused argument{?s}: {.arg {names(rlang::list2(...))}}.",
      "i" = "Extra arguments are only passed to {.fn filter_components}, when {.code filter_components = TRUE}."
    ))
  }
  graph
}
