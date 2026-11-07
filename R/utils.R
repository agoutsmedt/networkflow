# Internal helpers shared by the exported functions: argument checks (all
# through cli_abort(), so that errors name the argument and explain the fix)
# and the "one graph or a list of graphs" dispatch used by most steps.

# Check that `data` has the columns `cols`; `arg` names the argument in the
# error message.
check_columns <- function(data, cols, arg, call = rlang::caller_env()) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg {arg}} has no column{?s} {.field {missing_cols}}.",
      call = call
    )
  }
  invisible(data)
}

# The `method` of build_network() and build_dynamic_networks() is one choice
# among two families: the similarity measures of biblionetwork
# (biblio_coupling(method = ), entity_network(method = )), which weight the
# edges, and the null models of the backbone package, which keep the
# significant edges only.
structured_methods <- c("angle", "strength", "similarity", "jaccard", "vladutz")
backbone_methods <- c("sdsm", "fdsm", "fixedfill", "fixedrow", "fixedcol")

# What each method is, and where it comes from (used in the messages).
method_info <- list(
  angle = list(what = "the coupling angle, the cosine of the two lists of targets",
               origin = "Sen and Gan (1983)", package = "biblionetwork"),
  jaccard = list(what = "the relative coupling strength, a Jaccard index (shared targets over the union of the two lists)",
                 origin = "Sen and Gan (1983)", package = "biblionetwork"),
  strength = list(what = "the coupling strength, which counts a shared target less when many sources are linked to it",
                  origin = "Shen et al. (2019)", package = "biblionetwork"),
  similarity = list(what = "the idf-weighted cosine of the two lists of targets",
                    origin = "Shen et al. (2019)", package = "biblionetwork"),
  vladutz = list(what = "the Vladutz-Cook formula, with a steep discount of the targets linked to many sources",
                 origin = "Vladutz and Cook (1984), as reported by Shen et al. (2019)", package = "biblionetwork"),
  sdsm = list(what = "the stochastic degree sequence model: random networks keep, on average, the degrees of sources and targets",
              origin = "Neal (2014), Neal (2022)", package = "backbone"),
  fdsm = list(what = "the fixed degree sequence model: random networks keep exactly the degrees of sources and targets",
              origin = "Neal (2022), Domagalski et al. (2021)", package = "backbone"),
  fixedfill = list(what = "random networks keep the total number of links",
                   origin = "Neal (2022)", package = "backbone"),
  fixedrow = list(what = "random networks keep the number of targets of each source",
                  origin = "Neal (2022)", package = "backbone"),
  fixedcol = list(what = "random networks keep the number of sources of each target",
                  origin = "Neal (2022)", package = "backbone")
)

# Check the `method` argument: required (no default, to force an explicit
# choice), one of the two families. `call` is the function the user called.
check_method <- function(method, call = rlang::caller_env()) {
  choices <- c(structured_methods, backbone_methods)
  if (missing(method) || is.null(method) || length(method) != 1 || !method %in% choices) {
    given <- if (!missing(method) && !is.null(method)) paste0(" ({.val {method}} is not a method)") else ""
    cli::cli_abort(
      c(
        paste0("Choose one value for {.arg method}", given, "."),
        "i" = "Similarity measures of {.pkg biblionetwork}, which weight the edges: {.val {structured_methods}}.",
        "i" = "Statistical backbones of {.pkg backbone}, which keep the significant edges (with {.arg alpha}): {.val {backbone_methods}}.",
        "i" = "See the details of {.fn build_dynamic_networks} for the origin of each method."
      ),
      call = call
    )
  }
  method
}

# The former arguments (projection_method, cooccurrence_method, model) were
# replaced by `method` in 0.0.9.9001: say how to translate the call.
check_former_arguments <- function(argument_names, call = rlang::caller_env()) {
  former <- intersect(argument_names, c("projection_method", "cooccurrence_method", "model"))
  if (length(former) == 0) {
    return(invisible())
  }
  cli::cli_abort(
    c(
      "{.arg {former}} no longer {?exists/exist}: one argument, {.arg method}, replaces {.arg projection_method}, {.arg cooccurrence_method} and {.arg model}.",
      "i" = "{.code projection_method = \"structured\", cooccurrence_method = \"coupling_angle\"} becomes {.code method = \"angle\"} (also {.val {structured_methods}}).",
      "i" = "{.code projection_method = \"statistical\", model = \"sdsm\"} becomes {.code method = \"sdsm\"} (also {.val {backbone_methods}}), with {.arg alpha} as before."
    ),
    call = call
  )
}

# Tell the user what the chosen method does and where it comes from.
inform_method <- function(method, source_id, target_id, edges_threshold, alpha,
                          backbone_args, document_id, projection_on) {
  info <- method_info[[method]]
  if (method %in% structured_methods) {
    what <- if (!is.null(document_id)) {
      "Coupling of entities through their documents ({.fn biblionetwork::entity_network})"
    } else if (projection_on == "target") {
      "Projection on the targets, e.g. co-citation or co-authorship ({.fn biblionetwork::biblio_coupling} on the swapped links)"
    } else {
      "Coupling ({.fn biblionetwork::biblio_coupling})"
    }
    cli::cli_inform(c(
      "i" = paste0(what, " with {.val {method}}: ties weighted by ", info$what,
                   ", after {info$origin}, between {.field {source_id}} entities sharing {.field {target_id}} entities."),
      "i" = "Two {.field {source_id}} are linked when they share at least {edges_threshold} {.field {target_id}} ({.arg edges_threshold})."
    ))
  } else {
    level <- if (!is.null(backbone_args$alpha)) backbone_args$alpha else alpha
    cli::cli_inform(c(
      "i" = paste0("Statistical backbone ({.pkg backbone}) with {.val {method}}: ", info$what,
                   ", after {info$origin}."),
      "i" = "A tie between two {.field {source_id}} is kept when the number of shared {.field {target_id}} is larger than in the random networks at the level {.arg alpha} = {level}: the edges are unweighted."
    ))
  }
  if (method == "fdsm" && is.null(backbone_args$trials)) {
    cli::cli_inform(c("!" = "{.val fdsm} approximates the distribution of each tie by simulating random networks: with the default number of trials of {.pkg backbone}, it can take hours even for a few hundred nodes. Set it with {.code backbone_args = list(trials = 1000)}, or use {.val sdsm}."))
  }
  invisible()
}

# Marker passed by build_network() as `time_variable` to build_dynamic_networks():
# one network, no time. (A user who gives NULL gets an error, see there.)
no_time_variable <- structure("no time", class = "networkflow_no_time")

# Columns of `data` that look like a time variable (dates, or numbers named
# like a year), to suggest a `time_variable` in an error message.
time_candidates <- function(data) {
  cols <- names(data)
  is_time <- vapply(cols, function(col) {
    x <- data[[col]]
    is_date_variable(x) ||
      (grepl("year|date|time|period", col, ignore.case = TRUE) &&
         (is.numeric(x) || all(grepl("^[0-9]{4}$", x[!is.na(x)]))))
  }, logical(1))
  cols[is_time]
}

# For arguments whose default is the vector of possible choices but which
# must be chosen explicitly (a measure, a model, an algorithm): abort with
# the list of choices when the user did not pick one.
check_explicit_choice <- function(x, choices, arg, context = NULL,
                                  call = rlang::caller_env()) {
  if (is.null(x) || length(x) != 1 || !x %in% choices) {
    cli::cli_abort(
      c(
        paste0(if (!is.null(context)) paste0(context, ", c") else "C",
               "hoose one value for {.arg {arg}}."),
        "i" = "Possible values: {.val {choices}}."
      ),
      call = call
    )
  }
  x
}

# TRUE for a tbl_graph, FALSE for a list of tbl_graphs, error otherwise.
# (A tbl_graph is itself a list, hence the order of the tests.)
is_single_graph <- function(graphs, arg = "graphs", call = rlang::caller_env()) {
  if (inherits(graphs, "tbl_graph")) {
    return(TRUE)
  }
  if (is.list(graphs) && length(graphs) > 0 &&
      all(vapply(graphs, inherits, logical(1), what = "tbl_graph"))) {
    return(FALSE)
  }
  cli::cli_abort(
    c(
      "{.arg {arg}} must be a {.cls tbl_graph} or a list of {.cls tbl_graph}s.",
      "x" = "It is {.obj_type_friendly {graphs}}."
    ),
    call = call
  )
}

# Node table of a tbl_graph as a tibble, whatever the active table.
node_table <- function(graph) {
  tidygraph::as_tibble(graph, active = "nodes")
}

# Mix two vectors of colours (vectorised): `amount1` of the first colour and
# `1 - amount1` of the second, transparency included. Used to colour edges
# between nodes of different clusters.
mixcolor <- function(col1, col2, amount1 = 0.5) {
  rgb1 <- grDevices::col2rgb(col1, alpha = TRUE)
  rgb2 <- grDevices::col2rgb(col2, alpha = TRUE)
  mix <- amount1 * rgb1 + (1 - amount1) * rgb2
  grDevices::rgb(mix[1, ], mix[2, ], mix[3, ], mix[4, ], maxColorValue = 255)
}

# Values that databases use for a missing identifier (e.g. 0 for the
# references they could not match).
placeholder_values <- c("0", "-1", "", "na", "nan", "null", "none", "unknown", "missing")

# A target shared by many sources ties every pair of them: a code for a
# missing identifier does it by accident, and a target that defines the corpus
# (a corpus of the works citing it) does it by construction. Warn about a
# placeholder-like target linked to at least 1% of the sources, and about any
# target linked to more than half of them (with at least 20 sources).
check_placeholder_target <- function(edges, target_id) {
  # counted with hashing (match/tabulate), much faster than grouping
  links <- if ("document" %in% names(edges)) unique(edges[, c("from", "to")]) else edges
  n_sources <- length(unique(links$from))
  if (n_sources < 20) {
    return(invisible())
  }
  targets <- unique(links$to)
  counts <- tabulate(match(links$to, targets), length(targets))
  share <- counts / n_sources
  placeholder <- tolower(trimws(as.character(targets))) %in% placeholder_values & share >= 0.01
  if (any(placeholder)) {
    i <- which(placeholder)[which.max(share[placeholder])]
    cli::cli_warn(c(
      "The {.field {target_id}} {.val {targets[i]}} is linked to {round(100 * share[i])}% of the sources ({counts[i]}): it looks like a code for a missing identifier.",
      "i" = "It ties every pair of these sources as if they shared a target: set it to {.code NA} or remove its rows."
    ))
  }
  top <- which.max(replace(share, placeholder, 0))
  if (share[top] > 0.5) {
    cli::cli_warn(c(
      "The {.field {target_id}} {.val {targets[top]}} is linked to {round(100 * share[top])}% of the sources: it ties most pairs of sources.",
      "i" = "If it is a code for a missing identifier, set it to {.code NA} or remove its rows.",
      "i" = "If the corpus was built from it (e.g. the works citing it), it says nothing about the differences between sources: consider removing its rows."
    ))
  }
  invisible()
}
