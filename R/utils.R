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
