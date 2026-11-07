# Internal functions of name_clusters(method = "llm"). The model is a chat
# object of ellmer, configured by the user (local server or online service):
# networkflow writes the description of each cluster and reads the label.

# The default instructions, sent as the system prompt before the description
# of each cluster (inserted in the documentation of name_clusters()).
llm_default_instructions <- paste(
  "You label the clusters of a network for a researcher.",
  "A cluster is a group of nodes more linked to each other than to the rest of the network.",
  "From the description of one cluster, give a short label of 2 or 3 words naming what its nodes have in common,",
  "written as ordinary words separated by spaces.",
  "Use only the information given. Do not add quotes or a final period."
)

# `llm` must be a chat object of ellmer.
llm_check_chat <- function(llm, call = rlang::caller_env()) {
  rlang::check_installed("ellmer", reason = "to name clusters with a language model.")
  example <- "llm = ellmer::chat_ollama(model = \"mistral\", params = ellmer::params(temperature = 0, seed = 123))"
  if (is.null(llm)) {
    cli::cli_abort(c(
      "With {.code method = \"llm\"}, give the model in {.arg llm}: a chat object of {.pkg ellmer}.",
      "i" = "E.g. {.code {example}}.",
      "i" = "To read the descriptions sent to the model without one, use {.code preview = TRUE}."
    ), call = call)
  }
  if (!inherits(llm, "Chat")) {
    cli::cli_abort(c(
      "{.arg llm} must be a chat object of {.pkg ellmer}, not {.obj_type_friendly {llm}}.",
      "i" = "E.g. {.code {example}}."
    ), call = call)
  }
  invisible(llm)
}

# Shorten a long text (an abstract in a node column) to `n` characters.
shorten <- function(x, n = 300) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  ifelse(!is.na(x) & nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)
}

# Rows of a table as lines "- a | b | c", missing values left out (and rows
# with no value at all).
format_rows <- function(table) {
  if (nrow(table) == 0) return(character(0))
  cells <- lapply(table, shorten)
  rows <- vapply(seq_len(nrow(table)), function(i) {
    row <- vapply(cells, `[`, character(1), i)
    paste(row[!is.na(row) & nzchar(row)], collapse = " | ")
  }, character(1))
  paste0("- ", rows[nzchar(rows)])
}

# The description of each cluster sent to the model, one character string
# per cluster, named by cluster.
#   nodes: node table (stacked windows for dynamic clusters, with a
#     `.nf_window` column).
#   columns: the node columns to write. Those with one value in each cluster
#     (a label computed before) describe the cluster and are written once;
#     the others describe the nodes.
llm_cluster_prompts <- function(nodes, cluster_id, columns, order_by, nb_nodes, context = NULL) {
  nodes <- nodes[!is.na(nodes[[cluster_id]]), , drop = FALSE]
  groups <- as.character(nodes[[cluster_id]])
  clusters <- sort(unique(groups))
  dynamic <- ".nf_window" %in% names(nodes)
  # A column describes the clusters when it has one value in each cluster of
  # several rows (a cluster of one node says nothing about it).
  several <- groups %in% names(which(table(groups) > 1))
  per_cluster <- vapply(columns, function(col) {
    any(several) && all(tapply(nodes[[col]][several], groups[several], function(x) length(unique(x)) == 1))
  }, logical(1))
  cluster_columns <- columns[per_cluster]
  node_columns <- columns[!per_cluster]

  prompts <- vapply(clusters, function(cl) {
    cluster_nodes <- nodes[groups == cl, , drop = FALSE]
    windows <- if (dynamic) unique(cluster_nodes[[".nf_window"]])
    cluster_nodes <- cluster_nodes[order(-cluster_nodes[[order_by]]), , drop = FALSE]
    if (dynamic) {
      # A node present in several windows is counted and listed once, with its
      # highest `order_by` (the first after sorting).
      key <- if (length(node_columns) > 0) node_columns else setdiff(names(cluster_nodes), c(".nf_window", order_by))
      cluster_nodes <- cluster_nodes[!duplicated(cluster_nodes[key]), , drop = FALSE]
    }
    n <- nrow(cluster_nodes)

    cluster_values <- vapply(cluster_columns, function(col) {
      value <- cluster_nodes[[col]][1]
      if (is.na(value)) return(NA_character_)
      # Written in full: a column made for the model (the terms of the other
      # clusters...) can be long and laid out on several lines.
      value <- sub("\\s+$", "", as.character(value))
      paste0(col, ":", if (startsWith(value, "\n")) "" else " ", sub("^[ \t]+", "", value))
    }, character(1))

    text <- c(
      if (!is.null(context)) c(paste("Context of the network:", context), ""),
      sprintf("Cluster \"%s\": %d node%s.", cl, n, if (n > 1) "s" else ""),
      if (length(windows) > 1) sprintf("Present in %d time windows, from %s to %s.",
                                       length(windows), windows[1], windows[length(windows)]),
      cluster_values[!is.na(cluster_values)]
    )
    if (length(node_columns) > 0 && nb_nodes > 0) {
      text <- c(text, "",
                sprintf("Main nodes (highest %s first; %s):", order_by, paste(node_columns, collapse = " | ")),
                format_rows(utils::head(cluster_nodes[node_columns], nb_nodes)))
    }
    paste(text, collapse = "\n")
  }, character(1))
  names(prompts) <- clusters
  prompts
}

# Clean the label given by the model: no quotes or final period, words
# separated by spaces ("Asset_Pricing", "AssetPricing" -> "Asset Pricing").
clean_label <- function(label) {
  if (!is.character(label) || length(label) != 1 || is.na(label) || !nzchar(trimws(label))) {
    stop("the model gave no label")
  }
  label <- gsub("_", " ", trimws(label))
  if (!grepl(" ", label)) label <- gsub("([a-z])([A-Z])", "\\1 \\2", label)
  label <- sub("[.]$", "", trimws(gsub("^[\"']|[\"']$", "", trimws(label))))
  gsub("\\s+", " ", label)
}

# Ask the model for the label of each cluster. Returns a named character
# vector (NA for the clusters whose label failed, with a warning). `state`
# records whether the model has answered once in this call of
# name_clusters(): until then, a failure is a problem of configuration and
# stops the run.
llm_labels <- function(prompts, llm, instructions, state) {
  # Some servers cut a long prompt silently to their context window (Ollama:
  # 4096 tokens by default). About 3.5 characters per token.
  tokens <- ceiling(max(nchar(prompts) + nchar(instructions)) / 3.5)
  if (tokens > 4000 && !isTRUE(state$warned_length)) {
    state$warned_length <- TRUE
    cli::cli_warn(c(
      "The longest description is about {tokens} tokens: some servers cut longer prompts without warning.",
      "i" = "With Ollama, the context is 4096 tokens by default: raise it (e.g. {.envvar OLLAMA_CONTEXT_LENGTH} when starting the server), or reduce {.arg nb_nodes} or {.arg prompt_columns}."
    ))
  }
  type <- ellmer::type_object(label = ellmer::type_string("The label of the cluster."))
  ask <- function(prompt) {
    # A copy of the chat for each cluster: no earlier conversation is sent.
    chat <- llm$clone()
    chat$set_turns(list())
    chat$set_system_prompt(instructions)
    clean_label(chat$chat_structured(prompt, type = type)$label)
  }

  labels <- stats::setNames(rep(NA_character_, length(prompts)), names(prompts))
  errors <- character(0)
  cli::cli_progress_bar("Naming clusters", total = length(prompts))
  for (cl in names(prompts)) {
    result <- tryCatch(ask(prompts[[cl]]), error = function(e) e)
    if (inherits(result, "error")) {
      if (!state$answered) {
        cli::cli_abort(c(
          "The model gave no label for the first cluster: check {.arg llm}.",
          "i" = "Try it outside networkflow, e.g. {.code llm$chat(\"Hello\")}.",
          "i" = "A slow model may need more time: {.code options(ellmer_timeout_s = 600)} (300 s by default)."
        ), parent = result, call = state$call)
      }
      errors[cl] <- conditionMessage(result)
    } else {
      state$answered <- TRUE
      labels[cl] <- result
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  if (length(errors) > 0) {
    cli::cli_warn(c(
      "The model gave no label for {length(errors)} cluster{?s}: {.val {names(errors)}} (labelled {.val no_name}).",
      "x" = "{errors[[1]]}",
      "i" = "If the model ran out of time, raise {.code options(ellmer_timeout_s = )} (300 s by default)."
    ))
  }
  labels
}
