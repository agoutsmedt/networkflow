# The model is simulated by a fake ellmer chat object: these tests need
# neither a model nor a server. It answers "Label_of_<first node listed>"
# (cleaned into "Label of <node>"), and fails on the calls given in `fail`.
fake_chat <- function(fail = integer(0)) {
  record <- new.env()
  record$calls <- list()
  make <- function(system = "The system prompt of the user.", turns = list("an earlier turn")) {
    self <- new.env()
    self$system <- system
    self$turns <- turns
    self$clone <- function(deep = FALSE) make(self$system, self$turns)
    self$set_turns <- function(value) { self$turns <- value; invisible(self) }
    self$set_system_prompt <- function(value) { self$system <- value; invisible(self) }
    self$chat_structured <- function(prompt, type) {
      record$calls[[length(record$calls) + 1]] <- list(system = self$system, turns = self$turns, prompt = prompt)
      if (length(record$calls) %in% fail) stop("the server failed")
      first <- regmatches(prompt, regexpr("(?<=\n- )[a-z]+", prompt, perl = TRUE))
      list(label = paste0("Label_of_", if (length(first)) first else "nothing"))
    }
    class(self) <- c("Chat", "R6")
    self
  }
  list(chat = make(), record = record)
}

clustered_toy_llm <- function() {
  add_clusters(toy_graph(), clustering_method = "leiden",
               objective_function = "modularity", seed = 1, verbose = FALSE) %>%
    add_node_roles(cluster_id = "cluster_leiden")
}

# The arguments shared by most calls.
name_llm <- function(graphs, ..., cluster_id = "cluster_leiden", prompt_columns = c("id", "title"),
                     order_by = "within_module_degree", nb_nodes = 10) {
  name_clusters(graphs, method = "llm", cluster_id = cluster_id, prompt_columns = prompt_columns,
                order_by = order_by, nb_nodes = nb_nodes, ...)
}

test_that("method = 'llm' needs prompt_columns, order_by and nb_nodes", {
  graph <- clustered_toy_llm()
  expect_error(name_clusters(graph, method = "llm", cluster_id = "cluster_leiden",
                             order_by = "within_module_degree", nb_nodes = 10, preview = TRUE),
               "prompt_columns")
  # The former name of the other methods is pointed out.
  expect_error(name_clusters(graph, method = "llm", cluster_id = "cluster_leiden", label_columns = "id",
                             order_by = "within_module_degree", nb_nodes = 10, preview = TRUE),
               "label_columns.*is used by the other methods")
  expect_error(name_llm(graph, order_by = NULL, preview = TRUE), "add_node_roles")
  expect_error(name_llm(graph, nb_nodes = NULL, preview = TRUE), "nb_nodes")
})

test_that("method = 'llm' checks its arguments before calling the model", {
  graph <- clustered_toy_llm()
  for (n in list(-1, 2.5, "ten", NA, c(1, 2))) {
    expect_error(name_llm(graph, nb_nodes = n, preview = TRUE), "whole number")
  }
  expect_error(name_llm(graph, order_by = "title", preview = TRUE), "numeric column")
  expect_error(name_llm(graph, order_by = "nope", preview = TRUE), "nope")
  expect_error(name_llm(graph, order_by = c("year", "within_module_degree"), preview = TRUE), "one column")
  expect_error(name_llm(graph, prompt_columns = "nope", preview = TRUE), "nope")
  for (context in list(NA, NA_character_, "", c("a", "b"))) {
    expect_error(name_llm(graph, context = context, preview = TRUE), "context")
  }
  expect_error(name_llm(graph, preview = "yes"), "preview")
  expect_error(name_llm(graph, instructions = c("a", "b"), preview = TRUE), "instructions")
  with_list <- tidygraph::mutate(graph, tags = lapply(id, function(x) c("u", "v")))
  expect_error(name_llm(with_list, prompt_columns = c("id", "tags"), preview = TRUE), "list columns")
})

test_that("a misspelt argument is an error, not ignored", {
  graph <- clustered_toy_llm()
  expect_error(name_llm(graph, preveiw = TRUE), "preveiw")
  expect_error(name_clusters(graph, method = "given_column", cluster_id = "cluster_leiden",
                             label_columns = "id", order_by = "year", lable_name = "x"), "lable_name")
})

test_that("method = 'llm' needs a chat object of ellmer", {
  skip_if_not_installed("ellmer")
  graph <- clustered_toy_llm()
  expect_error(name_llm(graph), "chat object of")
  expect_error(name_llm(graph, llm = "mistral"), "chat object of.*not a string")
})

test_that("method = 'llm' labels each cluster with the answer of the model", {
  skip_if_not_installed("ellmer")
  fake <- fake_chat()
  named <- name_llm(clustered_toy_llm(), llm = fake$chat, context = "A toy network.")
  nodes <- node_table(named)
  # In each triangle, a and b (or e and f) are tied only to their own
  # cluster; c and d also to each other (the bridge), which does not count.
  # Ties are kept in the order of the table: a and d come first. The label is
  # cleaned ("Label_of_a" -> "Label of a").
  expect_setequal(unique(nodes$cluster_label), c("Label of a", "Label of d"))
  expect_length(fake$record$calls, 2)
  call <- fake$record$calls[[1]]
  # The instructions replace the system prompt of the user, and the earlier
  # conversation is not sent.
  expect_match(call$system, "2 or 3 words", fixed = TRUE)
  expect_length(call$turns, 0)
  expect_match(call$prompt, "Context of the network: A toy network.", fixed = TRUE)
  # The chat object of the user is not modified.
  expect_equal(fake$chat$system, "The system prompt of the user.")
  expect_length(fake$chat$turns, 1)

  fake <- fake_chat()
  name_llm(clustered_toy_llm(), llm = fake$chat, instructions = "Name the cluster in French.")
  expect_equal(fake$record$calls[[1]]$system, "Name the cluster in French.")
})

test_that("method = 'llm' stops if the first cluster fails, and labels later failures 'no_name'", {
  skip_if_not_installed("ellmer")
  expect_error(name_llm(clustered_toy_llm(), llm = fake_chat(fail = 1)$chat),
               "first cluster.*check `llm`")
  expect_warning(named <- name_llm(clustered_toy_llm(), llm = fake_chat(fail = 2)$chat),
                 "no label for 1 cluster")
  expect_setequal(unique(node_table(named)$cluster_label), c("Label of a", "no_name"))
})

test_that("method = 'llm' warns when a description is long enough to be cut by a server", {
  skip_if_not_installed("ellmer")
  # The values of the nodes are cut, not a column with one value per cluster.
  long <- tidygraph::mutate(clustered_toy_llm(), terms = paste(cluster_leiden, strrep("word ", 3000)))
  expect_warning(name_llm(long, prompt_columns = c("id", "terms"), llm = fake_chat()$chat),
                 "some servers cut longer prompts")
  expect_no_warning(name_llm(clustered_toy_llm(), llm = fake_chat()$chat))
})

test_that("method = 'llm' names dynamic clusters once, across windows", {
  skip_if_not_installed("ellmer")
  fake <- fake_chat()
  windows <- toy_windows(c(a = "01", b = "01", c = "02", d = "02"),
                         c(a = "01", b = "01", c = "02", d = "02"))
  windows <- lapply(windows, tidygraph::mutate, score = seq_along(id))
  named <- name_llm(windows, name_dynamic_clusters = TRUE, cluster_id = "cluster",
                    prompt_columns = "id", order_by = "score", llm = fake$chat)
  expect_length(fake$record$calls, 2)
  expect_equal(node_table(named[[1]])$cluster_label, node_table(named[[2]])$cluster_label)
  # A node present in both windows is counted and listed once.
  prompt <- fake$record$calls[[1]]$prompt
  expect_match(prompt, "2 nodes")
  expect_match(prompt, "Present in 2 time windows")
  expect_equal(lengths(gregexpr("\n- ", prompt)), 2)
})

test_that("clusters numbered within each window are reported with name_dynamic_clusters = TRUE", {
  per_window <- toy_windows(c(a = "01", b = "01", c = "02"), c(d = "01", e = "02", f = "02"),
                            c(g = "01", h = "01", i = "02"))
  expect_warning(name_clusters(per_window, method = "tf-idf", name_dynamic_clusters = TRUE,
                               cluster_id = "cluster", text_columns = "id"),
                 "may not be unique across windows")
  # Dynamic clusters that are born and die: no warning.
  dynamic <- toy_windows(c(a = "01", b = "01", c = "02"), c(d = "01", e = "03", f = "03"),
                         c(g = "03", h = "04", i = "04"))
  expect_no_warning(name_clusters(dynamic, method = "tf-idf", name_dynamic_clusters = TRUE,
                                  cluster_id = "cluster", text_columns = "id"))
})

test_that("the description of a cluster gathers its context, cluster columns and main nodes", {
  nodes <- data.frame(
    id = c("d1", "d2", "d3", "d4"), cl = c("A", "A", "A", "B"), year = c(2000, 2004, 2002, 1990),
    journal = c("J1", "J1", "J2", "J3"), title = c("t one", "t two", "t three", "t four"),
    score = c(1, 3, 2, 1), tfidf = c("inflation, policy", "inflation, policy", "inflation, policy", "oil")
  )
  prompts <- llm_cluster_prompts(nodes, "cl", c("title", "year", "journal", "tfidf"), "score",
                                 nb_nodes = 2, context = "Toy.")
  prompt <- prompts[["A"]]
  expect_match(prompt, "Context of the network: Toy.", fixed = TRUE)
  expect_match(prompt, "3 nodes")
  # A column with one value per cluster is written once, for the cluster.
  expect_match(prompt, "\ntfidf: inflation, policy\n", fixed = TRUE)
  expect_match(prompt, "(highest score first; title | year | journal)", fixed = TRUE)
  # The two nodes of highest score, in order.
  expect_match(prompt, "- t two | 2004 | J1\n- t three | 2002 | J2", fixed = TRUE)
  expect_false(grepl("t one", prompt))
  expect_match(prompts[["B"]], "tfidf: oil", fixed = TRUE)
})

test_that("the number of nodes is right when only cluster columns are given, or nodes share their values", {
  nodes <- data.frame(cl = c("A", "A", "A", "B", "B"), score = 1:5,
                      title = c("Comment", "Comment", "Other", "x", "y"),
                      tfidf = c("inflation", "inflation", "inflation", "oil", "oil"))
  # Only a column with one value per cluster: no list of nodes.
  prompt <- llm_cluster_prompts(nodes, "cl", "tfidf", "score", nb_nodes = 10)[["A"]]
  expect_match(prompt, "Cluster \"A\": 3 nodes.\ntfidf: inflation", fixed = TRUE)
  expect_false(grepl("Main nodes", prompt, fixed = TRUE))
  # Two nodes with the same title are two nodes (of one graph).
  prompt <- llm_cluster_prompts(nodes, "cl", "title", "score", nb_nodes = 10)[["A"]]
  expect_match(prompt, "3 nodes")
  expect_equal(lengths(gregexpr("\n- Comment", prompt)), 2)
})

test_that("nb_nodes = 0 lists no node, and missing values are left out", {
  nodes <- data.frame(cl = c("A", "A"), score = 1:2, title = c("t1", NA), empty = NA_character_,
                      tfidf = c("inflation", "inflation"))
  prompt <- llm_cluster_prompts(nodes, "cl", c("title", "tfidf"), "score", nb_nodes = 0)[["A"]]
  expect_equal(prompt, "Cluster \"A\": 2 nodes.\ntfidf: inflation")
  # An empty column (one missing value per cluster) is not written; a node
  # without any value is not listed.
  prompt <- llm_cluster_prompts(nodes, "cl", c("title", "empty"), "score", nb_nodes = 5)[["A"]]
  expect_false(grepl("empty", prompt, fixed = TRUE))
  expect_true(endsWith(prompt, "(highest score first; title):\n- t1"))
})

test_that("a column with one value per cluster is written in full, line breaks included", {
  long <- paste0("\n  - cluster ", 1:40, ": ", strrep("term ", 5), collapse = "")
  nodes <- data.frame(cl = c("A", "A"), title = c("t1", "t2"), score = 1:2, others = long)
  prompt <- llm_cluster_prompts(nodes, "cl", c("title", "others"), "score", nb_nodes = 5)[["A"]]
  # A value starting with a line break keeps it (a list under the name).
  expect_match(prompt, paste0("others:", sub("\\s+$", "", long)), fixed = TRUE)
  expect_match(prompt, "\n  - cluster 40: ", fixed = TRUE)
  # The values of the nodes are still cut.
  nodes$title <- paste0(strrep("a", 400), 1:2)
  prompt <- llm_cluster_prompts(nodes, "cl", c("title", "others"), "score", nb_nodes = 5)[["A"]]
  expect_false(grepl(strrep("a", 300), prompt, fixed = TRUE))
})

test_that("preview = TRUE returns the messages without a model", {
  prompts <- name_llm(clustered_toy_llm(), context = "Toy.", instructions = "Name it.", preview = TRUE)
  expect_s3_class(prompts, "tbl_df")
  expect_named(prompts, c("cluster_leiden", "instructions", "description"))
  expect_equal(nrow(prompts), 2)
  expect_equal(unique(prompts$instructions), "Name it.")
  expect_match(prompts$description[1], "Context of the network: Toy.", fixed = TRUE)
  expect_match(prompts$description[1], "highest within_module_degree first", fixed = TRUE)
})

test_that("preview = TRUE gives one row per graph and cluster for a list of graphs", {
  windows <- toy_windows(w1 = c(a = "01", b = "01", c = "02", d = "02"),
                         w2 = c(a = "01", b = "01", c = "02", d = "02"))
  windows <- lapply(windows, tidygraph::mutate, score = seq_along(id))
  per_window <- name_llm(windows, cluster_id = "cluster", prompt_columns = "id", order_by = "score", preview = TRUE)
  expect_equal(per_window$graph, c("w1", "w1", "w2", "w2"))
  dynamic <- name_llm(windows, cluster_id = "cluster", name_dynamic_clusters = TRUE,
                      prompt_columns = "id", order_by = "score", preview = TRUE)
  expect_named(dynamic, c("cluster", "instructions", "description"))
  expect_equal(nrow(dynamic), 2)
})

test_that("the labels of the model are cleaned", {
  expect_equal(clean_label("\"Asset_Pricing.\""), "Asset Pricing")
  expect_equal(clean_label("TimeSeriesAnalysis"), "Time Series Analysis")
  expect_equal(clean_label("  Monetary   policy rules "), "Monetary policy rules")
  # A label with spaces keeps its capitals ("IPO", "McKenzie").
  expect_equal(clean_label("IPO Underpricing McKenzie"), "IPO Underpricing McKenzie")
  expect_error(clean_label(""), "no label")
  expect_error(clean_label(NULL), "no label")
})
