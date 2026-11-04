#: Benchmark of the networkflow pipeline: former vs current version -------
#
# Goal. Measure how long each step of the dynamic pipeline takes, and how
# much memory it uses, on corpora of increasing size, for the former
# version of the package (git ref `old_ref`) and the current working tree;
# and check that the results are unchanged where no change was intended.
#
# Data. Random samples of citing documents published in 1995-2004 in the
# authors' Scopus database (~/data/scopus/scopus.duckdb, table `reference`,
# disambiguated `cited_id`), with their titles. Samples of 2,000, 8,000 and
# 20,000 documents. The extraction is cached in
# tools::R_user_dir("networkflow", "cache"). Scopus data cannot be shared:
# this script runs only where the database exists, and bench/ is excluded
# from the package build (.Rbuildignore).
#
# Pipeline benchmarked (the steps of the guides), on sliding 5-year windows
# (6 networks): build_dynamic_networks (coupling strength, threshold 2,
# main component), add_clusters (Leiden, modularity, seed 1),
# merge_dynamic_clusters (partial, 0.51), name_clusters (tf-idf on titles,
# merged clusters, no lemmatisation), add_node_roles, color_networks,
# layout_networks (fr, on the last window only: layouts dominate otherwise),
# networks_to_alluv and minimize_crossing_alluvial.
#
# How. Each version is installed in its own temporary library; each
# (version, size) runs in a fresh R process (callr), so that timings and
# memory do not leak between runs. Memory is the maximum of R's heap
# reported by gc() during the step (it ignores memory used outside R's heap,
# e.g. by igraph's C code).
#
# Outputs. bench/results.csv (time and memory per step, version and size),
# bench/equality.csv (comparison of the results of both versions).
#
# Expected differences (intended changes, see NEWS.md): cluster identifiers
# are now numbered by size (partitions are compared with the adjusted Rand
# index, not by label); add_node_roles() had a bug for the "to" end of edges
# (roles differ); edge colours of the former version used the colour mixing
# of the deprecated community_colors.R.
#
# Run from the package root: Rscript bench/benchmark_functions.R

#: 1) Settings -------------------------------------------------------------
old_ref <- "30ebd05" # last commit before the 2026-11 review
sizes <- c(2000L, 8000L, 20000L)
db_path <- path.expand("~/data/scopus/scopus.duckdb")
cache_dir <- tools::R_user_dir("networkflow", "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
pkg_dir <- normalizePath(".")
stopifnot(file.exists(file.path(pkg_dir, "DESCRIPTION")))
if (!file.exists(db_path)) cli::cli_abort("The Scopus database {.file {db_path}} is not available.")

#: 2) Data: cached samples of the Scopus corpus ----------------------------
corpus_file <- file.path(cache_dir, "bench_scopus_1995_2004.rds")
if (!file.exists(corpus_file)) {
  cli::cli_progress_step("Extracting the corpus from Scopus")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), db_path, read_only = TRUE)
  nodes <- DBI::dbGetQuery(con, "
    SELECT a.scopus_id AS source_id, a.dc_title AS title,
           CAST(substr(a.prism_cover_date, 1, 4) AS INTEGER) AS year
    FROM article a
    WHERE substr(a.prism_cover_date, 1, 4) BETWEEN '1995' AND '2004'
      AND a.scopus_id IN (SELECT DISTINCT citing_id FROM reference)")
  refs <- DBI::dbGetQuery(con, "
    SELECT r.citing_id AS source_id, r.cited_id AS target_id
    FROM reference r JOIN article a ON a.scopus_id = r.citing_id
    WHERE substr(a.prism_cover_date, 1, 4) BETWEEN '1995' AND '2004'
      AND r.cited_id IS NOT NULL")
  DBI::dbDisconnect(con, shutdown = TRUE)
  saveRDS(list(nodes = nodes, refs = refs), corpus_file)
}
corpus <- readRDS(corpus_file)
cli::cli_inform("Corpus: {nrow(corpus$nodes)} documents, {nrow(corpus$refs)} references.")

# The same random samples for both versions.
samples <- lapply(sizes, function(n) {
  ids <- withr::with_seed(2026, sample(corpus$nodes$source_id, n))
  list(
    nodes = corpus$nodes[corpus$nodes$source_id %in% ids, ],
    refs = corpus$refs[corpus$refs$source_id %in% ids, ]
  )
})
names(samples) <- sizes
sample_files <- vapply(names(samples), function(n) {
  file <- file.path(cache_dir, paste0("bench_sample_", n, ".rds"))
  saveRDS(samples[[n]], file)
  file
}, character(1))

#: 3) Install both versions in temporary libraries ------------------------
lib_old <- file.path(tempdir(), "lib_old")
lib_new <- file.path(tempdir(), "lib_new")
dir.create(lib_old)
dir.create(lib_new)
old_src <- file.path(tempdir(), "networkflow_old")
system2("git", c("-C", shQuote(pkg_dir), "worktree", "add", "--detach", shQuote(old_src), old_ref))
for (x in list(c(old_src, lib_old), c(pkg_dir, lib_new))) {
  status <- system2("R", c("CMD", "INSTALL", "--no-docs", "--no-test-load",
                           paste0("--library=", shQuote(x[2])), shQuote(x[1])),
                    stdout = FALSE, stderr = FALSE)
  if (status != 0) cli::cli_abort("Installation of {.file {x[1]}} failed.")
}

#: 4) The pipeline, run in a fresh process --------------------------------
run_pipeline <- function(sample_file, output_file) {
  suppressPackageStartupMessages(library(networkflow))
  data <- readRDS(sample_file)
  timings <- list()
  # Time and maximum R heap (Mb) of one step.
  step <- function(name, expr) {
    gc(reset = TRUE)
    time <- system.time(value <- suppressMessages(suppressWarnings(force(expr))))[["elapsed"]]
    memory <- sum(gc()[, 6])
    timings[[name]] <<- data.frame(step = name, seconds = time, max_mb = memory)
    value
  }
  networks <- step("build_dynamic_networks", build_dynamic_networks(
    data$nodes, data$refs, "source_id", "target_id",
    time_variable = "year", time_window = 5, window_step = 1,
    method = "strength",
    edges_threshold = 2, filter_components = TRUE, verbose = FALSE
  ))
  networks <- step("add_clusters", add_clusters(
    networks, clustering_method = "leiden", objective_function = "modularity",
    seed = 1, verbose = FALSE
  ))
  networks <- step("merge_dynamic_clusters", merge_dynamic_clusters(
    networks, cluster_id = "cluster_leiden", node_id = "source_id",
    threshold_similarity = 0.51, similarity_type = "partial"
  ))
  networks <- step("name_clusters", name_clusters(
    networks, method = "tf-idf", name_merged_clusters = TRUE,
    cluster_id = "dynamic_cluster_leiden", text_columns = "title",
    nb_terms_label = 3, clean_word_method = "none"
  ))
  networks <- step("add_node_roles", add_node_roles(networks, cluster_id = "dynamic_cluster_leiden"))
  networks <- step("color_networks", color_networks(networks, column_to_color = "dynamic_cluster_leiden"))
  last <- step("layout_networks (last window)", withr::with_seed(1, layout_networks(
    networks[[length(networks)]], node_id = "source_id", layout = "fr"
  )))
  alluv <- step("networks_to_alluv", networks_to_alluv(
    networks, cluster_id = "dynamic_cluster_leiden", node_id = "source_id"
  ))
  alluv <- step("minimize_crossing_alluvial", minimize_crossing_alluvial(
    alluv, cluster_id = "dynamic_cluster_leiden", node_id = "source_id"
  ))
  # Results kept for the comparison of both versions.
  saveRDS(list(
    nodes = lapply(networks, function(g) tidygraph::as_tibble(g, active = "nodes")),
    edges = lapply(networks, function(g) {
      ids <- tidygraph::as_tibble(g, active = "nodes")$source_id
      e <- tidygraph::as_tibble(g, active = "edges")
      data.frame(a = pmin(ids[e$from], ids[e$to]), b = pmax(ids[e$from], ids[e$to]), weight = e$weight)
    }),
    alluv_rows = nrow(alluv)
  ), output_file)
  do.call(rbind, timings)
}

results <- list()
outputs <- list()
for (size in names(sample_files)) {
  for (version in c("former", "current")) {
    cli::cli_progress_step("{version} version, {size} documents")
    lib <- if (version == "former") lib_old else lib_new
    out <- file.path(cache_dir, sprintf("bench_output_%s_%s.rds", version, size))
    timing <- callr::r(run_pipeline, args = list(sample_files[[size]], out),
                       libpath = c(lib, .libPaths()))
    timing$version <- version
    timing$documents <- as.integer(size)
    results[[paste(version, size)]] <- timing
    outputs[[version]][[size]] <- readRDS(out)
  }
}
# on.exit() does not run at the top level of a script: remove the worktree here.
system2("git", c("-C", shQuote(pkg_dir), "worktree", "remove", "--force", shQuote(old_src)))
results <- do.call(rbind, results)
rownames(results) <- NULL
write.csv(results, file.path(pkg_dir, "bench", "results.csv"), row.names = FALSE)

#: 5) Equality of the results ----------------------------------------------
ari <- function(x, y) igraph::compare(as.integer(factor(x)), as.integer(factor(y)), method = "adjusted.rand")
equality <- do.call(rbind, lapply(names(sample_files), function(size) {
  old <- outputs$former[[size]]
  new <- outputs$current[[size]]
  same_edges <- all(mapply(function(a, b) {
    a <- a[order(a$a, a$b), ]
    b <- b[order(b$a, b$b), ]
    nrow(a) == nrow(b) && all(a$a == b$a & a$b == b$b) && isTRUE(all.equal(a$weight, b$weight))
  }, old$edges, new$edges))
  stack <- function(x, column) {
    unlist(lapply(names(x$nodes), function(w) setNames(x$nodes[[w]][[column]], paste(w, x$nodes[[w]]$source_id))))
  }
  # Window clusters are compared window by window: "01" of two windows are
  # different clusters (dynamic clusters, by contrast, are global).
  old_cl <- stack(old, "cluster_leiden")
  new_cl <- stack(new, "cluster_leiden")[names(old_cl)]
  window_of <- sub(" .*$", "", names(old_cl))
  old_cl <- paste(window_of, old_cl)
  new_cl <- paste(window_of, new_cl)
  old_dyn <- stack(old, "dynamic_cluster_leiden")
  new_dyn <- stack(new, "dynamic_cluster_leiden")[names(old_dyn)]
  old_label <- stack(old, "cluster_label")
  new_label <- stack(new, "cluster_label")[names(old_label)]
  old_role <- stack(old, "role_ga")
  new_role <- stack(new, "role_ga")[names(old_role)]
  data.frame(
    documents = as.integer(size),
    identical_networks = same_edges,
    ari_window_clusters = ari(old_cl, new_cl),
    ari_dynamic_clusters = ari(old_dyn, new_dyn),
    identical_labels = all(old_label == new_label),
    share_identical_roles = mean(old_role == new_role),
    identical_alluvial_rows = old$alluv_rows == new$alluv_rows
  )
}))
write.csv(equality, file.path(pkg_dir, "bench", "equality.csv"), row.names = FALSE)
print(equality)

#: 6) Summary --------------------------------------------------------------
summary <- reshape(results[, c("step", "documents", "version", "seconds")],
                   idvar = c("step", "documents"), timevar = "version", direction = "wide")
summary$speed_up <- round(summary$seconds.former / summary$seconds.current, 1)
print(summary[order(summary$documents), ], row.names = FALSE)
