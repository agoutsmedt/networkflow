# ------------------------------------------------------------------------------
# Evaluation of name_clusters(method = "llm"), step 2: labels for each variant
#
# Goals: label the clusters of 01_prepare_clusters.R with every combination of
#   model x prompt configuration, to compare them (judged in step 3).
# Inputs: ~/data/networkflow/evaluation/llm_labels/clusters.rds
# Output: ~/data/networkflow/evaluation/llm_labels/labels_<model>.rds (one tibble
#   per model: sample, cluster, config, label, seconds, prompt), saved after each
#   configuration; an existing result is never recomputed (cache), so the script
#   can be stopped and restarted. Usage: Rscript 02_run_labels.R <model> [configs]
# Local models (Ollama, CPU only): gemma3:4b, gemma3:12b, qwen3:14b (thinking
#   switched off with `api_args = list(reasoning_effort = "none")`: ellmer talks to
#   Ollama's OpenAI-compatible endpoint, where `think = FALSE` is NOT understood and
#   a structured answer then takes minutes or times out). temperature 0, seed 123.
# Configurations: system prompt (`instructions`) x what the user side contains
#   (columns, number of nodes, context, key terms). See `configs` below.
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({library(networkflow); library(dplyr)})
args <- commandArgs(TRUE)
model <- args[1]
out_dir <- path.expand("~/data/networkflow/evaluation/llm_labels")
res <- readRDS(file.path(out_dir, "clusters.rds"))
file <- file.path(out_dir, paste0("labels_", gsub(":", "_", model), ".rds"))
done <- if (file.exists(file)) readRDS(file) else NULL

# ---- System prompts ----------------------------------------------------------
# Examples in the detailed prompt come from unrelated fields (not economics),
# so that they cannot leak into the labels.
instr <- list(
  minimal = "Give a short label (2 or 3 words) for this cluster of a network.",
  default = NULL, # the package default
  detailed = paste(
    "You label the clusters of a network for a researcher.",
    "A cluster is a group of nodes (e.g. documents) more linked to each other than to the rest of the network.",
    "You receive the description of one cluster, with its most central nodes.",
    "",
    "Rules:",
    "- Name the topic the nodes share, in 2 to 4 words of ordinary language, e.g. \"Protein folding\", \"Urban heat islands\", \"Medieval trade routes\".",
    "- Be as specific as the information allows: prefer \"Coral bleaching\" to \"Marine biology\". Avoid empty words (studies, analysis, issues, research) and the name of a whole field when the nodes share a narrower topic.",
    "- Base the label on what most nodes have in common, not on one odd node; if the topics are mixed, name the dominant one.",
    "- Use only the information given.",
    "- Answer with the label alone: no quotes, no final period.",
    sep = "\n"
  )
)
context <- c(
  jme = "Articles of the Journal of Monetary Economics published in 2005-2014, linked when they cite the same references (bibliographic coupling).",
  wos = "Economics articles indexed in the Web of Science, 1995-1999, linked when they cite the same references (bibliographic coupling).",
  scopus30k = "Economics and business articles indexed in Scopus, 2002-2006, linked when they cite the same references (bibliographic coupling)."
)

# ---- Configurations ----------------------------------------------------------
configs <- tribble(
  ~config,        ~instr,     ~cols,                           ~n,  ~ctx,  ~terms,
  "c1_min",       "minimal",  "title",                          10, FALSE, FALSE,
  "c2_default",   "default",  "title",                          10, FALSE, FALSE,
  "c3_detailed",  "detailed", "title",                          10, FALSE, FALSE,
  "c4_def_ctx",   "default",  "title",                          10, TRUE,  FALSE,
  "c5_det_ctx",   "detailed", "title",                          10, TRUE,  FALSE,
  "c6_det_ctx_meta", "detailed", "title|year|journal",          10, TRUE,  FALSE,
  "c7_det_ctx_n25", "detailed", "title",                        25, TRUE,  FALSE,
  "c8_det_ctx_terms", "detailed", "title",                      10, TRUE,  TRUE,
  "c9_det_ctx_n5", "detailed", "title",                          5, TRUE,  FALSE
)
if (length(args) > 1) configs <- configs[configs$config %in% args[-1], ]

llm <- ellmer::chat_ollama(model = model, params = ellmer::params(temperature = 0, seed = 123),
                           api_args = if (grepl("qwen3", model)) list(reasoning_effort = "none") else list())

for (i in seq_len(nrow(configs))) {
  cf <- configs[i, ]
  if (!is.null(done) && cf$config %in% done$config) { cli::cli_alert_info("{cf$config}: cached"); next }
  rows <- list()
  for (s in names(res)) {
    cols <- strsplit(cf$cols, "|", fixed = TRUE)[[1]]
    if (cf$terms) cols <- c(cols, "key_terms")
    args_i <- list(res[[s]], method = "llm", cluster_id = "cluster_leiden", prompt_columns = cols,
                   order_by = "within_module_degree", nb_nodes = cf$n, llm = llm,
                   instructions = instr[[cf$instr]], context = if (cf$ctx) context[[s]])
    args_i <- args_i[!vapply(args_i, is.null, logical(1))]
    t <- system.time(out <- do.call(name_clusters, args_i))[["elapsed"]]
    pv <- do.call(name_clusters, c(args_i[names(args_i) != "llm"], list(preview = TRUE)))
    lab <- tidygraph::as_tibble(out) |> distinct(cluster_leiden, label = cluster_label)
    rows[[s]] <- lab |> mutate(sample = s, config = cf$config, model = model, seconds = t / nrow(lab),
                               prompt = pv$description[match(cluster_leiden, pv$cluster_leiden)])
    cli::cli_alert_success("{model} {cf$config} {s}: {round(t)} s")
  }
  done <- bind_rows(done, bind_rows(rows))
  saveRDS(done, file)
}
