# ------------------------------------------------------------------------------
# Evaluation of name_clusters(method = "llm"), step 3: blind judging by Haiku
#
# Goals: score every candidate label (all models x configurations) on accuracy and
#   specificity (bench/llm_labels/rubric.md), blind to the model and the prompt.
# Usage: Rscript 03_judging.R prepare   -> one input file per sample (judge sees
#   titles of the cluster and of the others, and the shuffled unique labels);
#        Rscript 03_judging.R aggregate -> reads verdicts, joins them to the
#   labels, writes scores.rds. Judging itself is done by Haiku subagents.
# Files: ~/data/networkflow/evaluation/llm_labels/judge/ (never overwritten)
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
action <- commandArgs(TRUE)[1]
dir <- path.expand("~/data/networkflow/evaluation/llm_labels")
jdir <- file.path(dir, "judge"); dir.create(jdir, showWarnings = FALSE)
labels <- list.files(dir, "^labels_.*rds$", full.names = TRUE) |> lapply(readRDS) |> bind_rows()
norm <- function(x) tolower(trimws(gsub("[^[:alnum:] ]", "", x)))

if (action == "prepare") {
  res <- readRDS(file.path(dir, "clusters.rds"))
  set.seed(20261107)
  key <- labels |> mutate(label_norm = norm(label)) |>
    distinct(sample, cluster = cluster_leiden, label_norm, .keep_all = TRUE) |>
    group_by(sample, cluster = cluster_leiden) |> mutate(label_id = sample(n())) |> ungroup()
  # lookup: normalised label -> id (for the aggregation); the judge never sees it
  lookup <- key |> select(sample, cluster, label_norm, label_id)
  saveRDS(lookup, file.path(jdir, "lookup.rds"))
  for (s in names(res)) {
    nodes <- tidygraph::as_tibble(res[[s]]) |> arrange(cluster_leiden, desc(within_module_degree))
    txt <- unlist(lapply(unique(nodes$cluster_leiden), function(cl) {
      own <- nodes[nodes$cluster_leiden == cl, ]
      others <- nodes[nodes$cluster_leiden != cl, ] |> group_by(cluster_leiden) |> slice_head(n = 4) |> ungroup()
      cands <- key |> filter(sample == s, cluster == cl) |> arrange(label_id)
      c(sprintf("=== CLUSTER %s (%d nodes)", cl, nrow(own)),
        "Most central titles:", sprintf("- %s (%s, %s)", own$title[1:min(15, nrow(own))], own$year[1:min(15, nrow(own))], own$journal[1:min(15, nrow(own))]),
        "Titles of the other clusters of this network:",
        sprintf("- [%s] %s", others$cluster_leiden, others$title),
        "Candidate labels:", sprintf("%d: %s", cands$label_id, cands$label), "")
    }))
    writeLines(c(sprintf("SAMPLE %s", s), "", txt), file.path(jdir, sprintf("input_%s.txt", s)))
  }
  cli::cli_alert_success("{nrow(key)} distinct (cluster, label) pairs to judge")
}

if (action == "aggregate") {
  lookup <- readRDS(file.path(jdir, "lookup.rds"))
  v <- list.files(jdir, "^verdicts_.*csv$", full.names = TRUE) |> lapply(\(f) read.csv(f, colClasses = c(sample = "character", cluster = "character"))) |> bind_rows()
  scored <- labels |> mutate(label_norm = norm(label), cluster = cluster_leiden) |>
    left_join(lookup, by = c("sample", "cluster", "label_norm")) |>
    left_join(v, by = c("sample", "cluster", "label_id"))
  cli::cli_alert_info("{sum(is.na(scored$accuracy))} labels without verdict")
  saveRDS(scored, file.path(dir, "scores.rds"))
}
