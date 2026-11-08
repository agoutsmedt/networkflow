# Progress of 02_run_labels.R: configurations finished per model (9 each).
# Run: Rscript bench/llm_labels/status.R
dir <- path.expand("~/data/networkflow/evaluation/llm_labels")
for (m in c("gemma3_4b", "qwen3_14b", "gemma3_12b")) {
  f <- file.path(dir, paste0("labels_", m, ".rds"))
  done <- if (file.exists(f)) readRDS(f) else NULL
  n <- if (is.null(done)) 0 else length(unique(done$config))
  sec <- if (is.null(done)) NA else mean(done$seconds)
  cat(sprintf("%-11s %d/9 configurations (mean %.0f s per cluster)\n", m, n, sec))
}
