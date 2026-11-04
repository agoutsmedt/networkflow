suppressPackageStartupMessages({library(networkflow); library(data.table)})
cache <- tools::R_user_dir("networkflow", "cache")
jme <- readRDS(file.path(cache, "verif_scopus_jme.rds"))
s30 <- readRDS(file.path(cache, "verif_scopus_30k.rds"))
wos <- readRDS(file.path(cache, "verif_wos_econ.rds"))
# run an expression, print time, R heap peak, messages and warnings (truncated)
run <- function(label, expr, show = TRUE) {
  gc(reset = TRUE)
  msgs <- character(); warns <- character()
  t <- system.time(val <- tryCatch(withCallingHandlers(expr,
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) structure(conditionMessage(e), class = "err")))[["elapsed"]]
  mem <- sum(gc()[, 6])
  cat(sprintf("\n### %s: %.1fs, heap max %.0f Mb\n", label, t, mem))
  if (show) {
    if (length(msgs)) cat("  MSG:", gsub("\n", " | ", substr(unique(msgs), 1, 400)), sep = "\n  ")
    if (length(warns)) cat("  WARN:", gsub("\n", " | ", substr(unique(warns), 1, 400)), sep = "\n  ")
    if (inherits(val, "err")) cat("  ERROR:", gsub("\n", " | ", substr(val, 1, 600)), "\n")
  }
  invisible(val)
}
desc <- function(g) if (inherits(g, "tbl_graph")) sprintf("%d nodes, %d edges", igraph::vcount(g), igraph::ecount(g)) else paste(names(g), sapply(g, igraph::vcount), sapply(g, igraph::ecount), sep = ":", collapse = " ")
