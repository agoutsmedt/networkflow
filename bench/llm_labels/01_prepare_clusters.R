# ------------------------------------------------------------------------------
# Evaluation of name_clusters(method = "llm"), step 1: clusters to be named
#
# Goals: build real clusters, of different kinds, on which the prompts of
#   name_clusters(method = "llm") are compared (script 02).
# Inputs: the samples cached by bench/verification/01_extract_samples.R
#   (Scopus Journal of Monetary Economics, 30,000 random Scopus documents, WoS
#   economics 1990-2004).
# Output: ~/data/networkflow/evaluation/llm_labels/clusters.rds, a named list
#   (one element per sample) of tbl_graph with the leiden clusters of the
#   `n_keep` largest clusters kept (others set aside), node roles and a
#   `key_terms` column (tf-idf terms of the cluster, for a prompt variant).
# Choices: one static network by sample (strength coupling, 5 years, edges
#   >= 2), seed fixed; the samples differ by how homogeneous the corpus is
#   (JME: subfields of one field, hard to tell apart; Scopus 30k: very
#   heterogeneous, easy; WoS economics: in between).
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({library(networkflow); library(dplyr)})
source("bench/verification/00_helpers.R")
out_dir <- path.expand("~/data/networkflow/evaluation/llm_labels")
n_keep <- 7

make <- function(d, target, years) {
  nodes <- d$nodes[d$nodes$year %in% years & !is.na(d$nodes$title), ]
  refs <- d$refs[d$refs$source_id %in% nodes$source_id & !is.na(d$refs[[target]]) & d$refs[[target]] != 0, ]  # 0 = missing id code in WoS
  g <- build_network(nodes, refs, "source_id", target, method = "strength",
                     edges_threshold = 2, filter_components = TRUE, verbose = FALSE)
  g <- add_clusters(g, clustering_method = "leiden", objective_function = "modularity", seed = 1, verbose = FALSE)
  g <- add_node_roles(g, cluster_id = "cluster_leiden")
  tab <- tidygraph::as_tibble(g)
  big <- names(sort(table(tab$cluster_leiden), decreasing = TRUE))[seq_len(min(n_keep, length(unique(tab$cluster_leiden))))]
  cli::cli_alert_info("{length(unique(tab$cluster_leiden))} clusters, {igraph::vcount(g)} nodes; kept sizes: {paste(table(tab$cluster_leiden)[big], collapse = ', ')}")
  g <- g |> tidygraph::activate(nodes) |> filter(cluster_leiden %in% big)
  # key terms of each cluster (tf-idf on titles), written once per cluster
  tf <- extract_tfidf(tidygraph::as_tibble(g), grouping_columns = "cluster_leiden", text_columns = "title", nb_terms = 6)
  kt <- tf |> as_tibble() |> arrange(cluster_leiden, desc(tf_idf)) |> group_by(cluster_leiden) |>
    summarise(key_terms = paste(term, collapse = ", "))
  g |> tidygraph::activate(nodes) |> left_join(kt, by = "cluster_leiden")
}
res <- list(
  jme = make(jme, "target_id", 2005:2014),
  wos = make(wos, "new_id", 1995:1999),
  scopus30k = make(s30, "target_id", 2002:2006)
)
saveRDS(res, file.path(out_dir, "clusters.rds"))
for (s in names(res)) { cat("\n##", s, "\n"); print(tidygraph::as_tibble(res[[s]]) |> group_by(cluster_leiden) |> summarise(n = n(), key_terms = first(key_terms))) }
