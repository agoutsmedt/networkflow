source("00_helpers.R")
which <- commandArgs(TRUE)[1]
d <- switch(which, jme = jme, s30 = s30, wos = wos)
target <- if (which == "wos") "new_id" else "target_id"
nodes <- d$nodes; refs <- d$refs
if (which == "s30") nodes <- nodes[nodes$year >= 1980 & nodes$year <= 2008, ]
cat("== dataset", which, nrow(nodes), "nodes", nrow(refs), "links\n")
win <- if (which == "jme") 5 else 5
nets <- run("build_dynamic_networks", build_dynamic_networks(nodes, refs, "source_id", target, method = "strength", time_variable = "year", time_window = win, window_step = 1, edges_threshold = 2, filter_components = TRUE))
cat("  windows:", length(nets), " nodes/edges of last:", desc(nets[[length(nets)]]), " total edges:", sum(sapply(nets, igraph::ecount)), "\n")
for (alg in c("leiden", "louvain", "fast_greedy", "walktrap", "infomap")) {
  x <- run(paste("add_clusters", alg), add_clusters(nets, clustering_method = alg, objective_function = "modularity", seed = 1), show = alg %in% c("leiden","infomap"))
  if (!inherits(x, "err")) cat("  clusters per window (first 5):", head(sapply(x, function(g) length(unique(tidygraph::as_tibble(g)[[paste0("cluster_", alg)]]))), 5), "\n")
}
nets <- run("add_clusters leiden (kept)", add_clusters(nets, clustering_method = "leiden", objective_function = "modularity", seed = 1, verbose = FALSE), show = FALSE)
x <- run("add_clusters leiden CPM resolution 0.01", add_clusters(nets[1:3], clustering_method = "leiden", objective_function = "CPM", resolution = 0.01, seed = 1))
x <- run("add_clusters leiden res 2", add_clusters(nets, clustering_method = "leiden", objective_function = "modularity", resolution = 2, seed = 1, verbose = FALSE), show = FALSE)
maj <- run("merge majority .51 partial", merge_dynamic_clusters(nets, "cluster_leiden", "source_id", method = "majority", threshold_similarity = 0.51, similarity_type = "partial"))
jac <- run("merge jaccard .3 partial", merge_dynamic_clusters(nets, "cluster_leiden", "source_id", method = "jaccard", threshold_similarity = 0.3, similarity_type = "complete", verbose = FALSE))
cat("  dynamic clusters majority:", length(unique(unlist(lapply(maj, function(g) tidygraph::as_tibble(g)$dynamic_cluster_leiden)))), " jaccard:", length(unique(unlist(lapply(jac, function(g) tidygraph::as_tibble(g)$dynamic_cluster_leiden)))), "\n")
dg <- run("merge_diagnostics majority (8 thresholds x 2)", merge_diagnostics(nets, "cluster_leiden", "source_id", method = "majority", verbose = FALSE))
dj <- run("merge_diagnostics jaccard", merge_diagnostics(nets, "cluster_leiden", "source_id", method = "jaccard", similarity_types = "partial", verbose = FALSE))
p <- run("plot_merge_diagnostics", print(plot_merge_diagnostics(dg)))
nm <- run("name_clusters tf-idf dynamic, lemmatize", name_clusters(maj, method = "tf-idf", name_dynamic_clusters = TRUE, cluster_id = "dynamic_cluster_leiden", text_columns = "title", clean_word_method = "lemmatize"))
nm2 <- run("name_clusters tf-idf per window, none", name_clusters(maj, method = "tf-idf", cluster_id = "cluster_leiden", text_columns = "title", clean_word_method = "none"))
nm3 <- run("name_clusters pagerank", name_clusters(maj, method = "tidygraph_functions", cluster_id = "cluster_leiden", tidygraph_function = tidygraph::centrality_pagerank(), label_columns = "title"))
x <- run("name_clusters tf-idf without method", name_clusters(maj[1:2], cluster_id = "cluster_leiden", text_columns = "title"))
tf <- run("extract_tfidf grouping across list", extract_tfidf(maj, text_columns = "title", grouping_columns = "dynamic_cluster_leiden", grouping_across_list = TRUE, clean_word_method = "none"))
if (!inherits(tf, "err")) print(head(tf, 3))
rl <- run("add_node_roles", add_node_roles(nm, module_col = "cluster_leiden"))
if (!inherits(rl, "err")) print(table(unlist(lapply(rl, function(g) tidygraph::as_tibble(g)$role_ga))))
if (!inherits(rl, "err")) print(table(unlist(lapply(rl, function(g) tidygraph::as_tibble(g)$role))))
co <- run("color_networks dynamic", color_networks(nm, column_to_color = "dynamic_cluster_leiden"))
big <- which.max(sapply(co, igraph::vcount)); cat("  largest window:", names(co)[big], desc(co[[big]]), "\n")
for (lay in if (igraph::vcount(co[[big]]) > 2000) c("fr", "umap") else c("fr", "kk", "forceatlas2", "umap")) { set.seed(1); x <- run(paste("layout", lay, "largest window"), layout_networks(co[[big]], node_id = "source_id", layout = lay), show = lay == "kk") }
dyn <- run("layout fr dynamic all windows", { set.seed(1); layout_networks(co, node_id = "source_id", layout = "fr", compute_dynamic_coordinates = TRUE) })
pl <- run("plot_networks (list) + print last", { p <- plot_networks(dyn, cluster_label_column = "cluster_label", node_size_column = NULL); print(p[[length(p)]]); p })
al <- run("networks_to_alluv", networks_to_alluv(co, "dynamic_cluster_leiden", "source_id"))
cat("  alluv rows:", nrow(al), " clusters:", length(unique(al$dynamic_cluster_leiden)), "\n")
mc <- run("minimize_crossing_alluvial (refine)", minimize_crossing_alluvial(al, "dynamic_cluster_leiden", "source_id"))
mc0 <- run("minimize_crossing_alluvial (no refine)", minimize_crossing_alluvial(al, "dynamic_cluster_leiden", "source_id", refine = FALSE))
pa <- run("plot_alluvial full options", print(plot_alluvial(co, "dynamic_cluster_leiden", "source_id", color_alluvial = TRUE, minimize_crossing = TRUE, prepare_label = TRUE, min_windows_cluster = 3)))
cat("VmHWM:", grep("VmHWM", readLines("/proc/self/status"), value = TRUE), "\n")
