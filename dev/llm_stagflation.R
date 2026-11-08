# name_clusters(method = "llm") on the data of the package: the 174 articles
# on stagflation (Nodes_stagflation, Ref_stagflation), linked by their shared
# references.
#
# Needs: install.packages("ellmer"); a local model, here with Ollama
# (install it, then `ollama pull mistral` in a terminal; keep the server
# running). Run from the root of the package. Part 1 needs no model.
# Time on a CPU: a few seconds per cluster.

pkgload::load_all(".", quiet = TRUE)

# Settings to change ---------------------------------------------------------
model <- "mistral"
nb_nodes <- 10

# The network: coupling, Leiden clusters, tf-idf label, roles of the nodes ----
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 1,
  verbose = FALSE
)
network <- add_clusters(network, clustering_method = "leiden", objective_function = "modularity",
                        seed = 123, verbose = FALSE)
network <- name_clusters(network, method = "tf-idf", cluster_id = "cluster_leiden",
                         text_columns = "source_title", clean_word_method = "none")
network <- add_node_roles(network, cluster_id = "cluster_leiden")
context <- "Economics articles on stagflation, linked by their shared references (bibliographic coupling)."

# 1. What the model reads (no model needed) ----------------------------------
prompts <- name_clusters(network, method = "llm", cluster_id = "cluster_leiden",
                         prompt_columns = c("source_title", "source_year", "cluster_label"),
                         order_by = "within_module_degree", nb_nodes = nb_nodes,
                         context = context, preview = TRUE)
cat(prompts$instructions[1], "\n\n", prompts$description[1], "\n\n", sep = "")

# 2. The labels of the model --------------------------------------------------
rlang::check_installed("ellmer")
chat <- ellmer::chat_ollama(model = model, params = ellmer::params(temperature = 0, seed = 123))
# Another local server: ellmer::chat_lmstudio(), chat_vllm(), chat_openai_compatible()...
network <- name_clusters(network, method = "llm", cluster_id = "cluster_leiden",
                         prompt_columns = c("source_title", "source_year", "cluster_label"),
                         order_by = "within_module_degree", nb_nodes = nb_nodes,
                         label_name = "llm_label", llm = chat, context = context)
labels <- unique(tidygraph::as_tibble(network)[c("cluster_leiden", "size_cluster_leiden", "cluster_label", "llm_label")])
print(as.data.frame(labels[order(-labels$size_cluster_leiden), ]))
