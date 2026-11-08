# Give readable names to clusters

**\[experimental\]**

A cluster identifier ("01", "cl_12") says nothing about its content.
`name_clusters()` gives each cluster a provisional label that helps
reading plots and tables, with one of four methods: the words that
distinguish the cluster from the others (`"tf-idf"`), the metadata of
its most central node (`"tidygraph_functions"`) or of its node with the
highest value of a given column (`"given_column"`), or a label written
by a language model from a description of the cluster (`"llm"`,
optional, through the [`ellmer`](https://ellmer.tidyverse.org) package:
a model on your computer or an online service). These labels are
starting points for interpretation, not the result of it: naming a
cluster requires reading its documents, the references they share, and
what distinguishes them from neighbouring clusters (Ding 2011) .

## Usage

``` r
name_clusters(
  graphs,
  method = c("tidygraph_functions", "given_column", "tf-idf", "llm"),
  name_dynamic_clusters = FALSE,
  cluster_id,
  label_columns,
  label_name = "cluster_label",
  tidygraph_function = NULL,
  order_by = NULL,
  text_columns = NULL,
  nb_terms_label = 3,
  prompt_columns = NULL,
  nb_nodes = NULL,
  llm = NULL,
  instructions = NULL,
  context = NULL,
  preview = FALSE,
  name_merged_clusters = NULL,
  ...
)
```

## Arguments

- graphs:

  A `tbl_graph` or a list of `tbl_graph`s.

- method:

  `"tf-idf"` (chosen when no method is given), `"tidygraph_functions"`,
  `"given_column"` or `"llm"` (see the details).

- name_dynamic_clusters:

  Logical. `TRUE` when `cluster_id` identifies clusters across a list of
  graphs (the dynamic clusters of
  [`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md)):
  each cluster then gets one name, computed on all its nodes in all
  windows. `FALSE` (default) names the clusters of each graph
  separately.

- cluster_id:

  Character scalar, the node column with the clusters to name.

- label_columns:

  For `"tidygraph_functions"` and `"given_column"`, the node column(s)
  used to build the label of the chosen node, joined by "\_" (e.g.
  `c("source_author", "source_year")`).

- label_name:

  Character scalar, the name of the new column (`"cluster_label"` by
  default).

- tidygraph_function:

  For `"tidygraph_functions"`, a call to a centrality function of
  [`tidygraph`](https://tidygraph.data-imaginist.com/), such as
  [`tidygraph::centrality_pagerank()`](https://tidygraph.data-imaginist.com/reference/centrality.html)
  (see
  [tidygraph::centrality](https://tidygraph.data-imaginist.com/reference/centrality.html)).

- order_by:

  Character scalar, a numeric node column used to rank the nodes of each
  cluster, for two methods (ignored by the others):

  - `"given_column"`: the node with the highest value names the cluster,
    e.g. `"node_size"` from
    [`build_network()`](https://agoutsmedt.github.io/networkflow/reference/build_network.md)
    with `compute_size = TRUE` (the most cited document of the cluster);

  - `"llm"` (required): the `nb_nodes` nodes with the highest values are
    shown to the model, e.g. `"within_module_degree"` from
    [`add_node_roles()`](https://agoutsmedt.github.io/networkflow/reference/add_node_roles.md)
    (the nodes most tied to their own cluster). For dynamic clusters, a
    node present in several windows is ranked by its highest value.

- text_columns:

  For `"tf-idf"`, the node column(s) with text (titles, abstracts,
  keywords), passed to
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md).

- nb_terms_label:

  For `"tf-idf"`, the number of terms in each label (3 by default),
  separated by commas. A term that is part of a term already in the
  label, or contains one, is skipped ("business" after "business
  cycle").

- prompt_columns:

  For `"llm"` (required), character vector, the node column(s) written
  in the description of each cluster sent to the model: what the model
  knows about the nodes (e.g.
  `c("source_title", "source_year", "cluster_label")`). A column with
  one value per cluster (a label computed before, e.g. with
  `method = "tf-idf"`) is given once for the cluster.

- nb_nodes:

  For `"llm"` (required), whole number, the number of nodes listed for
  each cluster, highest `order_by` first. `0` lists none: the model then
  reads only the `context` and the columns with one value per cluster.

- llm:

  For `"llm"`, the model: a chat object of
  [`ellmer`](https://ellmer.tidyverse.org), configured as you would for
  any other use, e.g.
  `ellmer::chat_ollama(model = "mistral", params = ellmer::params(temperature = 0, seed = 123))`
  (see the details). Not needed with `preview = TRUE`.

- instructions:

  For `"llm"`, character scalar, the system prompt sent before the
  description of each cluster. `NULL` (default) uses the instructions
  given in the details. Change them to ask for longer labels, another
  language, another vocabulary... To start from the default, get it with
  `preview = TRUE` (column `instructions`), e.g.
  `instructions = paste(prompts$instructions[1], "Answer in French.")`.

- context:

  For `"llm"`, optional character scalar describing the network to the
  model: what the nodes are, the field, the period, the question (e.g.
  `"Economics articles on stagflation, 1970-2010, linked by their shared references"`),
  written at the top of each description. Strongly recommended: in our
  tests on three corpora and three local models, it raised the quality
  of the labels more than any other change to the description (see the
  details).

- preview:

  For `"llm"`, logical. If `TRUE`, returns what the model would receive,
  without calling it (no model needed): a tibble with one row per
  cluster, its identifier, the `instructions` (the system prompt) and
  the `description` of the cluster (the user message), plus a `graph`
  column for a list of graphs named separately. Print a description with
  [`cat()`](https://rdrr.io/r/base/cat.html). `FALSE` (default) names
  the clusters.

- name_merged_clusters:

  **\[deprecated\]** Former name of `name_dynamic_clusters`.

- ...:

  For `"tf-idf"`, other arguments of
  [`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
  (e.g. `clean_word_method`, `n_gram`, `ngrams_filter`). Must be empty
  for the other methods (a misspelt argument is an error).

## Value

The same object as `graphs`, with a new node column `label_name`, also
joined to the edges through their cluster column. With `method = "llm"`
and `preview = TRUE`, a tibble of the messages that would be sent to the
model (see `preview`).

## Details

**tf-idf.** Each cluster is treated as one document made of the texts of
its nodes, and each term \\t\\ (word or n-gram) gets the weight
\$\$\mathrm{tf}\_{tc} \times \mathrm{idf}\_t = \frac{n\_{tc}}{\sum_u
n\_{uc}} \times \log\frac{C}{\mathrm{df}\_t}\$\$ (Spärck Jones 1972;
Salton and Buckley 1988) , with \\n\_{tc}\\ the number of occurrences of
\\t\\ in cluster \\c\\, \\C\\ the number of clusters and
\\\mathrm{df}\_t\\ the number of clusters where \\t\\ appears. The label
gathers the terms with the highest weight: frequent in the cluster, rare
in the others. With `name_dynamic_clusters = FALSE`, the comparison is
made within each graph, so clusters of different windows may get the
same label.

**Most central node.** `"tidygraph_functions"` computes the centrality
given in `tidygraph_function` on each graph (it is added as a node
column) and labels each cluster with the node with the highest value.
The centrality is computed on the whole graph, not within the cluster.
This method needs a network structure and therefore cannot be used with
`name_dynamic_clusters = TRUE`.

**Given column.** `"given_column"` labels each cluster with its node
with the highest `order_by` value: for an article network, typically its
most cited article.

**Language model.** `"llm"` writes a description of each cluster and
asks the model given in `llm` for a label. The model is a chat object of
the [`ellmer`](https://ellmer.tidyverse.org) package (only needed for
this method), so any model ellmer reaches can be used:

- a model running on your computer, so that the content of the nodes
  stays on it:
  [`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html)
  ([Ollama](https://ollama.com)), `chat_lmstudio()`, `chat_vllm()`, or
  `chat_openai_compatible()` for another local server (e.g. llama.cpp);

- an online service (`chat_openai()`, `chat_anthropic()`...): the
  descriptions of the clusters are then sent to it.

Configure the chat object as for any other use of ellmer; for more
stable labels, set
`params = ellmer::params(temperature = 0, seed = 123)` (this does not
guarantee the same labels on another computer, and some online services
ignore `seed`). Each cluster is sent alone to a copy of `llm` (its
earlier conversation is not sent), with `instructions` as the system
prompt, and the answer is constrained to one label
(`chat_structured()`). The label is cleaned: no quotes or final period,
words separated by spaces ("Asset_Pricing" becomes "Asset Pricing"). The
default instructions are:

    `r llm_default_instructions`

**Writing the prompt.** We compared nine prompts, with three local
models (`gemma3:4b`, `gemma3:12b`, `qwen3:14b`) on 21 clusters of three
corpora (articles of economics), the labels being scored blind by
another model (accuracy and specificity, from 1 to 5; a small test, to
be taken as an indication). What mattered most, in order:

1.  The instructions. Short ones ("a label of 2 or 3 words") give
    generic labels ("Monetary policy analysis"); the default above,
    which asks for the specific topic, forbids empty words and gives
    examples, raised the scores by 0.5 to 0.8 points whatever the model.

2.  The `context`: one sentence on what the nodes are, the field and the
    period raised the scores by a further 0.25 to 0.5 points. It helps
    the model most where it cannot tell the cluster apart by itself.

3.  Little else: 10 nodes are enough (25 did not help, 5 was a bit
    worse); year and journal in `prompt_columns` changed little; and a
    column of key terms (tf-idf) did not help and produced more repeated
    labels. A larger model did not systematically beat a small one once
    the prompt was good. If you change `instructions`, keep the rules
    asking for a specific topic and keep the `context`.

With
[`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html),
a model that thinks before answering (e.g. `qwen3`) can take minutes per
cluster or time out, because ellmer reaches Ollama through its
OpenAI-compatible endpoint, where `think = FALSE` is ignored: use
`api_args = list(reasoning_effort = "none")`.

The description of a cluster gathers, when available:

- the `context` you give;

- the number of nodes of the cluster (and of windows, for dynamic
  clusters);

- the columns of `prompt_columns` with one value per cluster (e.g. a
  tf-idf label computed before with `method = "tf-idf"`), given once;

- its `nb_nodes` main nodes (highest `order_by`), described by the other
  columns of `prompt_columns`.

Everything comes from the node tables of `graphs`. The value of a node
longer than 300 characters (an abstract) is cut, and its line breaks are
removed; columns with one value per cluster are written in full. Use
`preview = TRUE` to read the descriptions before running the model.

If the model fails on the first cluster, `name_clusters()` stops: the
problem is likely its configuration (try `llm$chat("Hello")`). A later
cluster whose label fails is labelled "no_name" with a warning. By
default, each answer may take up to 300 seconds, with up to 3 tries (the
options `ellmer_timeout_s` and `ellmer_max_tries` of ellmer): raise
`options(ellmer_timeout_s = )` for a slow model. A warning is given when
a description exceeds about 4000 tokens: some local servers cut longer
prompts without warning (Ollama has a context of 4096 tokens by
default).

Nodes of an unnamed cluster get "no_name".

## References

Ding Y (2011). “Community Detection: Topological vs. Topical.” *Journal
of Informetrics*, **5**(4), 498–514.
[doi:10.1016/j.joi.2011.02.006](https://doi.org/10.1016/j.joi.2011.02.006)
.  
  
Salton G, Buckley C (1988). “Term-Weighting Approaches in Automatic Text
Retrieval.” *Information Processing & Management*, **24**(5), 513–523.  
  
Spärck Jones K (1972). “A Statistical Interpretation of Term Specificity
and Its Application in Retrieval.” *Journal of Documentation*,
**28**(1), 11–21.

## See also

[`extract_tfidf()`](https://agoutsmedt.github.io/networkflow/reference/extract_tfidf.md)
to see more terms than the labels show.

## Examples

``` r
network <- build_network(
  nodes = subset(Nodes_stagflation, source_type == "Stagflation"),
  directed_edges = Ref_stagflation,
  source_id = "source_id",
  target_id = "target_id",
  method = "similarity",
  edges_threshold = 1
)
#> ℹ Coupling (`biblionetwork::biblio_coupling()`) with "similarity": ties
#>   weighted by the idf-weighted cosine of the two lists of targets, after Shen
#>   et al. (2019), between source_id entities sharing target_id entities.
#> ℹ Two source_id are linked when they share at least 1 target_id
#>   (`edges_threshold`).
#> ℹ Nodes without any edge are removed (`keep_singleton = FALSE`).
network <- add_clusters(
  network,
  clustering_method = "leiden",
  objective_function = "modularity",
  seed = 123,
  verbose = FALSE
)

# The words of the titles that distinguish each cluster
network <- name_clusters(
  network,
  method = "tf-idf",
  cluster_id = "cluster_leiden",
  text_columns = "source_title",
  nb_terms_label = 3,
  clean_word_method = "none"
)

# The article with the highest PageRank of each cluster
network <- name_clusters(
  network,
  method = "tidygraph_functions",
  cluster_id = "cluster_leiden",
  label_columns = c("source_author", "source_year"),
  label_name = "central_article",
  tidygraph_function = tidygraph::centrality_pagerank()
)
unique(tidygraph::as_tibble(network, active = "nodes")[
  c("cluster_leiden", "cluster_label", "central_article")
])
#> # A tibble: 4 × 3
#>   cluster_leiden cluster_label                            central_article
#>   <chr>          <chr>                                    <chr>          
#> 1 02             great inflation, stabilization, monetary VELDE-F_2004   
#> 2 01             productivity, unemployment, prices       WACHTER-M_1978 
#> 3 04             stagflation, inflation, 1970s            DELONG-B_1997  
#> 4 03             oil, great stagflation, price shocks     BARSKY-R_2002  

# What a language model would read for each cluster (no model needed):
# titles, years and the tf-idf label, for the 10 nodes most tied to their
# own cluster
network <- add_node_roles(network, cluster_id = "cluster_leiden")
prompts <- name_clusters(
  network,
  method = "llm",
  cluster_id = "cluster_leiden",
  prompt_columns = c("source_title", "source_year", "cluster_label"),
  order_by = "within_module_degree",
  nb_nodes = 10,
  context = "Economics articles on stagflation, linked by their shared references.",
  preview = TRUE
)
cat(prompts$instructions[1])
#> You label the clusters of a network for a researcher.
#> A cluster is a group of nodes (e.g. documents) more linked to each other than to the rest of the network.
#> You receive the description of one cluster, with its most central nodes.
#> 
#> Rules:
#> - Name the topic the nodes share, in 2 to 4 words of ordinary language, e.g. "Protein folding", "Urban heat islands", "Medieval trade routes".
#> - Be as specific as the information allows: prefer "Coral bleaching" to "Marine biology". Avoid empty words (studies, analysis, issues, research) and the name of a whole field when the nodes share a narrower topic.
#> - Base the label on what most nodes have in common, not on one odd node; if the topics are mixed, name the dominant one.
#> - Use only the information given.
#> - Answer with the label alone: no quotes, no final period.
cat(prompts$description[1])
#> Context of the network: Economics articles on stagflation, linked by their shared references.
#> 
#> Cluster "01": 69 nodes.
#> cluster_label: productivity, unemployment, prices
#> 
#> Main nodes (highest within_module_degree first; source_title | source_year):
#> - ENERGY PRICE INCREASES AND MACROECONOMIC POLICY | 1980
#> - THE VARIANCE AND ACCELERATION OF INFLATION IN THE 1970S: ALTERNATIVE EXPLANATORY MODELS AND METHODS | 1980
#> - MACRO POLICY RESPONSES TO PRICE SHOCKS | 1979
#> - INFLATION, FLEXIBLE EXCHANGE RATES, AND THE NATURAL RATE OF UNEMPLOYMENT | 1981
#> - THE REAL PRICE OF OIL AND THE 1970S WORLD INFLATION | 1981
#> - THE INFLATIONARY IMPACT OF HIGHER ENERGY PRICES 1973-1975 | 1978
#> - INFLATION IN THEORY AND PRACTICE | 1980
#> - INSTITUTIONAL FACTORS IN DOMESTIC INFLATION | 1978
#> - THE 1971-1974 CONTROLS PROGRAM AND THE PRICE LEVEL: AN ECONOMETRIC POST-MORTEM | 1981
#> - COMMENT ON <93>THE ACCELERATION OF INFLATION<94> | 1979

if (FALSE) { # \dontrun{
# A label written by a model running on your computer, here with Ollama
# (install Ollama, then `ollama pull mistral` in a terminal)
network <- name_clusters(
  network,
  method = "llm",
  cluster_id = "cluster_leiden",
  prompt_columns = c("source_title", "source_year", "cluster_label"),
  order_by = "within_module_degree",
  nb_nodes = 10,
  label_name = "llm_label",
  llm = ellmer::chat_ollama(model = "mistral",
                            params = ellmer::params(temperature = 0, seed = 123)),
  context = "Economics articles on stagflation, linked by their shared references."
)
} # }
```
