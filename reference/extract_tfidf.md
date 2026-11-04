# Find the terms that distinguish groups of nodes (tf-idf)

**\[experimental\]**

Clusters are defined by the structure of the network, not by the content
of the nodes. To interpret them, it helps to know which words are
specific to each cluster. `extract_tfidf()` extracts the words and
n-grams of one or several text columns (e.g. titles, abstracts) and
returns, for each group (e.g. each cluster), the terms with the highest
tf-idf (Spärck Jones 1972; Salton and Buckley 1988) :
\$\$\mathrm{tf}\_{td} = \frac{n\_{td}}{\sum_u n\_{ud}}, \qquad
\mathrm{idf}\_t = \log\frac{D}{\mathrm{df}\_t}, \qquad
\text{tf-idf}\_{td} = \mathrm{tf}\_{td} \times \mathrm{idf}\_t,\$\$ with
\\n\_{td}\\ the count of term \\t\\ in group \\d\\, \\D\\ the number of
groups and \\\mathrm{df}\_t\\ the number of groups containing \\t\\. A
term present in every group has a tf-idf of 0.

## Usage

``` r
extract_tfidf(
  data,
  text_columns,
  grouping_columns,
  grouping_across_list = FALSE,
  n_gram = 2L,
  stopwords_type = "smart",
  stopwords_vector = NULL,
  clean_word_method = c("lemmatize", "stemming", "none"),
  ngrams_filter = 5L,
  nb_terms = 5L
)
```

## Arguments

- data:

  A `tbl_graph`, a list of `tbl_graph`s (their node tables are stacked)
  or a data frame.

- text_columns:

  Character vector, the column(s) with text. Several columns are pasted
  together, separated by ". ".

- grouping_columns:

  Character vector, the column(s) defining the groups, the "documents"
  of the tf-idf (e.g. `"cluster_leiden"`).

- grouping_across_list:

  Logical. For a list of graphs whose group identifiers repeat across
  graphs (cluster "01" exists in every window), `TRUE` adds the name of
  the graph to the groups, so that each cluster of each window is a
  separate document. Not needed for identifiers that are already unique
  across the list, such as dynamic clusters.

- n_gram:

  Integer, the maximum length of n-grams (2 by default: words and pairs
  of words).

- stopwords_type:

  Character scalar, the stop word list of
  [`tidytext::get_stopwords()`](https://juliasilge.github.io/tidytext/reference/get_stopwords.html)
  (`"smart"` by default).

- stopwords_vector:

  Optional character vector of stop words, used instead of
  `stopwords_type`.

- clean_word_method:

  `"lemmatize"`, `"stemming"` (with the
  [`textstem`](https://github.com/trinker/textstem) package) or `"none"`
  (chosen when no method is given).

- ngrams_filter:

  Integer, the minimum number of occurrences of a term in the whole
  corpus (5 by default).

- nb_terms:

  Integer, the number of terms returned per group (5 by default).

## Value

A `data.table` with one row per group and term, for the `nb_terms` best
terms of each group: the grouping columns, `term`, `n` (count of the
term in the group), `tf`, `idf` and `tf_idf`.

## Details

The texts are lower-cased and split into n-grams of 1 to `n_gram` words
by
[`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html)
(Silge and Robinson 2017) . Each word is lemmatised or stemmed if
requested; n-grams containing a stop word or a number are dropped, as
are terms rarer than `ngrams_filter` in the corpus and terms appearing
only once in a group, which would otherwise reach the top of small
groups by accident.

The choice of the groups decides what "specific" means: computed on the
clusters of one network, tf-idf finds what distinguishes a cluster from
the other clusters of the same period; computed on the stacked windows
with `grouping_across_list = TRUE`, it also distinguishes a cluster from
its own past and future.

Lemmatising and stemming are optional and need the `textstem` package.
For larger corpora or other weighting schemes, the text-mining functions
of the mintR package (<https://codeberg.org/agoutsmedt/mintR>) can be
used on the node table.

## References

Salton G, Buckley C (1988). “Term-Weighting Approaches in Automatic Text
Retrieval.” *Information Processing & Management*, **24**(5), 513–523.  
  
Silge J, Robinson D (2017). *Text Mining with R: A Tidy Approach*.
O'Reilly.  
  
Spärck Jones K (1972). “A Statistical Interpretation of Term Specificity
and Its Application in Retrieval.” *Journal of Documentation*,
**28**(1), 11–21.

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

extract_tfidf(
  network,
  text_columns = "source_title",
  grouping_columns = "cluster_leiden",
  n_gram = 2,
  clean_word_method = "none",
  nb_terms = 3
)
#>     cluster_leiden                 term     n         tf       idf     tf_idf
#>             <char>               <char> <int>      <num>     <num>      <num>
#>  1:             01         productivity     8 0.04705882 1.3862944 0.06523738
#>  2:             01         unemployment     7 0.04117647 1.3862944 0.05708271
#>  3:             01               prices     7 0.04117647 1.3862944 0.05708271
#>  4:             02      great inflation    21 0.11290323 1.3862944 0.15651711
#>  5:             02        stabilization     5 0.02688172 1.3862944 0.03726598
#>  6:             02 stabilization policy     5 0.02688172 1.3862944 0.03726598
#>  7:             03                  oil    13 0.14942529 0.6931472 0.10357372
#>  8:             03            oil price     5 0.05747126 1.3862944 0.07967209
#>  9:             03    great stagflation     4 0.04597701 0.6931472 0.03186884
#> 10:             04          stagflation     2 0.12500000 0.2876821 0.03596026
#> 11:             04            inflation     5 0.31250000 0.0000000 0.00000000
#> 12:             04                1970s     2 0.12500000 0.0000000 0.00000000
```
