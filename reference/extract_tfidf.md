# Extracting TF-IDF Values for Ngrams

**\[experimental\]**

This function takes as input a tibble graph (from
[tidygraph](https://tidygraph.data-imaginist.com/)), a list of tibble
graphs or a data frame, extract the ngrams from the text column(s) of
your choice, and calculates the Term-Frequency
Inverse-Document-Frequency value of each ngram for each grouping
variables you have chosen.

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

  A tibble graph from
  [tidygraph](https://tidygraph.data-imaginist.com/), a list of tibble
  graphs or a data frame.

- text_columns:

  The columns with the text you want to analyze. If you give multiple
  columns, they will be united to extract the terms.

- grouping_columns:

  The column(s) you want to use to calculate the tf-idf. These columns
  will become your "document" unit in the
  [`tidytext::bind_tf_idf()`](https://juliasilge.github.io/tidytext/reference/bind_tf_idf.html)
  function. For instance, if you run the function on a unique tibble
  graph, you may want to compute the tf-idf depending on the clusters
  your nodes are belonging. You have to take care that the identifier of
  the variable you are using to compute the tf-idf is unique for each
  group (see the details for more information).

- grouping_across_list:

  Set to `TRUE` if you want to compute tf-idf on the whole list of
  tibble graphs and that you have no unique identifier for them (see the
  details for more information).

- n_gram:

  The maximum n you want for tokenizing your ngrams (see
  [`tidytext::unnest_tokens()`](https://juliasilge.github.io/tidytext/reference/unnest_tokens.html)
  for more information). 2 by default, i.e. only unigrams and bigrams
  will be extracted.

- stopwords_type:

  The type of stopwords list you want to use to remove stopwords from
  your ngrams. The "smart" list is chosen by default, but see other
  possilities with
  [stopwords::stopwords_getsources](https://rdrr.io/pkg/stopwords/man/stopwords_getsources.html).

- stopwords_vector:

  Use your own stopwords list, in a vector of strings format.

- clean_word_method:

  Choose the method to clean and standardized your ngrams. You can
  lemmatize or stem words through the [textstem
  package](https://github.com/trinker/textstem). Choose "none" if you
  don't want to apply any cleaning method.

- ngrams_filter:

  You can exclude from tf-idf computation the ngrams that does not
  appear a certain number of time in the whole corpus.

- nb_terms:

  The functions extracts the `nb_terms` (5 by default) highest TF-IDF
  for each grouping variables.

## Value

A data.table with the terms (i.e. ngrams) appearing in each "document"
(that is your `grouping_columns`) with the number of time they appear
per document (`n`), their term frequency (`tf`), their inverse document
frequency (`idf`), and their term-frequency inverse-document-frequency
(`tf_idf`). The terms are those with the highest `tf_idf` value for each
value of the grouping columns, depending on the `nb_words` value you
set. For instance, if `nb_words` is set to 5 (default valuet), and that
you compute the TF-IDF on the cluster variable, the function extracts
the 5 terms with the highest TF-IDF value for each cluster.

## Details

This functions extract TF-IDF values for various types of input, from
multiple text columns and with grouping of multiple columns. The most
simple case is to use this function with a data frame or a unique tibble
graph with an easily identifiable grouping variable (like a cluster).
But it also allows more complex uses in the case of a list of tibble
graphs.

If you enter as an input a list of tibble graphs, the function extracts
TF-IDF on the binded graphs, and not graph after graph. If your want to
extract TF-IDF for each graphs separately, then use
[`lapply()`](https://rdrr.io/r/base/lapply.html) and apply
`extract_tfidf()` for each graph: the input will be a unique tibble
graph, and the operation will be repeated for each tibble graphs of your
list.

As the extraction of TF-IDF is made on the whole aggregated list, you
have to choose carefully your `grouping_columns`. Indeed, your grouping
columns must identify variables that are unique. For instance, in the
case you have used
[`add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md),
each node in each of your graph is associated to a cluster. But the
identifier of the clusters ("01", "02", "03", etc.) are the same across
tibble graphs. It means that all the "01" clusters will be grouped
together, and it is something you don't want. In this case, set
`grouping_across_list` to `TRUE`: the identifier of the cluster will be
merged with the name of the corresponding tibble_graph in the list.
However, you don't need to use this possibility if you have a unique
identifier across your tibble graphs. That is the case, for instance, if
you have use
[`merge_dynamic_clusters()`](https://agoutsmedt.github.io/networkflow/reference/merge_dynamic_clusters.md),
you have a column of clusters merged across your different tibble
graphs. These new inter-networks clusters constitute a unique
identifier.

TF-IDF are calculated from the number of occurrence of a term in each
document. The terms which occur only once are removed to avoid too rare
terms to appear at the top of your grouping variables.

## Examples

``` r
nodes <- networkflow::Nodes_stagflation |>
  dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

temporal_networks <- build_dynamic_networks(nodes = nodes,
directed_edges = references,
source_id = "source_id",
target_id = "target_id",
time_variable = "source_year",
cooccurrence_method = "coupling_similarity",
time_window = 10,
edges_threshold = 1,
overlapping_window = TRUE,
filter_components = TRUE)
#> ℹ Backbone method selected: structured
#> ℹ Keep_singleton == FALSE: removing the nodes that are alone with no edge. 
#> 
#> ── Generation of the network for the 1975-1984 time window. ────────────────────
#> 
#> ── Generation of the network for the 1976-1985 time window. ────────────────────
#> 
#> ── Generation of the network for the 1977-1986 time window. ────────────────────
#> 
#> ── Generation of the network for the 1978-1987 time window. ────────────────────
#> 
#> ── Generation of the network for the 1979-1988 time window. ────────────────────
#> 
#> ── Generation of the network for the 1980-1989 time window. ────────────────────
#> 
#> ── Generation of the network for the 1981-1990 time window. ────────────────────
#> 
#> ── Generation of the network for the 1982-1991 time window. ────────────────────
#> 
#> ── Generation of the network for the 1983-1992 time window. ────────────────────
#> 
#> ── Generation of the network for the 1984-1993 time window. ────────────────────
#> 
#> ── Generation of the network for the 1985-1994 time window. ────────────────────
#> 
#> ── Generation of the network for the 1986-1995 time window. ────────────────────
#> 
#> ── Generation of the network for the 1987-1996 time window. ────────────────────
#> 
#> ── Generation of the network for the 1988-1997 time window. ────────────────────
#> 
#> ── Generation of the network for the 1989-1998 time window. ────────────────────
#> 
#> ── Generation of the network for the 1990-1999 time window. ────────────────────
#> 
#> ── Generation of the network for the 1991-2000 time window. ────────────────────
#> 
#> ── Generation of the network for the 1992-2001 time window. ────────────────────
#> 
#> ── Generation of the network for the 1993-2002 time window. ────────────────────
#> 
#> ── Generation of the network for the 1994-2003 time window. ────────────────────
#> 
#> ── Generation of the network for the 1995-2004 time window. ────────────────────
#> 
#> ── Generation of the network for the 1996-2005 time window. ────────────────────
#> 
#> ── Generation of the network for the 1997-2006 time window. ────────────────────
#> 
#> ── Generation of the network for the 1998-2007 time window. ────────────────────
#> 
#> ── Generation of the network for the 1999-2008 time window. ────────────────────
#> 
#> ── Generation of the network for the 2000-2009 time window. ────────────────────
#> 
#> ── Generation of the network for the 2001-2010 time window. ────────────────────
#> 
#> ── Generation of the network for the 2002-2011 time window. ────────────────────
#> 
#> ── Generation of the network for the 2003-2012 time window. ────────────────────
#> 
#> ── Generation of the network for the 2004-2013 time window. ────────────────────

temporal_networks <- add_clusters(temporal_networks,
objective_function = "modularity",
clustering_method = "leiden")
#> 
#> ── Cluster detection for the "1975-1984" period ────────────────────────────────
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "39.1%" of the network.
#> 
#> ── Cluster detection for the "1976-1985" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "24.6%" of the network.
#> 
#> ── Cluster detection for the "1977-1986" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "26.2%" of the network.
#> 
#> ── Cluster detection for the "1978-1987" period ────────────────────────────────
#> ℹ The leiden method detected 6 clusters. The biggest cluster represents "28.6%" of the network.
#> 
#> ── Cluster detection for the "1979-1988" period ────────────────────────────────
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "35.4%" of the network.
#> 
#> ── Cluster detection for the "1980-1989" period ────────────────────────────────
#> ℹ The leiden method detected 5 clusters. The biggest cluster represents "40%" of the network.
#> 
#> ── Cluster detection for the "1981-1990" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "37%" of the network.
#> 
#> ── Cluster detection for the "1982-1991" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "38.1%" of the network.
#> 
#> ── Cluster detection for the "1983-1992" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "42.9%" of the network.
#> 
#> ── Cluster detection for the "1984-1993" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "40%" of the network.
#> 
#> ── Cluster detection for the "1985-1994" period ────────────────────────────────
#> ℹ The leiden method detected 2 clusters. The biggest cluster represents "57.1%" of the network.
#> 
#> ── Cluster detection for the "1986-1995" period ────────────────────────────────
#> ℹ The leiden method detected 2 clusters. The biggest cluster represents "57.1%" of the network.
#> 
#> ── Cluster detection for the "1987-1996" period ────────────────────────────────
#> ℹ The leiden method detected 2 clusters. The biggest cluster represents "62.5%" of the network.
#> 
#> ── Cluster detection for the "1988-1997" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "36.4%" of the network.
#> 
#> ── Cluster detection for the "1989-1998" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "45.5%" of the network.
#> 
#> ── Cluster detection for the "1990-1999" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "46.2%" of the network.
#> 
#> ── Cluster detection for the "1991-2000" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "38.9%" of the network.
#> 
#> ── Cluster detection for the "1992-2001" period ────────────────────────────────
#> ℹ The leiden method detected 4 clusters. The biggest cluster represents "35%" of the network.
#> 
#> ── Cluster detection for the "1993-2002" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "40.7%" of the network.
#> 
#> ── Cluster detection for the "1994-2003" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "38.7%" of the network.
#> 
#> ── Cluster detection for the "1995-2004" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "38.9%" of the network.
#> 
#> ── Cluster detection for the "1996-2005" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "40.5%" of the network.
#> 
#> ── Cluster detection for the "1997-2006" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "40%" of the network.
#> 
#> ── Cluster detection for the "1998-2007" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "39.6%" of the network.
#> 
#> ── Cluster detection for the "1999-2008" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "39.1%" of the network.
#> 
#> ── Cluster detection for the "2000-2009" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "49%" of the network.
#> 
#> ── Cluster detection for the "2001-2010" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "45.7%" of the network.
#> 
#> ── Cluster detection for the "2002-2011" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "50%" of the network.
#> 
#> ── Cluster detection for the "2003-2012" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "55.3%" of the network.
#> 
#> ── Cluster detection for the "2004-2013" period ────────────────────────────────
#> ℹ The leiden method detected 3 clusters. The biggest cluster represents "54.8%" of the network.

library(stopwords)
tfidf <- extract_tfidf(temporal_networks,
n_gram = 4,
text_columns = "source_title",
grouping_columns = "cluster_leiden",
grouping_across_list = TRUE,
clean_word_method = "lemmatize")
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 4 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.

tfidf[[1]]
#>   [1] "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984"
#>   [7] "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984"
#>  [13] "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984" "1975-1984"
#>  [19] "1975-1984" "1975-1984" "1975-1984" "1976-1985" "1976-1985" "1976-1985"
#>  [25] "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985"
#>  [31] "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985"
#>  [37] "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985"
#>  [43] "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1976-1985" "1977-1986"
#>  [49] "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986"
#>  [55] "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986"
#>  [61] "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986"
#>  [67] "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986" "1977-1986"
#>  [73] "1977-1986" "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987"
#>  [79] "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987"
#>  [85] "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987"
#>  [91] "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987" "1978-1987"
#>  [97] "1978-1987" "1978-1987" "1978-1987" "1979-1988" "1979-1988" "1979-1988"
#> [103] "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988"
#> [109] "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988"
#> [115] "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988" "1979-1988"
#> [121] "1979-1988" "1979-1988" "1979-1988" "1980-1989" "1980-1989" "1980-1989"
#> [127] "1980-1989" "1980-1989" "1980-1989" "1980-1989" "1980-1989" "1980-1989"
#> [133] "1980-1989" "1980-1989" "1980-1989" "1980-1989" "1980-1989" "1980-1989"
#> [139] "1980-1989" "1981-1990" "1981-1990" "1981-1990" "1981-1990" "1981-1990"
#> [145] "1981-1990" "1981-1990" "1981-1990" "1981-1990" "1981-1990" "1981-1990"
#> [151] "1981-1990" "1981-1990" "1981-1990" "1981-1990" "1981-1990" "1981-1990"
#> [157] "1981-1990" "1982-1991" "1982-1991" "1982-1991" "1982-1991" "1982-1991"
#> [163] "1982-1991" "1982-1991" "1982-1991" "1982-1991" "1982-1991" "1982-1991"
#> [169] "1982-1991" "1982-1991" "1983-1992" "1983-1992" "1983-1992" "1983-1992"
#> [175] "1983-1992" "1983-1992" "1983-1992" "1983-1992" "1983-1992" "1983-1992"
#> [181] "1983-1992" "1983-1992" "1984-1993" "1984-1993" "1984-1993" "1984-1993"
#> [187] "1984-1993" "1984-1993" "1984-1993" "1984-1993" "1984-1993" "1984-1993"
#> [193] "1984-1993" "1985-1994" "1985-1994" "1985-1994" "1985-1994" "1986-1995"
#> [199] "1986-1995" "1986-1995" "1986-1995" "1987-1996" "1987-1996" "1987-1996"
#> [205] "1987-1996" "1987-1996" "1988-1997" "1988-1997" "1988-1997" "1988-1997"
#> [211] "1988-1997" "1988-1997" "1988-1997" "1988-1997" "1988-1997" "1989-1998"
#> [217] "1989-1998" "1989-1998" "1989-1998" "1989-1998" "1989-1998" "1989-1998"
#> [223] "1989-1998" "1989-1998" "1990-1999" "1990-1999" "1990-1999" "1990-1999"
#> [229] "1990-1999" "1990-1999" "1990-1999" "1990-1999" "1990-1999" "1991-2000"
#> [235] "1991-2000" "1991-2000" "1991-2000" "1991-2000" "1991-2000" "1991-2000"
#> [241] "1991-2000" "1991-2000" "1991-2000" "1991-2000" "1991-2000" "1992-2001"
#> [247] "1992-2001" "1992-2001" "1992-2001" "1992-2001" "1992-2001" "1992-2001"
#> [253] "1992-2001" "1992-2001" "1992-2001" "1992-2001" "1992-2001" "1992-2001"
#> [259] "1992-2001" "1992-2001" "1992-2001" "1992-2001" "1993-2002" "1993-2002"
#> [265] "1993-2002" "1993-2002" "1993-2002" "1993-2002" "1993-2002" "1993-2002"
#> [271] "1993-2002" "1993-2002" "1993-2002" "1993-2002" "1993-2002" "1993-2002"
#> [277] "1993-2002" "1994-2003" "1994-2003" "1994-2003" "1994-2003" "1994-2003"
#> [283] "1994-2003" "1994-2003" "1994-2003" "1994-2003" "1994-2003" "1994-2003"
#> [289] "1994-2003" "1994-2003" "1994-2003" "1994-2003" "1995-2004" "1995-2004"
#> [295] "1995-2004" "1995-2004" "1995-2004" "1995-2004" "1995-2004" "1995-2004"
#> [301] "1995-2004" "1995-2004" "1995-2004" "1995-2004" "1995-2004" "1995-2004"
#> [307] "1995-2004" "1996-2005" "1996-2005" "1996-2005" "1996-2005" "1996-2005"
#> [313] "1996-2005" "1996-2005" "1996-2005" "1996-2005" "1996-2005" "1996-2005"
#> [319] "1996-2005" "1996-2005" "1996-2005" "1996-2005" "1997-2006" "1997-2006"
#> [325] "1997-2006" "1997-2006" "1997-2006" "1997-2006" "1997-2006" "1997-2006"
#> [331] "1997-2006" "1997-2006" "1997-2006" "1997-2006" "1997-2006" "1997-2006"
#> [337] "1997-2006" "1998-2007" "1998-2007" "1998-2007" "1998-2007" "1998-2007"
#> [343] "1998-2007" "1998-2007" "1998-2007" "1998-2007" "1998-2007" "1998-2007"
#> [349] "1998-2007" "1998-2007" "1998-2007" "1998-2007" "1999-2008" "1999-2008"
#> [355] "1999-2008" "1999-2008" "1999-2008" "1999-2008" "1999-2008" "1999-2008"
#> [361] "1999-2008" "1999-2008" "1999-2008" "1999-2008" "1999-2008" "1999-2008"
#> [367] "1999-2008" "2000-2009" "2000-2009" "2000-2009" "2000-2009" "2000-2009"
#> [373] "2000-2009" "2000-2009" "2000-2009" "2000-2009" "2000-2009" "2000-2009"
#> [379] "2000-2009" "2000-2009" "2000-2009" "2000-2009" "2001-2010" "2001-2010"
#> [385] "2001-2010" "2001-2010" "2001-2010" "2001-2010" "2001-2010" "2001-2010"
#> [391] "2001-2010" "2001-2010" "2001-2010" "2001-2010" "2001-2010" "2001-2010"
#> [397] "2001-2010" "2002-2011" "2002-2011" "2002-2011" "2002-2011" "2002-2011"
#> [403] "2002-2011" "2002-2011" "2002-2011" "2002-2011" "2002-2011" "2002-2011"
#> [409] "2002-2011" "2002-2011" "2002-2011" "2002-2011" "2003-2012" "2003-2012"
#> [415] "2003-2012" "2003-2012" "2003-2012" "2003-2012" "2003-2012" "2003-2012"
#> [421] "2003-2012" "2003-2012" "2003-2012" "2003-2012" "2003-2012" "2003-2012"
#> [427] "2003-2012" "2004-2013" "2004-2013" "2004-2013" "2004-2013" "2004-2013"
#> [433] "2004-2013" "2004-2013" "2004-2013" "2004-2013" "2004-2013" "2004-2013"
#> [439] "2004-2013" "2004-2013" "2004-2013" "2004-2013"
```
