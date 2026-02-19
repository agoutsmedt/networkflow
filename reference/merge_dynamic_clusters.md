# Find Similar Clusters across Multiple Temporal Networks

**\[experimental\]**

This function creates a new column "intertemporal_name" for each network
from a list of temporal networks to identify similar clusters across
time. The function gives the same name to two clusters from two
succesive temporal networks if they match the conditions defined by the
user: `threshold_similarity`, `cluster_colum` and `similarity_type`.

## Usage

``` r
merge_dynamic_clusters(
  list_graph,
  cluster_id,
  node_id,
  threshold_similarity = 0.5001,
  similarity_type = c("complete", "partial")
)
```

## Arguments

- list_graph:

  A list of tibble graphs ((from
  [tidygraph](https://tidygraph.data-imaginist.com/))) The list is
  expected to be ordered in a sequential order from the oldest to the
  most recent network.

- cluster_id:

  The column with the identifier of the cluster. If you have used
  [add_clusters()](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md),
  it is of the form `cluster_{clustering_method}`.

- node_id:

  The column with the unique identifier of each node.

- threshold_similarity:

  The threshold_similarity variable defines how sensitive the function
  is to giving the same name to two clusters. A higher threshold will
  lead to more communities.

  For example, if you have two temporal networks with two communities
  each. Communities A and B for the older network, and communities A'
  and B' for the more recent network. A threshold of 0.51 with a
  "complete" similarity_type means that community A' will be given the
  name A if 51% of the nodes from A' in the more recent network
  originate from A in the older network, and 51% of the node from A in
  the older network becomes in A' in the more recent network.

- similarity_type:

  Choose a similarity type to compare the threshold to:

  - "complete" similarity compute the share of nodes going from an older
    community to a more recent community on all the nodes in both
    networks

  - "partial" similarity compute the share of nodes going from an older
    community to a more recent community only on nodes that exists in
    both networks

  Complete similarity is particularly suited if the number of nodes in
  your networks is relatively stable over time as the threshold capture
  the share of all nodes moving between clusters. Partial similarity can
  be particularly useful when the number of nodes in your networks
  increases rapidly. The interpretation of the threshold is that it
  captures the share of nodes existing in both networks moving between
  clusters.

  For example, with a complete similarity threshold of 0.51, if (1) all
  nodes from community A in network t-1 go into community A' in network
  t+1, and (2) all nodes in community A' present in network t-1
  originate from community A, but (3) the number of nodes in A' is more
  than twice of A because of new nodes that did not exists in t-1, A'
  will never meet the threshold requirement to be named A despite a
  strong similarity between the two clusters. Conceptually, this might
  be a desired behavior of the function because one might considered
  that A' is too different from A to be considered the same cluster as
  its composition is changed from new nodes. In that case complete
  similarity is the right choice. However, if one consider that A and A'
  are very similar because all the nodes that exists in both networks
  are identified as part of the same community, then partial threshold
  similarity is more desirable.

## Value

The function returns the same list of networks used as input in
`list_graph` but with a new column `dynamic_{cluster_id}` (i.e, the name
of the new column depends of the column that served as input). The
column is the result of the inter-graphs grouping of the original
clusters of the `cluster_id`. The dynamic clusters are also merged with
the different `cluster_id` columns of the edges data.

## Examples

``` r
library(networkflow)

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

temporal_networks <- merge_dynamic_clusters(temporal_networks,
cluster_id = "cluster_leiden",
node_id = "source_id",
threshold_similarity = 0.51,
similarity_type = "partial")

temporal_networks[[1]]
#> # A tbl_graph: 64 nodes and 375 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Edge Data: 375 × 11 (active)
#>     from    to   weight Source  Target     cluster_leiden_from cluster_leiden_to
#>    <int> <int>    <dbl> <chr>   <chr>      <chr>               <chr>            
#>  1     6    11 0.00160  1021902 1111111122 02                  02               
#>  2     6    45 0.000183 1021902 1111111128 02                  03               
#>  3     6    35 0.000751 1021902 1111111146 02                  02               
#>  4     6    20 0.000128 1021902 1111111180 02                  02               
#>  5     6    42 0.000624 1021902 1111111182 02                  02               
#>  6     6    21 0.000365 1021902 1111111183 02                  02               
#>  7     6    52 0.000274 1021902 1184127    02                  03               
#>  8     6    31 0.00124  1021902 14490177   02                  02               
#>  9     6    64 0.000278 1021902 16167977   02                  03               
#> 10     3     6 0.000173 1021902 16182201   02                  02               
#> # ℹ 365 more rows
#> # ℹ 4 more variables: cluster_leiden <chr>, dynamic_cluster_leiden_from <chr>,
#> #   dynamic_cluster_leiden_to <chr>, dynamic_cluster_leiden <chr>
#> #
#> # Node Data: 64 × 11
#>   source_id source_author source_year source_label   source_title source_journal
#>   <chr>     <chr>               <int> <chr>          <chr>        <chr>         
#> 1 16182155  GORDON-R             1975 GORDON-R-1975a ALTERNATIVE… BROOKINGS PAP…
#> 2 26283591  GORDON-R             1975 GORDON-R-1975b THE IMPACT … BROOKINGS PAP…
#> 3 16182201  OKUN-A               1975 OKUN-A-1975a   INFLATION: … BROOKINGS PAP…
#> # ℹ 61 more rows
#> # ℹ 5 more variables: source_type <chr>, time_window <chr>,
#> #   cluster_leiden <chr>, size_cluster_leiden <dbl>,
#> #   dynamic_cluster_leiden <chr>
```
