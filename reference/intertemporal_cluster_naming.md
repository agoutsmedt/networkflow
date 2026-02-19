# Find Similar Clusters across Multiple Temporal Networks

**\[deprecated\]**

This function creates a new column "intertemporal_name" for each network
from a list of temporal networks to identify similar clusters across
time. The function gives the same name to two clusters from two
succesive temporal networks if they match the conditions defined by the
user: `threshold_similarity`, `cluster_colum` and `similarity_type`.

## Usage

``` r
intertemporal_cluster_naming(
  list_graph = NA,
  cluster_column = NA,
  node_key = NA,
  threshold_similarity = 0.5001,
  similarity_type = c("complete, partial")
)
```

## Arguments

- list_graph:

  A list of networks. The list is expected to be ordered in a sequential
  order from the oldest to the most recent network.

- cluster_column:

  The column with the identifier of the cluster.

- node_key:

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

  - "complete" similarity compute the share of nodes going from a older
    community to a more recent community on all the nodes in both
    networks

  - "partial" similarity compute the share of nodes going from a older
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
`list_graph` but with a new column `intertemporal_name`. The column is
the result of the inter-temporal grouping of the original clusters of
the `cluster_column`.

## Examples

``` r
library(biblionetwork)
#> 
#> Attaching package: ‘biblionetwork’
#> The following objects are masked from ‘package:networkflow’:
#> 
#>     Authors_stagflation, Nodes_stagflation, Ref_stagflation
library(magrittr)
library(tidygraph)
#> 
#> Attaching package: ‘tidygraph’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter

nodes <- networkflow::Nodes_stagflation %>%
dplyr::filter(source_type == "Stagflation")

references <- networkflow::Ref_stagflation

temporal_networks <- dynamic_network_cooccurrence(nodes = nodes,
directed_edges = references,
source_column = "source_id",
target_column = "target_id",
time_variable = "source_year",
cooccurrence_method = "coupling_similarity",
time_window = 15,
edges_threshold = 1,
compute_size = FALSE,
keep_singleton = FALSE,
overlapping_window = TRUE)
#> Creation of the network for the 1975-1989 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1976-1990 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1977-1991 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1978-1992 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1979-1993 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1980-1994 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1981-1995 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1982-1996 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1983-1997 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1984-1998 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1985-1999 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1986-2000 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1987-2001 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1988-2002 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1989-2003 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1990-2004 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1991-2005 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1992-2006 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1993-2007 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1994-2008 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1995-2009 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1996-2010 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1997-2011 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1998-2012 window.
#> We remove the nodes that are alone with no edge. 
#> 
#> Creation of the network for the 1999-2013 window.
#> We remove the nodes that are alone with no edge. 
#> 

temporal_networks <- lapply(temporal_networks,
                                    function(tbl) tbl %N>%
                                                  mutate(clusters = tidygraph::group_louvain()))

intertemporal_cluster_naming(temporal_networks,
cluster_column = "clusters",
node_key = "source_id",
threshold_similarity = 0.51,
similarity_type = "partial")
#> Warning: `intertemporal_cluster_naming()` was deprecated in networkflow 0.1.0.
#> ℹ Please use `merge_dynamic_clusters()` instead.
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> Joining with `by = join_by(clusters)`
#> $`1975-1989`
#> # A tbl_graph: 71 nodes and 435 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 71 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 16182155   GORDON-R             1975 GORDON-R-19… ALTERNATIVE… "BROOKINGS PA…
#>  2 26283591   GORDON-R             1975 GORDON-R-19… THE IMPACT … "BROOKINGS PA…
#>  3 16182201   OKUN-A               1975 OKUN-A-1975a INFLATION: … "BROOKINGS PA…
#>  4 47749045   BRONFENBRENN…        1976 BRONFENBREN… ELEMENTS OF… "ZEITSCHRIFT …
#>  5 1111111141 KARNOSKY-D           1976 KARNOSKY-D-… THE LINK BE… "REVIEW"      
#>  6 1021902    FRIEDMAN-M           1977 FRIEDMAN-M-… NOBEL LECTU… "THE JOURNAL …
#>  7 5200398    GOLDSTEIN-M          1977 GOLDSTEIN-M… DOWNWARD PR… "STAFF PAPERS"
#>  8 31895842   GORDON-R             1977 GORDON-R-19… CAN THE INF… "BROOKINGS PA…
#>  9 14371908   RASCHE-R             1977 RASCHE-R-19… THE EFFECTS… ""            
#> 10 6013999    SHERMAN-H            1977 SHERMAN-H-1… MONOPOLY PO… "JOURNAL OF E…
#> # ℹ 61 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 435 × 5
#>    from    to   weight Source  Target    
#>   <int> <int>    <dbl> <chr>   <chr>     
#> 1     6    11 0.00158  1021902 1111111122
#> 2     6    45 0.000173 1021902 1111111128
#> 3     6    66 0.000430 1021902 1111111134
#> # ℹ 432 more rows
#> 
#> $`1976-1990`
#> # A tbl_graph: 66 nodes and 385 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 66 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1111111141 KARNOSKY-D           1976 KARNOSKY-D-… THE LINK BE… "REVIEW"      
#>  2 1021902    FRIEDMAN-M           1977 FRIEDMAN-M-… NOBEL LECTU… "THE JOURNAL …
#>  3 5200398    GOLDSTEIN-M          1977 GOLDSTEIN-M… DOWNWARD PR… "STAFF PAPERS"
#>  4 31895842   GORDON-R             1977 GORDON-R-19… CAN THE INF… "BROOKINGS PA…
#>  5 14371908   RASCHE-R             1977 RASCHE-R-19… THE EFFECTS… ""            
#>  6 1111111122 CARLSON-K            1978 CARLSON-K-1… INFLATION, … "FEDERAL RESE…
#>  7 1111111131 ECKSTEIN-O           1978 ECKSTEIN-O-… THE GREAT R… ""            
#>  8 9756183    FAIR-R               1978 FAIR-R-1978  INFLATION A… "AFTER THE PH…
#>  9 1111111150 MORK-K               1978 MORK-K-1978a THE INFLATI… ""            
#> 10 16182206   PERRY-G              1978 PERRY-G-1978 SLOWING THE… "BROOKINGS PA…
#> # ℹ 56 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 385 × 5
#>    from    to   weight Source  Target    
#>   <int> <int>    <dbl> <chr>   <chr>     
#> 1     2     6 0.00161  1021902 1111111122
#> 2     2    40 0.000174 1021902 1111111128
#> 3     2    61 0.000436 1021902 1111111134
#> # ℹ 382 more rows
#> 
#> $`1977-1991`
#> # A tbl_graph: 66 nodes and 384 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 66 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1021902    FRIEDMAN-M           1977 FRIEDMAN-M-… NOBEL LECTU… "THE JOURNAL …
#>  2 5200398    GOLDSTEIN-M          1977 GOLDSTEIN-M… DOWNWARD PR… "STAFF PAPERS"
#>  3 31895842   GORDON-R             1977 GORDON-R-19… CAN THE INF… "BROOKINGS PA…
#>  4 14371908   RASCHE-R             1977 RASCHE-R-19… THE EFFECTS… ""            
#>  5 1111111122 CARLSON-K            1978 CARLSON-K-1… INFLATION, … "FEDERAL RESE…
#>  6 1111111131 ECKSTEIN-O           1978 ECKSTEIN-O-… THE GREAT R… ""            
#>  7 9756183    FAIR-R               1978 FAIR-R-1978  INFLATION A… "AFTER THE PH…
#>  8 1111111150 MORK-K               1978 MORK-K-1978a THE INFLATI… ""            
#>  9 16182206   PERRY-G              1978 PERRY-G-1978 SLOWING THE… "BROOKINGS PA…
#> 10 2207592    PHELPS-E             1978 PHELPS-E-19… COMMODITY-S… "JOURNAL OF M…
#> # ℹ 56 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 384 × 5
#>    from    to   weight Source  Target    
#>   <int> <int>    <dbl> <chr>   <chr>     
#> 1     1     5 0.00161  1021902 1111111122
#> 2     1    39 0.000174 1021902 1111111128
#> 3     1    60 0.000436 1021902 1111111134
#> # ℹ 381 more rows
#> 
#> $`1978-1992`
#> # A tbl_graph: 61 nodes and 341 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 61 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1111111122 CARLSON-K            1978 CARLSON-K-1… INFLATION, … "FEDERAL RESE…
#>  2 1111111131 ECKSTEIN-O           1978 ECKSTEIN-O-… THE GREAT R… ""            
#>  3 9756183    FAIR-R               1978 FAIR-R-1978  INFLATION A… "AFTER THE PH…
#>  4 1111111150 MORK-K               1978 MORK-K-1978a THE INFLATI… ""            
#>  5 16182206   PERRY-G              1978 PERRY-G-1978 SLOWING THE… "BROOKINGS PA…
#>  6 2207592    PHELPS-E             1978 PHELPS-E-19… COMMODITY-S… "JOURNAL OF M…
#>  7 16413171   STEIN-J              1978 STEIN-J-197… INFLATION, … "JOURNAL OF M…
#>  8 1111111172 STEIN-J              1978 STEIN-J-197… INFLATION A… "JOURNAL OF B…
#>  9 31302737   TATOM-J              1978 TATOM-J-1978 DOES THE ST… "FEDERAL RESE…
#> 10 1111111180 WACHTER-M            1978 WACHTER-M-1… INSTITUTION… ""            
#> # ℹ 51 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 341 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    24    35 0.000497 10585974 1111111128
#> 2     2    24 0.000167 10585974 1111111131
#> 3    24    37 0.00188  10585974 1111111137
#> # ℹ 338 more rows
#> 
#> $`1979-1993`
#> # A tbl_graph: 51 nodes and 255 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 51 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 5240516   BLINDER-A            1979 BLINDER-A-19… ECONOMIC PO… ""            
#>  2 6119013   BRUNO-M              1979 BRUNO-M-1979b MACRO-ECONO… ""            
#>  3 31502746  GRAMLICH-E           1979 GRAMLICH-E-1… MACRO POLIC… "BROOKINGS PA…
#>  4 18541214  NORSWORTHY-J         1979 NORSWORTHY-J… THE SLOWDOW… ""            
#>  5 20623867  SACHS-J              1979 SACHS-J-1979  WAGES, PROF… "BROOKINGS PA…
#>  6 33940681  TATOM-J              1979 TATOM-J-1979a THE PRODUCT… ""            
#>  7 41293703  WEINTRAUB-S          1979 WEINTRAUB-S-… COMMENT ON … "JOURNAL OF P…
#>  8 37095547  ATESOGLU-H           1980 ATESOGLU-H-1… INFLATION A… "JOURNAL OF P…
#>  9 1172921   BRANSON-W            1980 BRANSON-W-19… INTERNATION… ""            
#> 10 14490177  BRUNNER-K            1980 BRUNNER-K-19… STAGFLATION… "JOURNAL OF M…
#> # ℹ 41 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 255 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    13    23 0.000554 10585974 1111111128
#> 2    13    25 0.00203  10585974 1111111137
#> 3    13    19 0.00259  10585974 1111111160
#> # ℹ 252 more rows
#> 
#> $`1980-1994`
#> # A tbl_graph: 43 nodes and 171 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 43 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 37095547   ATESOGLU-H           1980 ATESOGLU-H-… INFLATION A… "JOURNAL OF P…
#>  2 1172921    BRANSON-W            1980 BRANSON-W-1… INTERNATION… ""            
#>  3 14490177   BRUNNER-K            1980 BRUNNER-K-1… STAGFLATION… "JOURNAL OF M…
#>  4 14371905   BRUNO-M              1980 BRUNO-M-1980 IMPORT PRIC… "THE ECONOMIC…
#>  5 10585974   FRYE-J               1980 FRYE-J-1980  THE VARIANC… ""            
#>  6 1111111146 LEIJONHUFVUD…        1980 LEIJONHUFVU… THEORIES OF… "REVUE DE L<9…
#>  7 112214146  MORK-K               1980 MORK-K-1980b ENERGY PRIC… ""            
#>  8 25850485   NORDHAUS-W           1980 NORDHAUS-W-… OIL AND ECO… "BROOKINGS PA…
#>  9 16008556   PERRY-G              1980 PERRY-G-1980 INFLATION I… "BROOKINGS PA…
#> 10 1111111159 PINDYCK-R            1980 PINDYCK-R-1… ENERGY PRIC… "THE ENERGY J…
#> # ℹ 33 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 171 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1     5    15 0.000604 10585974 1111111128
#> 2     5    17 0.00222  10585974 1111111137
#> 3     5    11 0.00269  10585974 1111111160
#> # ℹ 168 more rows
#> 
#> $`1981-1995`
#> # A tbl_graph: 32 nodes and 106 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 32 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 25091445   BLINDER-A            1981 BLINDER-A-1… "THE 1971-1… ""            
#>  2 1111111128 DARBY-M              1981 DARBY-M-1981 "THE REAL P… ""            
#>  3 38951845   FRYE-J               1981 FRYE-J-1981  "GOVERNMENT… "THE AMERICAN…
#>  4 1111111137 GORDON-R             1981 GORDON-R-19… "INFLATION,… "NBER WORKING…
#>  5 16001640   LUTZ-MA              1981 LUTZ-MA-1981 "STAGFLATIO… ""            
#>  6 5315078    RASCHE-R             1981 RASCHE-R-19… "ENERGY PRI… "CARNEGIE-ROC…
#>  7 46282251   ATESOGLU-H           1982 ATESOGLU-H-… "WAGES AND … "JOURNAL OF P…
#>  8 42623802   BLAAS-W              1982 BLAAS-W-1982 "INSTITUTIO… "JOURNAL OF E…
#>  9 31895841   BLINDER-A            1982 BLINDER-A-1… "THE ANATOM… "INFLATION: C…
#> 10 1184127    DARBY-M              1982 DARBY-M-1982 "THE PRICE … "THE AMERICAN…
#> # ℹ 22 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 106 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    29    32 0.000428 10729971 1111111176
#> 2    21    32 0.000428 10729971 16167977  
#> 3    30    32 0.00120  10729971 19627977  
#> # ℹ 103 more rows
#> 
#> $`1982-1996`
#> # A tbl_graph: 26 nodes and 78 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 26 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 42623802  BLAAS-W              1982 BLAAS-W-1982  INSTITUTION… "JOURNAL OF E…
#>  2 31895841  BLINDER-A            1982 BLINDER-A-19… THE ANATOMY… "INFLATION: C…
#>  3 1184127   DARBY-M              1982 DARBY-M-1982  THE PRICE O… "THE AMERICAN…
#>  4 14576298  GRUBB-D              1982 GRUBB-D-1982  CAUSES OF T… "THE REVIEW O…
#>  5 33117553  KOURI-PJK            1982 KOURI-PJK-19… MACROECONOM… "AMERICAN ECO…
#>  6 16995857  NORDHAUS-W           1982 NORDHAUS-W-1… ECONOMIC PO… "EUROPEAN ECO…
#>  7 18087990  OLSON-M              1982 OLSON-M-1982a STAGFLATION… "AMERICAN ECO…
#>  8 7815498   DENISON-E            1983 DENISON-E-19… THE INTERRU… "THE ECONOMIC…
#>  9 5982867   GYLFASON-T           1983 GYLFASON-T-1… DOES DEVALU… "CANADIAN JOU…
#> 10 4189938   LINDBECK-A           1983 LINDBECK-A-1… THE RECENT … "THE ECONOMIC…
#> # ℹ 16 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 78 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    22    25 0.000436 10729971 1111111176
#> 2    14    25 0.000436 10729971 16167977  
#> 3    23    25 0.00122  10729971 19627977  
#> # ℹ 75 more rows
#> 
#> $`1983-1997`
#> # A tbl_graph: 22 nodes and 69 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 22 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 7815498    DENISON-E            1983 DENISON-E-1… THE INTERRU… "THE ECONOMIC…
#>  2 5982867    GYLFASON-T           1983 GYLFASON-T-… DOES DEVALU… "CANADIAN JOU…
#>  3 4189938    LINDBECK-A           1983 LINDBECK-A-… THE RECENT … "THE ECONOMIC…
#>  4 5160307    MCCALLUM-C           1983 MCCALLUM-C-… INFLATION A… "THE ECONOMIC…
#>  5 17500902   ROTEMBERG-J          1983 ROTEMBERG-J… SUPPLY SHOC… "JOURNAL OF M…
#>  6 2901822    BRUNO-M              1984 BRUNO-M-1984 RAW MATERIA… "THE QUARTERL…
#>  7 16167977   GORDON-R             1984 GORDON-R-19… SUPPLY SHOC… "THE AMERICAN…
#>  8 1111111134 FISCHER-S            1985 FISCHER-S-1… SUPPLY SHOC… "JOURNAL OF M…
#>  9 40564631   MARTIN-LW            1985 MARTIN-LW-1… STAGFLATION… "AMERICAN JOU…
#> 10 1184130    GISSER-M             1986 GISSER-M-19… CRUDE OIL A… "JOURNAL OF M…
#> # ℹ 12 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 69 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    15    18 0.000353 10729971 1111111176
#> 2     7    18 0.000400 10729971 16167977  
#> 3    16    18 0.00123  10729971 19627977  
#> # ℹ 66 more rows
#> 
#> $`1984-1998`
#> # A tbl_graph: 19 nodes and 67 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 19 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 2901822    BRUNO-M              1984 BRUNO-M-1984 RAW MATERIA… "THE QUARTERL…
#>  2 16167977   GORDON-R             1984 GORDON-R-19… SUPPLY SHOC… "THE AMERICAN…
#>  3 1111111134 FISCHER-S            1985 FISCHER-S-1… SUPPLY SHOC… "JOURNAL OF M…
#>  4 40564631   MARTIN-LW            1985 MARTIN-LW-1… STAGFLATION… "AMERICAN JOU…
#>  5 1184130    GISSER-M             1986 GISSER-M-19… CRUDE OIL A… "JOURNAL OF M…
#>  6 25481516   HAKES-DR             1988 HAKES-DR-19… EVIDENCE OF… "REVIEW OF RA…
#>  7 6714008    HELLIWELL-J          1988 HELLIWELL-J… COMPARATIVE… "JOURNAL OF E…
#>  8 1717556    OLSON-M              1988 OLSON-M-1988 THE PRODUCT… "JOURNAL OF E…
#>  9 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… "JOURNAL OF M…
#> 10 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… "RESERVE BANK…
#> 11 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… "PRICE STABIL…
#> 12 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… "THE QUARTERL…
#> 13 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… "JOURNAL OF M…
#> 14 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… "JOURNAL OF M…
#> 15 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … "BROOKINGS PA…
#> 16 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… "REDUCING INF…
#> 17 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… "REDUCING INF…
#> 18 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… "JOURNAL OF E…
#> 19 17355017   MAYER-T              1998 MAYER-T-1998 MONETARY PO… ""            
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 67 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1    10    13 0.000277 10729971 1111111176
#> 2     2    13 0.000353 10729971 16167977  
#> 3    13    19 0.00147  10729971 17355017  
#> # ℹ 64 more rows
#> 
#> $`1985-1999`
#> # A tbl_graph: 18 nodes and 65 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 18 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1111111134 FISCHER-S            1985 FISCHER-S-1… SUPPLY SHOC… "JOURNAL OF M…
#>  2 40564631   MARTIN-LW            1985 MARTIN-LW-1… STAGFLATION… "AMERICAN JOU…
#>  3 1184130    GISSER-M             1986 GISSER-M-19… CRUDE OIL A… "JOURNAL OF M…
#>  4 25481516   HAKES-DR             1988 HAKES-DR-19… EVIDENCE OF… "REVIEW OF RA…
#>  5 6714008    HELLIWELL-J          1988 HELLIWELL-J… COMPARATIVE… "JOURNAL OF E…
#>  6 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… "JOURNAL OF M…
#>  7 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… "RESERVE BANK…
#>  8 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… "PRICE STABIL…
#>  9 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… "THE QUARTERL…
#> 10 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… "JOURNAL OF M…
#> 11 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… "JOURNAL OF M…
#> 12 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … "BROOKINGS PA…
#> 13 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… "REDUCING INF…
#> 14 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… "REDUCING INF…
#> 15 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… "JOURNAL OF E…
#> 16 17355017   MAYER-T              1998 MAYER-T-1998 MONETARY PO… ""            
#> 17 73951174   IRELAND-P            1999 IRELAND-P-1… DOES THE TI… "JOURNAL OF M…
#> 18 24247525   TAYLOR-J             1999 TAYLOR-J-19… A HISTORICA… "MONETARY POL…
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 65 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1     7    10 0.000282 10729971 1111111176
#> 2    10    16 0.00149  10729971 17355017  
#> 3     8    10 0.00112  10729971 19627977  
#> # ℹ 62 more rows
#> 
#> $`1986-2000`
#> # A tbl_graph: 21 nodes and 107 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 21 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1184130    GISSER-M             1986 GISSER-M-19… CRUDE OIL A… "JOURNAL OF M…
#>  2 25481516   HAKES-DR             1988 HAKES-DR-19… EVIDENCE OF… "REVIEW OF RA…
#>  3 6714008    HELLIWELL-J          1988 HELLIWELL-J… COMPARATIVE… "JOURNAL OF E…
#>  4 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… "JOURNAL OF M…
#>  5 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… "RESERVE BANK…
#>  6 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… "PRICE STABIL…
#>  7 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… "THE QUARTERL…
#>  8 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… "JOURNAL OF M…
#>  9 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… "JOURNAL OF M…
#> 10 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … "BROOKINGS PA…
#> # ℹ 11 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 107 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1     8    20 0.000132 10729971 1111111143
#> 2     5     8 0.000269 10729971 1111111176
#> 3     8    19 0.00102  10729971 15559333  
#> # ℹ 104 more rows
#> 
#> $`1987-2001`
#> # A tbl_graph: 24 nodes and 125 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 24 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 25481516   HAKES-DR             1988 HAKES-DR-19… EVIDENCE OF… "REVIEW OF RA…
#>  2 6714008    HELLIWELL-J          1988 HELLIWELL-J… COMPARATIVE… "JOURNAL OF E…
#>  3 1717556    OLSON-M              1988 OLSON-M-1988 THE PRODUCT… "JOURNAL OF E…
#>  4 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… "JOURNAL OF M…
#>  5 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… "RESERVE BANK…
#>  6 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… "PRICE STABIL…
#>  7 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… "THE QUARTERL…
#>  8 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… "JOURNAL OF M…
#>  9 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… "JOURNAL OF M…
#> 10 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … "BROOKINGS PA…
#> # ℹ 14 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 125 × 5
#>    from    to   weight Source   Target    
#>   <int> <int>    <dbl> <chr>    <chr>     
#> 1     8    20 0.000133 10729971 1111111143
#> 2     5     8 0.000253 10729971 1111111176
#> 3     8    19 0.000992 10729971 15559333  
#> # ℹ 122 more rows
#> 
#> $`1988-2002`
#> # A tbl_graph: 32 nodes and 231 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 32 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 25481516   HAKES-DR             1988 HAKES-DR-19… EVIDENCE OF… "REVIEW OF RA…
#>  2 6714008    HELLIWELL-J          1988 HELLIWELL-J… COMPARATIVE… "JOURNAL OF E…
#>  3 1717556    OLSON-M              1988 OLSON-M-1988 THE PRODUCT… "JOURNAL OF E…
#>  4 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… "JOURNAL OF M…
#>  5 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… "RESERVE BANK…
#>  6 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… "PRICE STABIL…
#>  7 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… "THE QUARTERL…
#>  8 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… "JOURNAL OF M…
#>  9 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… "JOURNAL OF M…
#> 10 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … "BROOKINGS PA…
#> # ℹ 22 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 231 × 5
#>    from    to   weight Source    Target    
#>   <int> <int>    <dbl> <chr>     <chr>     
#> 1    26    31 0.000840 101742296 105203321 
#> 2    20    31 0.000391 101742296 1111111143
#> 3    19    31 0.00251  101742296 15559333  
#> # ℹ 228 more rows
#> 
#> $`1989-2003`
#> # A tbl_graph: 34 nodes and 305 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 34 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… JOURNAL OF MO…
#>  2 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… RESERVE BANK …
#>  3 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… PRICE STABILI…
#>  4 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… THE QUARTERLY…
#>  5 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… JOURNAL OF MO…
#>  6 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… JOURNAL OF MO…
#>  7 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … BROOKINGS PAP…
#>  8 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… REDUCING INFL…
#>  9 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… REDUCING INFL…
#> 10 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… JOURNAL OF EC…
#> # ℹ 24 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 305 × 5
#>    from    to   weight Source    Target    
#>   <int> <int>    <dbl> <chr>     <chr>     
#> 1    24    29 0.000796 101742296 105203321 
#> 2    18    29 0.000348 101742296 1111111143
#> 3    17    29 0.00233  101742296 15559333  
#> # ℹ 302 more rows
#> 
#> $`1990-2004`
#> # A tbl_graph: 39 nodes and 431 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 39 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… JOURNAL OF MO…
#>  2 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… RESERVE BANK …
#>  3 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… PRICE STABILI…
#>  4 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… THE QUARTERLY…
#>  5 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… JOURNAL OF MO…
#>  6 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… JOURNAL OF MO…
#>  7 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … BROOKINGS PAP…
#>  8 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… REDUCING INFL…
#>  9 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… REDUCING INFL…
#> 10 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… JOURNAL OF EC…
#> # ℹ 29 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 431 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    29    38 0.00101  101742279 101742296
#> 2    37    38 0.000143 101742279 101777013
#> 3    38    39 0.000225 101742279 106566092
#> # ℹ 428 more rows
#> 
#> $`1991-2005`
#> # A tbl_graph: 47 nodes and 663 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 47 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 214927     BALL-L               1991 BALL-L-1991  THE GENESIS… JOURNAL OF MO…
#>  2 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… RESERVE BANK …
#>  3 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… PRICE STABILI…
#>  4 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… THE QUARTERLY…
#>  5 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… JOURNAL OF MO…
#>  6 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… JOURNAL OF MO…
#>  7 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … BROOKINGS PAP…
#>  8 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… REDUCING INFL…
#>  9 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… REDUCING INFL…
#> 10 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… JOURNAL OF EC…
#> # ℹ 37 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 663 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    29    38 0.000901 101742279 101742296
#> 2    37    38 0.000125 101742279 101777013
#> 3    38    41 0.000122 101742279 106371053
#> # ℹ 660 more rows
#> 
#> $`1992-2006`
#> # A tbl_graph: 50 nodes and 768 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 50 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 1111111176 TAYLOR-J             1992 TAYLOR-J-19… THE GREAT I… RESERVE BANK …
#>  2 19627977   PARKIN-M             1993 PARKIN-M-19… INFLATION I… PRICE STABILI…
#>  3 2207578    BALL-L               1995 BALL-L-1995a RELATIVE-PR… THE QUARTERLY…
#>  4 10729971   BALL-L               1995 BALL-L-1995b TIME-CONSIS… JOURNAL OF MO…
#>  5 24669774   ROTEMBERG-J          1996 ROTEMBERG-J… IMPERFECT C… JOURNAL OF MO…
#>  6 43590636   BERNANKE-B           1997 BERNANKE-B-… SYSTEMATIC … BROOKINGS PAP…
#>  7 8456979    DELONG-B             1997 DELONG-B-19… AMERICA'S P… REDUCING INFL…
#>  8 27334211   TAYLOR-J             1997 TAYLOR-J-19… AMERICA<92>… REDUCING INFL…
#>  9 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… JOURNAL OF EC…
#> 10 94940407   HETZEL-R             1998 HETZEL-R-19… ARTHUR BURN… FEDERAL RESER…
#> # ℹ 40 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 768 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    28    37 0.000882 101742279 101742296
#> 2    36    37 0.000123 101742279 101777013
#> 3    37    40 0.000115 101742279 106371053
#> # ℹ 765 more rows
#> 
#> $`1993-2007`
#> # A tbl_graph: 55 nodes and 967 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 55 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 19627977  PARKIN-M             1993 PARKIN-M-1993 INFLATION I… "PRICE STABIL…
#>  2 2207578   BALL-L               1995 BALL-L-1995a  RELATIVE-PR… "THE QUARTERL…
#>  3 10729971  BALL-L               1995 BALL-L-1995b  TIME-CONSIS… "JOURNAL OF M…
#>  4 24669774  ROTEMBERG-J          1996 ROTEMBERG-J-… IMPERFECT C… "JOURNAL OF M…
#>  5 43590636  BERNANKE-B           1997 BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#>  6 8456979   DELONG-B             1997 DELONG-B-1997 AMERICA'S P… "REDUCING INF…
#>  7 27334211  TAYLOR-J             1997 TAYLOR-J-1997 AMERICA<92>… "REDUCING INF…
#>  8 60300900  CHARI-V              1998 CHARI-V-1998  EXPECTATION… "JOURNAL OF E…
#>  9 94940407  HETZEL-R             1998 HETZEL-R-1998 ARTHUR BURN… "FEDERAL RESE…
#> 10 17355017  MAYER-T              1998 MAYER-T-1998  MONETARY PO… ""            
#> # ℹ 45 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 967 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    27    36 0.000857 101742279 101742296
#> 2    35    36 0.000119 101742279 101777013
#> 3    36    39 0.000110 101742279 106371053
#> # ℹ 964 more rows
#> 
#> $`1994-2008`
#> # A tbl_graph: 55 nodes and 985 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 55 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 2207578   BALL-L               1995 BALL-L-1995a  RELATIVE-PR… "THE QUARTERL…
#>  2 10729971  BALL-L               1995 BALL-L-1995b  TIME-CONSIS… "JOURNAL OF M…
#>  3 24669774  ROTEMBERG-J          1996 ROTEMBERG-J-… IMPERFECT C… "JOURNAL OF M…
#>  4 43590636  BERNANKE-B           1997 BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#>  5 8456979   DELONG-B             1997 DELONG-B-1997 AMERICA'S P… "REDUCING INF…
#>  6 27334211  TAYLOR-J             1997 TAYLOR-J-1997 AMERICA<92>… "REDUCING INF…
#>  7 60300900  CHARI-V              1998 CHARI-V-1998  EXPECTATION… "JOURNAL OF E…
#>  8 94940407  HETZEL-R             1998 HETZEL-R-1998 ARTHUR BURN… "FEDERAL RESE…
#>  9 17355017  MAYER-T              1998 MAYER-T-1998  MONETARY PO… ""            
#> 10 73951174  IRELAND-P            1999 IRELAND-P-19… DOES THE TI… "JOURNAL OF M…
#> # ℹ 45 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 985 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    26    35 0.000848 101742279 101742296
#> 2    34    35 0.000116 101742279 101777013
#> 3    35    38 0.000110 101742279 106371053
#> # ℹ 982 more rows
#> 
#> $`1995-2009`
#> # A tbl_graph: 60 nodes and 1151 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 60 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 2207578   BALL-L               1995 BALL-L-1995a  RELATIVE-PR… "THE QUARTERL…
#>  2 10729971  BALL-L               1995 BALL-L-1995b  TIME-CONSIS… "JOURNAL OF M…
#>  3 24669774  ROTEMBERG-J          1996 ROTEMBERG-J-… IMPERFECT C… "JOURNAL OF M…
#>  4 43590636  BERNANKE-B           1997 BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#>  5 8456979   DELONG-B             1997 DELONG-B-1997 AMERICA'S P… "REDUCING INF…
#>  6 27334211  TAYLOR-J             1997 TAYLOR-J-1997 AMERICA<92>… "REDUCING INF…
#>  7 60300900  CHARI-V              1998 CHARI-V-1998  EXPECTATION… "JOURNAL OF E…
#>  8 94940407  HETZEL-R             1998 HETZEL-R-1998 ARTHUR BURN… "FEDERAL RESE…
#>  9 17355017  MAYER-T              1998 MAYER-T-1998  MONETARY PO… ""            
#> 10 73951174  IRELAND-P            1999 IRELAND-P-19… DOES THE TI… "JOURNAL OF M…
#> # ℹ 50 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 1,151 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    26    35 0.000838 101742279 101742296
#> 2    34    35 0.000115 101742279 101777013
#> 3    35    38 0.000108 101742279 106371053
#> # ℹ 1,148 more rows
#> 
#> $`1996-2010`
#> # A tbl_graph: 60 nodes and 1163 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 60 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 24669774  ROTEMBERG-J          1996 ROTEMBERG-J-… IMPERFECT C… "JOURNAL OF M…
#>  2 43590636  BERNANKE-B           1997 BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#>  3 8456979   DELONG-B             1997 DELONG-B-1997 AMERICA'S P… "REDUCING INF…
#>  4 27334211  TAYLOR-J             1997 TAYLOR-J-1997 AMERICA<92>… "REDUCING INF…
#>  5 60300900  CHARI-V              1998 CHARI-V-1998  EXPECTATION… "JOURNAL OF E…
#>  6 94940407  HETZEL-R             1998 HETZEL-R-1998 ARTHUR BURN… "FEDERAL RESE…
#>  7 17355017  MAYER-T              1998 MAYER-T-1998  MONETARY PO… ""            
#>  8 73951174  IRELAND-P            1999 IRELAND-P-19… DOES THE TI… "JOURNAL OF M…
#>  9 24247525  TAYLOR-J             1999 TAYLOR-J-199… A HISTORICA… "MONETARY POL…
#> 10 95020640  CHRISTIANO-L         2000 CHRISTIANO-L… THE EXPECTA… ""            
#> # ℹ 50 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 1,163 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    24    33 0.000838 101742279 101742296
#> 2    32    33 0.000115 101742279 101777013
#> 3    33    36 0.000108 101742279 106371053
#> # ℹ 1,160 more rows
#> 
#> $`1997-2011`
#> # A tbl_graph: 60 nodes and 1183 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 60 × 10 (active)
#>    source_id source_author source_year source_label  source_title source_journal
#>    <chr>     <chr>               <int> <chr>         <chr>        <chr>         
#>  1 43590636  BERNANKE-B           1997 BERNANKE-B-1… SYSTEMATIC … "BROOKINGS PA…
#>  2 8456979   DELONG-B             1997 DELONG-B-1997 AMERICA'S P… "REDUCING INF…
#>  3 27334211  TAYLOR-J             1997 TAYLOR-J-1997 AMERICA<92>… "REDUCING INF…
#>  4 60300900  CHARI-V              1998 CHARI-V-1998  EXPECTATION… "JOURNAL OF E…
#>  5 94940407  HETZEL-R             1998 HETZEL-R-1998 ARTHUR BURN… "FEDERAL RESE…
#>  6 17355017  MAYER-T              1998 MAYER-T-1998  MONETARY PO… ""            
#>  7 73951174  IRELAND-P            1999 IRELAND-P-19… DOES THE TI… "JOURNAL OF M…
#>  8 24247525  TAYLOR-J             1999 TAYLOR-J-199… A HISTORICA… "MONETARY POL…
#>  9 95020640  CHRISTIANO-L         2000 CHRISTIANO-L… THE EXPECTA… ""            
#> 10 76064614  CLARIDA-R            2000 CLARIDA-R-20… MONETARY PO… "THE QUARTERL…
#> # ℹ 50 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 1,183 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    23    32 0.000836 101742279 101742296
#> 2    31    32 0.000114 101742279 101777013
#> 3    32    35 0.000107 101742279 106371053
#> # ℹ 1,180 more rows
#> 
#> $`1998-2012`
#> # A tbl_graph: 59 nodes and 1199 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 59 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 60300900   CHARI-V              1998 CHARI-V-1998 EXPECTATION… "JOURNAL OF E…
#>  2 94940407   HETZEL-R             1998 HETZEL-R-19… ARTHUR BURN… "FEDERAL RESE…
#>  3 17355017   MAYER-T              1998 MAYER-T-1998 MONETARY PO… ""            
#>  4 73951174   IRELAND-P            1999 IRELAND-P-1… DOES THE TI… "JOURNAL OF M…
#>  5 24247525   TAYLOR-J             1999 TAYLOR-J-19… A HISTORICA… "MONETARY POL…
#>  6 95020640   CHRISTIANO-L         2000 CHRISTIANO-… THE EXPECTA… ""            
#>  7 76064614   CLARIDA-R            2000 CLARIDA-R-2… MONETARY PO… "THE QUARTERL…
#>  8 15559333   LANSING-K            2000 LANSING-K-2… EXPLORING T… "RESEARCH DEP…
#>  9 1111111143 LANSING-K            2000 LANSING-K-2… LEARNING AB… ""            
#> 10 19381502   ORPHANIDES-A         2000 ORPHANIDES-… ACTIVIST ST… ""            
#> # ℹ 49 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 1,199 × 5
#>    from    to   weight Source    Target   
#>   <int> <int>    <dbl> <chr>     <chr>    
#> 1    20    29 0.000835 101742279 101742296
#> 2    28    29 0.000112 101742279 101777013
#> 3    29    32 0.000105 101742279 106371053
#> # ℹ 1,196 more rows
#> 
#> $`1999-2013`
#> # A tbl_graph: 64 nodes and 1515 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 64 × 10 (active)
#>    source_id  source_author source_year source_label source_title source_journal
#>    <chr>      <chr>               <int> <chr>        <chr>        <chr>         
#>  1 73951174   IRELAND-P            1999 IRELAND-P-1… DOES THE TI… "JOURNAL OF M…
#>  2 24247525   TAYLOR-J             1999 TAYLOR-J-19… A HISTORICA… "MONETARY POL…
#>  3 95020640   CHRISTIANO-L         2000 CHRISTIANO-… THE EXPECTA… ""            
#>  4 76064614   CLARIDA-R            2000 CLARIDA-R-2… MONETARY PO… "THE QUARTERL…
#>  5 15559333   LANSING-K            2000 LANSING-K-2… EXPLORING T… "RESEARCH DEP…
#>  6 1111111143 LANSING-K            2000 LANSING-K-2… LEARNING AB… ""            
#>  7 19381502   ORPHANIDES-A         2000 ORPHANIDES-… ACTIVIST ST… ""            
#>  8 95575922   COGLEY-T             2001 COGLEY-T-20… EVOLVING PO… "NBER MACROEC…
#>  9 82181005   DAVIS-S              2001 DAVIS-S-2001 SECTORAL JO… "JOURNAL OF M…
#> 10 81273149   NITZAN-J             2001 NITZAN-J-20… REGIMES OF … "REVIEW OF IN…
#> # ℹ 54 more rows
#> # ℹ 4 more variables: source_type <chr>, time_window <chr>, clusters <chr>,
#> #   intertemporal_name <chr>
#> #
#> # Edge Data: 1,515 × 5
#>    from    to    weight Source    Target   
#>   <int> <int>     <dbl> <chr>     <chr>    
#> 1    17    26 0.000785  101742279 101742296
#> 2    25    26 0.000105  101742279 101777013
#> 3    26    29 0.0000994 101742279 106371053
#> # ℹ 1,512 more rows
#> 
```
