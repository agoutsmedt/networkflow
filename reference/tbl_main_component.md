# Creating Graph And Keeping Main Component With Tidygraph From Edges And Nodes

**\[deprecated\]**

This function is deprecated and will be replaced by
`networkflow::extract_main_component()`. This function which i) creates
a [tidygraph](https://tidygraph.data-imaginist.com/index.html) graph
using
[tbl_graph()](https://tidygraph.data-imaginist.com/reference/tbl_graph.html);
ii) keeps the main components of the graph, using
[main_components()](https://tidygraph.data-imaginist.com/reference/group_graph.html);
and iii) warns the user if the first biggest component removed is too
large.

## Usage

``` r
tbl_main_component(
  edges,
  nodes,
  nb_components = 1,
  threshold_alert = 0.05,
  ...
)
```

## Arguments

- edges:

  A dataframe with a list of links between nodes, under the columns
  "from" and "to". The two columns should be in character format.

- nodes:

  A dataframe with a list of nodes. The first column will be used as the
  identifying column. Be careful to avoid doublons in the first column.
  The first column should be in character format.

- nb_components:

  By default, the function takes the main component of the graph
  (nb_components = 1). However it is possible to take as many components
  as you wish Component 1 is the largest one, component 2 is the second
  one, etc.

- threshold_alert:

  If the biggest component after the last one selection (by default,
  nb_component = 1) gathers more than x% (by default, 5%) of the total
  number of nodes, the function triggers a warning to inform the user
  that he has removed a big component of the network.

- ...:

  Any parameter of the
  [tbl_graph()](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
  function. Edges are directed by default. Coupling or co-citation
  network are *undirected* networks.

## Value

A tidygraph object with a list of nodes and a list of edges.

## Details

The function will automatically rename the first column of nodes as
"Id".
