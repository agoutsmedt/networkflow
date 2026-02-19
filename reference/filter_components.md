# Filtering Network Components

**\[experimental\]**

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
filter_components(
  graphs,
  nb_components = 1,
  threshold_alert = 0.05,
  keep_component_columns = FALSE
)
```

## Arguments

- graphs:

  A tibble graph from
  [tidygraph](https://tidygraph.data-imaginist.com/), a list of tibble
  graphs or a data frame.

- nb_components:

  By default, the function takes the main component of the graph
  (`nb_components = 1`). However it is possible to take as many
  components as you wish. The first component is the largest one,
  component 2 is the second one, etc.

- threshold_alert:

  If the biggest component after the last one selected (by default,
  nb_component = 1) gathers more than x% (by default, 5%) of the total
  number of nodes, the function triggers a warning to inform the user
  that he has removed a big component of the network.

- keep_component_columns:

  Set to `TRUE` if you want to store in the tibble graph the components
  number and the size of the components.

## Value

The same tidygraph object or list of tidygraph objects with nodes

## Details

The function will automatically rename the first column of nodes as
"Id".
