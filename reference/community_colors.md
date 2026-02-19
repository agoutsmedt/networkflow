# Creating Color Attribute For Network Communities

**\[deprecated\]**

This function takes as an input a tidygraph graph, with a column with a
community identifier for each node and each edge. It attributes to each
community a color, depending on a palette chosen. If the two nodes
connected by an edge have a different community, the function mixes the
color of the two communities.

## Usage

``` r
community_colors(graph, palette, community_column = "Com_ID")
```

## Arguments

- graph:

  A tidygraph graph

- palette:

  The palette to be used for attributing colors to communities. If you
  use a palette with less color than the total number of communities,
  you will have communities represented by the same color in your
  visualisation.

- community_column:

  The name of the column with the community identifier/number. By
  defautl "Com_ID", as it is the name of the column when you use the
  [`leiden_workflow()`](https://agoutsmedt.github.io/networkflow/reference/leiden_workflow.md)
  function for detecting communities.

## Value

The same tidygraph object but with a new `color` column in the nodes
side, for each community, and a new `color_edges` column in the edges
side.
