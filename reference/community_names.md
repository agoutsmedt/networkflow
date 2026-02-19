# Automatically Attributing Names to Communities

**\[deprecated\]**

A function to give to a community the name of its node with the highest
chosen measure. It also gives the edges the name of their community. If
the edge connects nodes from different community, name will be NA.

The function takes into account the parameters chosen for the
`leiden_improved()` function. If you have chosen 2 or 3 levels of
resolution, it repeats the same process for the second and third
resolution. In other words, for 3 levels of resolution, you will have
the names of the communities for the first value of the Leiden
resolution, but also the names for the second and third values.

## Usage

``` r
community_names(
  graph,
  ordering_column,
  naming = "Label",
  community_column = "Com_ID"
)
```

## Arguments

- graph:

  A tidygraph object.

- ordering_column:

  Enter the name of the column you want to be used to choose the
  community name. For instance, if you choose `Degree`, the function
  takes the value of the `naming` column of the node with the highest
  degree in the community to name the community. You can use other
  measure than network centrality measures: for instance, if nodes are
  articles, you can use the number of citations of articles.

- naming:

  Enter the name of the column you want to be used for naming the
  community. The function takes the node with the highest
  `centrality_measure` chosen, and use the node value in the `naming`
  column to title the community. For instance, if nodes are individuals
  and if you have a column called `surname`, you can use this column.

- community_column:

  The name of your community identifiers column.

## Value

The same graph object but with a column `Community_name`, as well as
`Community_2_name` and `Community_3_name` if you have run the
[`leiden_workflow()`](https://agoutsmedt.github.io/networkflow/reference/leiden_workflow.md)
function for more than one resolution.

## Details

The attribute of nodes and edges with the names of the communities is
called `Community_name`. It is formed by the identifier of the community
(in the `Com_ID` column) and by the `naming` value.

If you have entered a second and a third resolutions values in the
`leiden_improved()` function, you will have two supplementary columns:
`Community_2_name` and `Community_3_name`.
