# Displaying the Highest Cited Nodes

**\[deprecated\]** A simple function for keeping a number n of nodes
with the highest chosen statistics value per communities and a number n
of nodes with the highest chosen statistics value within the whole
network. This is used to display only the most important nodes on your
graph visualisation.

## Usage

``` r
top_nodes(
  graph,
  ordering_column,
  top_n = 20,
  top_n_per_com = 1,
  biggest_community = FALSE,
  community_threshold = 0.01
)
```

## Arguments

- graph:

  A tidygraph object.

- ordering_column:

  The name of the column with numeric values you want to use to select
  the most important nodes of your network. For instance, the column
  gathering the number of citations of each article of your dataframe.

- top_n:

  The number of nodes with the highest chosen statistics value. For
  instance the number of highest cited nodes you want to display on the
  graph.

- top_n_per_com:

  The number of highest cited nodes per community to display.

- biggest_community:

  If true, you have the possibility to remove the smallest community,
  depending of the `community_threshold` you have set.

- community_threshold:

  If `biggest_community` is true, the function only selects the nodes
  that belong to communities which represent at least x% of the total
  number of nodes. By default, the parameter is set to 1%.

## Value

A data.table with a number n of nodes with the highest chosen statistics
value per communities and a number n of nodes with the highest chosen
statistics value within the whole network. Informations like coordinates
(x,y) and community identifier are kept for using the data.table in a
visualisation.
