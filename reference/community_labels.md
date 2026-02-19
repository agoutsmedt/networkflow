# Calculating the Coordinates for Projecting Community Labels

**\[deprecated\]**

## Usage

``` r
community_labels(
  graph,
  biggest_community = FALSE,
  community_threshold = 0.01,
  community_name_column = "Community_name",
  community_size_column = "Size_com"
)
```

## Arguments

- graph:

  A tidygraph object.

- biggest_community:

  If true, you have the possibility to remove the smallest community,
  depending of the `community_threshold` you have set.

- community_threshold:

  If `biggest_community` is true, the function selects the nodes that
  belong to communities which represent at least x% of the total number
  of nodes. By default, the parameter is set to 1%.

- community_name_column:

  Name of the column with the name of the community to be used as label

- community_size_column:

  Name of the column with the total number of nodes or the share of
  nodes in each community.

## Value

A data.frame with the list of your communities names (only the biggest
one if `biggest_community` set to `TRUE`) and a pair of coordinates to
project these names on your visualisation. It also keeps the color of
the community and its size.

## Details

A simple function to calculate the mean of coordinates x and y for each
community. These coordinates are to be used to plot the name of the
community on the graph visualisation.

You have to run this function only after having calculating (x,y)
coordinates for your nodes.
