# Add Leiden Communities to graph

**\[deprecated\]**

This function has been superseded by the more general
[`networkflow::add_clusters()`](https://agoutsmedt.github.io/networkflow/reference/add_clusters.md).
This function takes as input a tidygraph graph It then runs the Leiden
detection community algorithm (Traag et al. 2019) which creates a
partition. The `Leiden_workflow` functhion uses `find_partition()` from
the
[leidenAlg](https://cran.r-project.org/web/packages/leidenAlg/index.html)
package. The function then associates each node to its corresponding
community number. It also creates a community attribute for edges: to
each edge is associated a corresponding community number, if the two
nodes connected by the edge belong to the same community. If nodes have
a different community, the edge takes as attribute the total number of
communities plus 1.

## Usage

``` r
leiden_workflow(graph, res_1 = 1, res_2 = NULL, res_3 = NULL, niter = 1000)
```

## Arguments

- graph:

  A tidygraph object with a "weight" column for edges.

- res_1:

  The first resolution used for running the Leiden algorithm. 1 by
  default.

- res_2:

  The second resolution used for running the Leiden algorithm a second
  time. It adds a second community attribute to nodes and edges. By
  default, res_2 is null and the function just run the Leiden algorithm
  once (with a resolution equals to res_1).

- res_3:

  The third resolution used for running the Leiden algorithm a third
  time.

- niter:

  Number of iterations to run the Leiden algorithm, in order to optimise
  the resulting partition. By default, `niter` equals 500 which warrants
  a quasi-optimal partitionning. Decrease n_iterations for exploratory
  work, in order to decrease computation time.

## Value

The same tidygraph graph as input, but with a new (or three) community
number attribute(s) for each node and each edge.

## Details

The function could be run for 1, 2 or 3 different resolution values of
the Leiden algorithm. It allows the user to compare different
communities partitions. A low resolution means fewer communities. For
instance, if the second resolution is smaller than the first one, we can
observe how decreasing the resolution led some communities to disappear
and to be merged with other communities. Sankey diagrams enable
interesting analysis of the different partitions.

The function also automatically calculates the percentage of total nodes
that are gathered in each community, in the column `Size_com`. This
calculation is only done for `Com_ID`, that is for the resolution equals
to 1.

To make plotting easier later, a zero is put before each one-digit
community number (community 5 becomes 05).

The community attribute of nodes and edges for the first resolution is
called `Com_ID`. For the second and third resolution, it is called
respectively `Com_ID_2` and `Com_ID_3`.

Attributing a community number to edges enable to give edges the same
color of the nodes they are connecting, if the two nodes have the same
color, or a different color than any node, if the nodes belong to a
different community.

## References

Traag VA, Waltman L, van Eck NJ (2019). “From Louvain to Leiden:
Guaranteeing Well-Connected Communities.” *Scientific reports*,
**9**(1), 1–12.
