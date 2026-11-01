# Nodes of a ready-made bibliographic coupling network on the stagflation

The 154 documents of
[Nodes_stagflation](https://agoutsmedt.github.io/networkflow/reference/Nodes_stagflation.md)
that explain the stagflation and have references in
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md).
With
[Edges_coupling](https://agoutsmedt.github.io/networkflow/reference/Edges_coupling.md),
it gives a bibliographic coupling network already computed, for instance
to try the functions of the package on a `tbl_graph` built directly with
[`tidygraph::tbl_graph()`](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).

## Usage

``` r
Nodes_coupling
```

## Format

A data frame with 154 rows and 6 variables:

- source_id:

  Identifier of the document, as a character string.

- source_author:

  First author of the document.

- source_label:

  Short label (author and year), for plots.

- source_year:

  Year of publication.

- source_title:

  Title of the document.

- source_journal:

  Journal of publication, for articles.

## Source

Built from
[Nodes_stagflation](https://agoutsmedt.github.io/networkflow/reference/Nodes_stagflation.md)
and
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md)
by `inst/data-raw/creating_network_data.R`.
