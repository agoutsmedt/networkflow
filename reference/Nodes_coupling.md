# Articles and Books Explaining the 1970s US Stagflation.

A dataset containing the books and academic articles endeavouring to
explain what happened in the US economy in the 1970s and which have at
least two references in their bibliography. This file could be used as
nodes, with `edges_coupling.rda`, for a bibliographic coupling network.

## Usage

``` r
Nodes_coupling
```

## Format

A data frame with 154 rows and 6 variables:

- source_id:

  Identifier of the document on stagflation, in character format

- source_author:

  Author of the document on stagflation

- source_label:

  Use this as a label for nodes

- source_year:

  Year of publication of the document

- source_title:

  Title of the document

- source_journal:

  Journal of publication of the document (if an article)

## Source

Created from `Nodes_stagflation.rda`
