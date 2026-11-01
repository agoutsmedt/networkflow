# Documents explaining the 1970s US stagflation, and the documents they cite

The corpus of Goutsmedt (2021) : the 174 books and academic articles
that tried to explain the US stagflation of the 1970s, published from
1975 to 2013 (`source_type == "Stagflation"`), and the 480 documents
cited at least twice by them (`"Non-Stagflation"`). With
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md),
it is the example data of the package: the explanations are the nodes of
a bibliographic coupling network, linked by the references they share.

## Usage

``` r
Nodes_stagflation
```

## Format

A data frame with 654 rows and 7 variables:

- source_id:

  Identifier of the document.

- source_author:

  First author of the document (see
  [Authors_stagflation](https://agoutsmedt.github.io/networkflow/reference/Authors_stagflation.md)
  for all authors).

- source_label:

  Short label (author and year), for plots.

- source_year:

  Year of publication, as a character string.

- source_title:

  Title of the document.

- source_journal:

  Journal of publication, for articles.

- source_type:

  `"Stagflation"` for the documents explaining the stagflation,
  `"Non-Stagflation"` for the documents they cite.

## Source

Goutsmedt (2021) .

## References

Goutsmedt A (2021). “From the Stagflation to the Great Inflation:
Explaining the US Economy of the 1970s.” *Revue d'Économie Politique*,
**131**(3), 557–582.
<https://www.cairn.info/revue-d-economie-politique-2021-3-page-557.htm>.
