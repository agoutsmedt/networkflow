# Edges For Bibliographic Coupling Network Of Articles and Books Explaining the 1970s US Stagflation.

A dataset containing the edges of the bibliographic coupling network of
articles and books on stagflation. Built by using
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md):
`biblionetwork::biblio_coupling(Ref_stagflation,"source_id","target_id")`.
Could be used with
[Nodes_coupling](https://agoutsmedt.github.io/networkflow/reference/Nodes_coupling.md)
to create a network with tidygraph.

## Usage

``` r
Edges_coupling
```

## Format

A data frame with 2593 rows and 5 variables:

- from:

  Identifier of the Source document on stagflation, in character format

- to:

  Identifier of the Target document on stagflation, in character format

- weight:

  weight calculated according to the "coupling_angle" method (see
  [biblio_coupling()](https://agoutsmedt.github.io/biblionetwork//reference/biblio_coupling.html))

- Source:

  copy of the identifiers of the Source document on stagflation

- Target:

  copy of the identifiers of the Target document on stagflation

## Source

Created from `Nodes_stagflation.rda`
