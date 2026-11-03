# Edges of a ready-made bibliographic coupling network on the stagflation

The bibliographic coupling edges between the documents of
[Nodes_coupling](https://agoutsmedt.github.io/networkflow/reference/Nodes_coupling.md),
computed with
`biblionetwork::coupling_angle(Ref_stagflation, "source_id", "target_id")`
(coupling angle, see
[`build_dynamic_networks()`](https://agoutsmedt.github.io/networkflow/reference/build_dynamic_networks.md)).

## Usage

``` r
Edges_coupling
```

## Format

A data frame with 2,593 rows and 5 variables:

- from:

  Identifier of the first document, as a character string.

- to:

  Identifier of the second document, as a character string.

- weight:

  Coupling angle between the two documents (see
  [`biblionetwork::coupling_angle()`](https://agoutsmedt.github.io/biblionetwork//reference/coupling_angle.html)).

- Source:

  Copy of `from`.

- Target:

  Copy of `to`.

## Source

Built from
[Ref_stagflation](https://agoutsmedt.github.io/networkflow/reference/Ref_stagflation.md)
by `inst/data-raw/creating_network_data.R`.
