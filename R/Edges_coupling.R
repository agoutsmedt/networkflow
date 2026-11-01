#' Edges of a ready-made bibliographic coupling network on the stagflation
#'
#' The bibliographic coupling edges between the documents of
#' [Nodes_coupling], computed with
#' `biblionetwork::biblio_coupling(Ref_stagflation, "source_id", "target_id")`
#' (coupling angle, see [build_dynamic_networks()]).
#'
#' @format A data frame with 2,593 rows and 5 variables:
#' \describe{
#'   \item{from}{Identifier of the first document, as a character string.}
#'   \item{to}{Identifier of the second document, as a character string.}
#'   \item{weight}{Coupling angle between the two documents (see
#'   [biblionetwork::biblio_coupling()]).}
#'   \item{Source}{Copy of `from`.}
#'   \item{Target}{Copy of `to`.}
#' }
#' @source Built from [Ref_stagflation] by
#'   `inst/data-raw/creating_network_data.R`.
"Edges_coupling"
