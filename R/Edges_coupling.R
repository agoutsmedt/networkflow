#' Edges For Bibliographic Coupling Network Of Articles and Books Explaining the 1970s US Stagflation.
#'
#' A dataset containing the edges of the bibliographic coupling network of articles and books on stagflation.
#' Built by using [Ref_stagflation]: `biblionetwork::biblio_coupling(Ref_stagflation,"Citing_ItemID_Ref","ItemID_Ref")`.
#' Could be used with [Nodes_coupling] to create a network with tidygraph.
#'
#' @format A data frame with 154 rows and 6 variables:
#' \describe{
#'   \item{from}{Identifier of the Source document on stagflation, in character format}
#'   \item{to}{Identifier of the Target document on stagflation, in character format}
#'   \item{weight}{weight calculated according to the "coupling_angle" method (see [biblio_coupling()][biblionetwork::biblio_coupling()])}
#'   \item{Source}{copy of the identifiers of the Source document on stagflation}
#'   \item{Target}{copy of the identifiers of the Target document on stagflation}
#' }
#' @source Created from `Nodes_stagflation.rda`


"Edges_coupling"
