#' Articles and Books Explaining the 1970s US Stagflation.
#'
#' A dataset containing the books and academic articles endeavouring to explain
#' what happened in the US economy in the 1970s and which have at least two references
#' in their bibliography. This file could be used as nodes, with `edges_coupling.rda`, for a bibliographic coupling
#' network.
#'
#' @format A data frame with 154 rows and 6 variables:
#' \describe{
#'   \item{ItemID_Ref}{Identifier of the document on stagflation, in character format}
#'   \item{Author}{Author of the document on stagflation}
#'   \item{Author_date}{Use this as a label for nodes}
#'   \item{Year}{Year of publication of the document}
#'   \item{Title}{Title of the document}
#'   \item{Journal}{Journal of publication of the document (if an article)}
#' }
#' @source Created from `Nodes_stagflation.rda`


"Nodes_coupling"
