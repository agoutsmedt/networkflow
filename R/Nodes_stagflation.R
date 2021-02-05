#' Articles and Books Explaining the 1970s US Stagflation.
#'
#' A dataset containing the books and academic articles endeavouring to explain
#' what happened in the US economy in the 1970s, as well as all the articles and books
#' cited at least twice by the first set of articles and books (on the stagflation).
#'
#' @format A data frame with 558 rows and 7 variables:
#' \describe{
#'   \item{ItemID_Ref}{Identifier of the document}
#'   \item{Author}{Author of the document}
#'   \item{Author_date}{Use this as a label for nodes}
#'   \item{Year}{Year of publication of the document}
#'   \item{Title}{Title of the document}
#'   \item{Journal}{Journal of publication of the document (if an article)}
#'   \item{Type}{If "Stagflation", the document is listed as an explanation of the US stagflation.
#'   If "Non-Stagflation", the document is cited by a document explaining the stagflation}
#' }
#' @source Goutsmedt A. (2020) “From Stagflation to the Great Inflation: Explaining the 1970s US Economic
#' Situation”. Revue d’Economie Politique, Forthcoming 2021.

"Nodes_stagflation"
