#' Documents explaining the 1970s US stagflation, and the documents they cite
#'
#' The corpus of \insertCite{goutsmedt2021;textual}{networkflow}: the 174
#' books and academic articles that tried to explain the US stagflation of
#' the 1970s, published from 1975 to 2013 (`source_type == "Stagflation"`),
#' and the 480 documents cited at least twice by them (`"Non-Stagflation"`).
#' With [Ref_stagflation], it is the example data of the package: the
#' explanations are the nodes of a bibliographic coupling network, linked by
#' the references they share.
#'
#' @format A data frame with 654 rows and 7 variables:
#' \describe{
#'   \item{source_id}{Identifier of the document.}
#'   \item{source_author}{First author of the document (see
#'   [Authors_stagflation] for all authors).}
#'   \item{source_label}{Short label (author and year), for plots.}
#'   \item{source_year}{Year of publication, as a character string.}
#'   \item{source_title}{Title of the document.}
#'   \item{source_journal}{Journal of publication, for articles.}
#'   \item{source_type}{`"Stagflation"` for the documents explaining the
#'   stagflation, `"Non-Stagflation"` for the documents they cite.}
#' }
#' @source \insertCite{goutsmedt2021;textual}{networkflow}.
#'
#' @references
#'   \insertAllCited{}
"Nodes_stagflation"
