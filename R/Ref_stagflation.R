#' References cited by the documents explaining the 1970s US stagflation
#'
#' The direct citations of the corpus of
#' \insertCite{goutsmedt2021;textual}{networkflow}: one row per link between
#' a document of [Nodes_stagflation] explaining the stagflation and a
#' document it cites. It is the bipartite relation that
#' [build_network()] and [build_dynamic_networks()] project into a
#' bibliographic coupling network.
#'
#' @format A data frame with 4,416 rows and 6 variables:
#' \describe{
#'   \item{source_id}{Identifier of the citing document.}
#'   \item{target_id}{Identifier of the cited document.}
#'   \item{target_author}{Author of the cited document.}
#'   \item{target_year}{Year of publication of the cited document.}
#'   \item{target_title}{Title of the cited document.}
#'   \item{target_journal}{Journal of the cited document, for articles.}
#' }
#' @source \insertCite{goutsmedt2021;textual}{networkflow}.
#'
#' @references
#'   \insertAllCited{}
"Ref_stagflation"
