#' Nodes of a ready-made bibliographic coupling network on the stagflation
#'
#' The 154 documents of [Nodes_stagflation] that explain the stagflation and
#' have references in [Ref_stagflation]. With [Edges_coupling], it gives a
#' bibliographic coupling network already computed, for instance to try the
#' functions of the package on a `tbl_graph` built directly with
#' [tidygraph::tbl_graph()].
#'
#' @format A data frame with 154 rows and 6 variables:
#' \describe{
#'   \item{source_id}{Identifier of the document, as a character string.}
#'   \item{source_author}{First author of the document.}
#'   \item{source_label}{Short label (author and year), for plots.}
#'   \item{source_year}{Year of publication.}
#'   \item{source_title}{Title of the document.}
#'   \item{source_journal}{Journal of publication, for articles.}
#' }
#' @source Built from [Nodes_stagflation] and [Ref_stagflation] by
#'   `inst/data-raw/creating_network_data.R`.
"Nodes_coupling"
