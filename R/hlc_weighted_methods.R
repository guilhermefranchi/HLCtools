#' List available lying-weighted HLC methods
#'
#' Returns the names of the currently implemented lying-weighted Herd Lying
#' Concordance methods.
#'
#' @return A character vector containing the available lying-weighted HLC
#'   method names.
#'
#' @examples
#' hlc_weighted_methods()
#'
#' @export
hlc_weighted_methods <- function() {
  c(
    "HLC_SD_LYING",
    "HLC_MAD_LYING",
    "HLC_IQR_LYING",
    "HLC_ENT_LYING"
  )
}
