#' List available lying-weighted HLC methods
#'
#' Returns the currently implemented lying-weighted Herd Lying Concordance
#' method names.
#'
#' @return A character vector of available lying-weighted HLC method names.
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
