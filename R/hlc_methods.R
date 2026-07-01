#' List available HLC methods
#'
#' Returns the currently implemented Herd Lying Concordance dispersion bases.
#'
#' @return A character vector of available method names.
#'
#' @export
hlc_methods <- function() {
  c("sd", "mad", "iqr", "entropy")
}
