#' List available unweighted HLC methods
#'
#' Returns the currently implemented unweighted Herd Lying Concordance
#' dispersion bases.
#'
#' @return A character vector of available unweighted method names.
#'
#' @export
hlc_methods <- function() {
  c("sd", "mad", "iqr", "entropy")
}
