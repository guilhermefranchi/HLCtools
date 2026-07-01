#' Example lying behaviour dataset
#'
#' A small synthetic dataset for demonstrating Herd Lying Concordance calculations.
#' The dataset contains individual animal lying times for two groups, two days,
#' two time intervals per day, and five cows per group-time interval.
#'
#' @format A data frame with 40 rows and 6 columns:
#' \describe{
#'   \item{group}{Group identifier.}
#'   \item{day}{Day identifier.}
#'   \item{week}{Week identifier.}
#'   \item{time}{Time interval identifier.}
#'   \item{cow}{Individual cow identifier.}
#'   \item{lying}{Lying time within the interval, in minutes.}
#' }
#'
#' @examples
#' data(example_lies)
#'
#' hlc_intervals <- calculate_hlc(
#'   data = example_lies,
#'   group = group,
#'   animal = cow,
#'   day = day,
#'   period = week,
#'   interval = time,
#'   lying = lying
#' )
#'
#' hlc_intervals
"example_lies"
