#' Compare HLC methods
#'
#' Summarises daily HLC metrics across available dispersion bases.
#' This function helps users compare HLC implementations rather than
#' automatically selecting a single best metric.
#'
#' @param data A daily HLC data frame produced by `summarise_hlc_daily()`.
#'
#' @return A tibble with descriptive summaries for each available HLC method.
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
#' hlc_daily <- summarise_hlc_daily(
#'   data = hlc_intervals,
#'   group = group,
#'   day = day,
#'   period = week
#' )
#'
#' compare_hlc_methods(hlc_daily)
#'
#' @export
compare_hlc_methods <- function(data) {

  method_cols <- c(
    "mean_HLC_SD",
    "mean_HLC_MAD",
    "mean_HLC_IQR",
    "mean_HLC_ENT"
  )

  available_cols <- intersect(method_cols, names(data))

  if (length(available_cols) == 0) {
    stop(
      "No daily HLC columns were found. Expected one or more of: ",
      paste(method_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- lapply(
    available_cols,
    function(col) {
      x <- data[[col]]

      data.frame(
        method = gsub("^mean_HLC_", "HLC_", col),
        n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )

  dplyr::bind_rows(out)
}
