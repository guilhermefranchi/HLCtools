#' Compare HLC methods
#'
#' Summarises HLC metrics across available dispersion bases.
#' This function helps users compare HLC implementations rather than
#' automatically selecting a single best metric.
#'
#' @param data A data frame containing HLC summary columns.
#'   This can be daily, weekly, treatment-level, or another summarised object.
#' @param metrics Optional character vector of HLC columns to compare. If `NULL`,
#'   columns matching `mean_HLC_SD`, `mean_HLC_MAD`, `mean_HLC_IQR`,
#'   `mean_HLC_ENT`, `HLC_SD`, `HLC_MAD`, `HLC_IQR`, and `HLC_ENT`
#'   are used when available.
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
compare_hlc_methods <- function(data, metrics = NULL) {

  if (is.null(metrics)) {
    method_cols <- c(
      "mean_HLC_SD",
      "mean_HLC_MAD",
      "mean_HLC_IQR",
      "mean_HLC_ENT",
      "mean_HLC_SD_LYING",
      "mean_HLC_MAD_LYING",
      "mean_HLC_IQR_LYING",
      "mean_HLC_ENT_LYING",
      "HLC_SD",
      "HLC_MAD",
      "HLC_IQR",
      "HLC_ENT",
      "HLC_SD_LYING",
      "HLC_MAD_LYING",
      "HLC_IQR_LYING",
      "HLC_ENT_LYING"
    )

    available_cols <- intersect(method_cols, names(data))
  } else {
    available_cols <- metrics
  }

  missing_cols <- setdiff(available_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "The following metric columns were not found in `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(available_cols) == 0) {
    stop(
      "No HLC columns were found. Expected unweighted or lying-weighted HLC columns, ",
      "for example mean_HLC_SD, mean_HLC_MAD, mean_HLC_SD_LYING, ",
      "or HLC_SD, HLC_MAD, HLC_SD_LYING. ",
      "Alternatively, provide metric column names using the `metrics` argument.",
      call. = FALSE
    )
  }

  out <- lapply(
    available_cols,
    function(col) {
      x <- data[[col]]

      data.frame(
        method = gsub("^mean_", "", col),
        metric_column = col,
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
