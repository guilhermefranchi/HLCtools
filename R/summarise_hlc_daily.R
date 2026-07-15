#' Summarise HLC metrics by day
#'
#' Aggregates interval-level HLC metrics into daily group-level summaries.
#'
#' @param data Interval-level HLC data produced by `calculate_hlc()`.
#' @param group Column identifying herd, treatment, pen, or group.
#' @param day Column identifying day/date.
#' @param period Optional column identifying a larger period, for example week.
#' @param interval_min Length of the observation interval in minutes. Default is 15.
#'
#' @return A tibble with daily HLC summaries.
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
#'   lying = lying,
#'   interval_min = 15
#' )
#'
#' hlc_daily <- summarise_hlc_daily(
#'   data = hlc_intervals,
#'   group = group,
#'   day = day,
#'   period = week,
#'   interval_min = 15
#' )
#'
#' hlc_daily
#'
#' @export
summarise_hlc_daily <- function(data,
                                group,
                                day,
                                period = NULL,
                                interval_min = 15) {

  group <- rlang::enquo(group)
  day <- rlang::enquo(day)
  period <- rlang::enquo(period)

  grouping_vars <- list(group, day)

  if (!rlang::quo_is_null(period)) {
    grouping_vars <- c(grouping_vars, list(period))
  }

  hlc_cols <- grep("^HLC_(SD|MAD|IQR|ENT)$", names(data), value = TRUE)
  hlc_lying_cols <- grep("^HLC_(SD|MAD|IQR|ENT)_LYING$", names(data), value = TRUE)
  sync_cols <- grep("^sync[0-9]+$", names(data), value = TRUE)

  data |>
    dplyr::group_by(!!!grouping_vars) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(hlc_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "mean_{.col}"
      ),

      dplyr::across(
        dplyr::all_of(hlc_lying_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "mean_{.col}"
      ),

      dplyr::across(
        dplyr::all_of(sync_cols),
        ~ sum(.x, na.rm = TRUE) * interval_min,
        .names = "{.col}_time_min"
      ),

      mean_lying_min_interval = mean(mean_lying, na.rm = TRUE),
      mean_lying_prop = mean(lying_prop, na.rm = TRUE),
      n_intervals = dplyr::n(),
      mean_n_animals = mean(n_animals, na.rm = TRUE),

      .groups = "drop"
    )
}
