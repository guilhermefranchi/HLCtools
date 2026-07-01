#' Calculate interval-level Herd Lying Concordance
#'
#' Calculates Herd Lying Concordance (HLC) metrics from individual animal
#' lying-time records. HLC is calculated within each group-time interval as
#' one minus a normalised measure of between-animal dispersion.
#'
#' @param data A data frame containing individual animal behaviour records.
#' @param group Column identifying the herd, treatment, pen, or group.
#' @param animal Column identifying the individual animal.
#' @param interval Column identifying the time interval within day.
#' @param lying Column containing lying time within the interval, in minutes.
#' @param day Optional column identifying day/date.
#' @param period Optional column identifying a larger period, for example week.
#' @param interval_min Length of the observation interval in minutes. Default is 15.
#' @param methods Character vector of HLC methods to compute. Options are
#'   `"sd"`, `"mad"`, `"iqr"`, and `"entropy"`.
#' @param lying_threshold Threshold in minutes used to define binary lying state
#'   for entropy. Default is half of `interval_min`.
#' @param sync_thresholds Numeric vector of threshold synchrony cut-offs, expressed
#'   as proportions. Default is `c(0.6, 0.7, 0.8, 0.9)`.
#' @param add_lying_weighted Logical. If TRUE, adds lying-weighted HLC metrics.
#'
#' @return A tibble with one row per group-time interval.
#'
#' @examples
#' \dontrun{
#' hlc_intervals <- calculate_hlc(
#'   data = raw_for_sync,
#'   group = Treatment,
#'   animal = Cow,
#'   day = exp.day,
#'   period = week,
#'   interval = t_hms,
#'   lying = Lying_min
#' )
#' }
#'
#' @export
calculate_hlc <- function(data,
                          group,
                          animal,
                          interval,
                          lying,
                          day = NULL,
                          period = NULL,
                          interval_min = 15,
                          methods = c("sd", "mad", "iqr", "entropy"),
                          lying_threshold = interval_min / 2,
                          sync_thresholds = c(0.6, 0.7, 0.8, 0.9),
                          add_lying_weighted = TRUE) {

  group <- rlang::enquo(group)
  animal <- rlang::enquo(animal)
  interval <- rlang::enquo(interval)
  lying <- rlang::enquo(lying)
  day <- rlang::enquo(day)
  period <- rlang::enquo(period)

  methods <- match.arg(
    methods,
    choices = c("sd", "mad", "iqr", "entropy"),
    several.ok = TRUE
  )

  max_half_range <- interval_min / 2

  grouping_vars <- list(group)

  if (!rlang::quo_is_null(day)) {
    grouping_vars <- c(grouping_vars, list(day))
  }

  if (!rlang::quo_is_null(period)) {
    grouping_vars <- c(grouping_vars, list(period))
  }

  grouping_vars <- c(grouping_vars, list(interval))

  out <- data |>
    dplyr::mutate(
      .lying = as.numeric(!!lying),
      .lying_binary = as.integer(.lying >= lying_threshold)
    ) |>
    dplyr::group_by(!!!grouping_vars) |>
    dplyr::summarise(
      n_animals = dplyr::n_distinct(!!animal),
      mean_lying = mean(.lying, na.rm = TRUE),
      lying_prop = bound01(mean_lying / interval_min),

      sd_lying = pop_sd(.lying),
      mad_lying = mean_abs_dev(.lying),
      iqr_lying = stats::IQR(.lying, na.rm = TRUE),
      p_lying = mean(.lying_binary, na.rm = TRUE),

      .groups = "drop"
    )

  if ("sd" %in% methods) {
    out <- out |>
      dplyr::mutate(
        HLC_SD = bound01(1 - sd_lying / max_half_range)
      )
  }

  if ("mad" %in% methods) {
    out <- out |>
      dplyr::mutate(
        HLC_MAD = bound01(1 - mad_lying / max_half_range)
      )
  }

  if ("iqr" %in% methods) {
    out <- out |>
      dplyr::mutate(
        HLC_IQR = bound01(1 - iqr_lying / interval_min)
      )
  }

  if ("entropy" %in% methods) {
    out <- out |>
      dplyr::mutate(
        entropy_raw = binary_entropy(p_lying),
        HLC_ENT = bound01(1 - entropy_raw / log(2))
      )
  }

  if (length(sync_thresholds) > 0) {
    for (thr in sync_thresholds) {
      nm <- threshold_label(thr)
      out[[nm]] <- as.integer(out$lying_prop > thr)
    }
  }

  if (isTRUE(add_lying_weighted)) {
    hlc_cols <- grep("^HLC_(SD|MAD|IQR|ENT)$", names(out), value = TRUE)

    for (col in hlc_cols) {
      new_col <- paste0(col, "_LYING")
      out[[new_col]] <- out[[col]] * out$lying_prop
    }
  }

  out
}
