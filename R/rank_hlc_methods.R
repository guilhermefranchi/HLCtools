#' Rank HLC methods for a dataset
#'
#' Ranks available Herd Lying Concordance (HLC) implementations using descriptive
#' criteria relevant for method selection. The function is intended to support
#' transparent, dataset-specific selection of an HLC implementation. It does not
#' imply that one dispersion basis is universally best.
#'
#' @param data A data frame containing daily or period-level HLC summaries.
#'   Usually this is the output of `summarise_hlc_daily()`.
#' @param group Optional column identifying treatment, herd, pen, farm, or other
#'   comparison group. If supplied, treatment/group detectability is evaluated.
#' @param period Optional column identifying week, period, block, or another
#'   temporal unit. If supplied, temporal stability is evaluated.
#' @param lying_prop Optional column containing lying proportion. If supplied,
#'   Spearman correlation between each HLC method and lying proportion is reported.
#'   This correlation is reported for interpretation but is not included in the
#'   default ranking score.
#' @param metrics Optional character vector of HLC columns to rank. If `NULL`,
#'   columns matching `mean_HLC_SD`, `mean_HLC_MAD`, `mean_HLC_IQR`, and
#'   `mean_HLC_ENT` are used when available.
#' @param weights Named numeric vector giving weights for ranking criteria.
#'   Supported names are `"detectability"`, `"temporal_stability"`,
#'   `"outlier_robustness"`, `"boundary"`, and `"missingness"`.
#'
#' @return A tibble summarising method performance and ranking. Lower
#'   `weighted_rank_score` indicates a more favourable method under the chosen
#'   criteria. The column `selected` marks the top-ranked method.
#'
#' @details
#' The default criteria are:
#'
#' - detectability: ability to separate groups, assessed by the F statistic from
#'   a nested linear model comparison when `group` is supplied;
#' - temporal stability: mean within-group SD of period-level means when both
#'   `group` and `period` are supplied;
#' - outlier robustness: absolute distance of SD/MAD from 1, where values closer
#'   to 1 indicate less outlier sensitivity;
#' - boundary: percentage of exact 0 or 1 values;
#' - missingness: percentage of missing values.
#'
#' The ranking is descriptive and should support, not replace, biological
#' interpretation.
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
#' rank_hlc_methods(
#'   data = hlc_daily,
#'   group = group,
#'   period = week,
#'   lying_prop = mean_lying_prop
#' )
#'
#' @export
rank_hlc_methods <- function(data,
                             group = NULL,
                             period = NULL,
                             lying_prop = NULL,
                             metrics = NULL,
                             weights = c(
                               detectability = 1,
                               temporal_stability = 1,
                               outlier_robustness = 1,
                               boundary = 1,
                               missingness = 1
                             )) {

  group_quo <- rlang::enquo(group)
  period_quo <- rlang::enquo(period)
  lying_prop_quo <- rlang::enquo(lying_prop)

  group_name <- if (rlang::quo_is_null(group_quo)) {
    NULL
  } else {
    rlang::as_name(group_quo)
  }

  period_name <- if (rlang::quo_is_null(period_quo)) {
    NULL
  } else {
    rlang::as_name(period_quo)
  }

  lying_prop_name <- if (rlang::quo_is_null(lying_prop_quo)) {
    NULL
  } else {
    rlang::as_name(lying_prop_quo)
  }

  if (is.null(metrics)) {
    default_metric_cols <- c(
      "mean_HLC_SD",
      "mean_HLC_MAD",
      "mean_HLC_IQR",
      "mean_HLC_ENT"
    )

    metrics <- intersect(default_metric_cols, names(data))
  }

  if (length(metrics) == 0) {
    stop(
      "No HLC metric columns were found. Expected one or more of: ",
      "mean_HLC_SD, mean_HLC_MAD, mean_HLC_IQR, mean_HLC_ENT. ",
      "Alternatively, provide metric column names using the `metrics` argument.",
      call. = FALSE
    )
  }

  missing_metrics <- setdiff(metrics, names(data))

  if (length(missing_metrics) > 0) {
    stop(
      "The following metric columns were not found in `data`: ",
      paste(missing_metrics, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_weights <- c(
    "detectability",
    "temporal_stability",
    "outlier_robustness",
    "boundary",
    "missingness"
  )

  unknown_weights <- setdiff(names(weights), allowed_weights)

  if (length(unknown_weights) > 0) {
    stop(
      "Unsupported weight name(s): ",
      paste(unknown_weights, collapse = ", "),
      ". Supported names are: ",
      paste(allowed_weights, collapse = ", "),
      call. = FALSE
    )
  }

  compute_detectability <- function(x, group_vec, period_vec = NULL) {

    if (is.null(group_vec)) {
      return(list(
        detectability_F = NA_real_,
        detectability_p = NA_real_,
        delta_AIC = NA_real_
      ))
    }

    dat <- data.frame(
      y = x,
      group = as.factor(group_vec)
    )

    if (!is.null(period_vec)) {
      dat$period <- as.factor(period_vec)
    }

    dat <- dat[stats::complete.cases(dat), , drop = FALSE]

    if (nrow(dat) < 4 || length(unique(dat$group)) < 2) {
      return(list(
        detectability_F = NA_real_,
        detectability_p = NA_real_,
        delta_AIC = NA_real_
      ))
    }

    fit_full <- tryCatch(
      {
        if ("period" %in% names(dat) && length(unique(dat$period)) > 1) {
          stats::lm(y ~ group + period, data = dat)
        } else {
          stats::lm(y ~ group, data = dat)
        }
      },
      error = function(e) NULL
    )

    fit_reduced <- tryCatch(
      {
        if ("period" %in% names(dat) && length(unique(dat$period)) > 1) {
          stats::lm(y ~ period, data = dat)
        } else {
          stats::lm(y ~ 1, data = dat)
        }
      },
      error = function(e) NULL
    )

    if (is.null(fit_full) || is.null(fit_reduced)) {
      return(list(
        detectability_F = NA_real_,
        detectability_p = NA_real_,
        delta_AIC = NA_real_
      ))
    }

    model_test <- tryCatch(
      stats::anova(fit_reduced, fit_full),
      error = function(e) NULL
    )

    if (is.null(model_test) || nrow(model_test) < 2) {
      return(list(
        detectability_F = NA_real_,
        detectability_p = NA_real_,
        delta_AIC = NA_real_
      ))
    }

    list(
      detectability_F = unname(model_test$F[2]),
      detectability_p = unname(model_test$`Pr(>F)`[2]),
      delta_AIC = stats::AIC(fit_reduced) - stats::AIC(fit_full)
    )
  }

  compute_temporal_sd <- function(x, group_vec = NULL, period_vec = NULL) {

    if (is.null(period_vec)) {
      return(NA_real_)
    }

    if (!is.null(group_vec)) {
      dat <- data.frame(
        x = x,
        group = group_vec,
        period = period_vec
      )

      dat <- dat[stats::complete.cases(dat), , drop = FALSE]

      if (nrow(dat) == 0) {
        return(NA_real_)
      }

      period_means <- stats::aggregate(
        x ~ group + period,
        data = dat,
        FUN = mean
      )

      group_sds <- stats::aggregate(
        x ~ group,
        data = period_means,
        FUN = stats::sd
      )

      return(mean(group_sds$x, na.rm = TRUE))
    }

    dat <- data.frame(
      x = x,
      period = period_vec
    )

    dat <- dat[stats::complete.cases(dat), , drop = FALSE]

    if (nrow(dat) == 0) {
      return(NA_real_)
    }

    period_means <- stats::aggregate(
      x ~ period,
      data = dat,
      FUN = mean
    )

    stats::sd(period_means$x, na.rm = TRUE)
  }

  method_summaries <- lapply(
    metrics,
    function(col) {

      x <- data[[col]]

      group_vec <- if (!is.null(group_name)) data[[group_name]] else NULL
      period_vec <- if (!is.null(period_name)) data[[period_name]] else NULL
      lying_vec <- if (!is.null(lying_prop_name)) data[[lying_prop_name]] else NULL

      detect <- compute_detectability(
        x = x,
        group_vec = group_vec,
        period_vec = period_vec
      )

      temporal_sd <- compute_temporal_sd(
        x = x,
        group_vec = group_vec,
        period_vec = period_vec
      )

      mad_val <- stats::mad(x, na.rm = TRUE)
      sd_val <- stats::sd(x, na.rm = TRUE)

      outlier_sensitivity <- if (
        is.finite(mad_val) &&
        !is.na(mad_val) &&
        mad_val > 0
      ) {
        sd_val / mad_val
      } else {
        NA_real_
      }

      lying_correlation <- if (!is.null(lying_vec)) {
        ok <- is.finite(x) & is.finite(lying_vec)

        if (sum(ok) >= 3) {
          suppressWarnings(
            stats::cor(
              x[ok],
              lying_vec[ok],
              method = "spearman"
            )
          )
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }

      data.frame(
        method = gsub("^mean_", "", col),
        metric_column = col,
        n_total = length(x),
        n_non_missing = sum(!is.na(x)),
        pct_missing = 100 * mean(is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        pct_boundary = 100 * mean(x %in% c(0, 1), na.rm = TRUE),
        detectability_F = detect$detectability_F,
        detectability_p = detect$detectability_p,
        delta_AIC = detect$delta_AIC,
        temporal_sd = temporal_sd,
        outlier_sensitivity = outlier_sensitivity,
        outlier_distance_from_1 = abs(outlier_sensitivity - 1),
        lying_prop_spearman = lying_correlation,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- dplyr::bind_rows(method_summaries)

  rank_or_na <- function(x, decreasing = FALSE) {
    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }

    if (decreasing) {
      return(rank(-x, ties.method = "average", na.last = "keep"))
    }

    rank(x, ties.method = "average", na.last = "keep")
  }

  out$rank_detectability <- rank_or_na(
    out$detectability_F,
    decreasing = TRUE
  )

  out$rank_temporal_stability <- rank_or_na(
    out$temporal_sd,
    decreasing = FALSE
  )

  out$rank_outlier_robustness <- rank_or_na(
    out$outlier_distance_from_1,
    decreasing = FALSE
  )

  out$rank_boundary <- rank_or_na(
    out$pct_boundary,
    decreasing = FALSE
  )

  out$rank_missingness <- rank_or_na(
    out$pct_missing,
    decreasing = FALSE
  )

  rank_matrix <- cbind(
    detectability = out$rank_detectability,
    temporal_stability = out$rank_temporal_stability,
    outlier_robustness = out$rank_outlier_robustness,
    boundary = out$rank_boundary,
    missingness = out$rank_missingness
  )

  weights <- weights[colnames(rank_matrix)]
  weights[is.na(weights)] <- 0

  out$weighted_rank_score <- apply(
    rank_matrix,
    1,
    function(ranks) {
      ok <- !is.na(ranks) & weights > 0

      if (!any(ok)) {
        return(NA_real_)
      }

      sum(ranks[ok] * weights[ok]) / sum(weights[ok])
    }
  )

  out <- out |>
    dplyr::arrange(
      weighted_rank_score,
      pct_missing,
      pct_boundary,
      dplyr::desc(detectability_F)
    ) |>
    dplyr::mutate(
      selected = dplyr::row_number() == 1
    )

  out
}
