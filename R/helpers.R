# Internal helper: bound numeric values to [0, 1]
bound01 <- function(x) {
  pmax(pmin(x, 1), 0)
}

# Internal helper: population standard deviation
pop_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sqrt(mean((x - mean(x))^2))
}

# Internal helper: mean absolute deviation from the mean
mean_abs_dev <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  mean(abs(x - mean(x)))
}

# Internal helper: binary Shannon entropy
binary_entropy <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_real_,
    p <= 0 | p >= 1 ~ 0,
    TRUE ~ -p * log(p) - (1 - p) * log(1 - p)
  )
}

# Internal helper: label synchrony thresholds
threshold_label <- function(x) {
  paste0("sync", round(x * 100))
}
