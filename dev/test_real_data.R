# ──────────────────────────────────────────────────────────────────────────────
# Local package test with real data
# This script is for development only.
# It is not part of the exported HLCtools package.
# ──────────────────────────────────────────────────────────────────────────────

# Load package functions from current source
pkgload::load_all()

# Required packages for this local test
library(dplyr)
library(readxl)

# Load your real dataset
raw_for_sync <- readxl::read_excel(
  "O:/Tech_ANIVET-Nutrition/DKC intensiv/F 2883 GRÆSMETAN - LKH-NIHA/Behavior sensor data/Pedometers/Data_for_Lying_Sync.xlsx",
  sheet = "Sheet 1"
) |>
  dplyr::mutate(
    Treatment = factor(Treatment),
    exp.day   = as.integer(exp.day),
    week      = as.integer(week)
  )

# Calculate interval-level HLC
hlc_intervals <- calculate_hlc(
  data = raw_for_sync,
  group = Treatment,
  animal = Cow,
  day = exp.day,
  period = week,
  interval = t_hms,
  lying = Lying_min,
  interval_min = 15,
  methods = c("sd", "mad", "iqr", "entropy"),
  sync_thresholds = c(0.6, 0.7, 0.8, 0.9),
  add_lying_weighted = TRUE
)

dplyr::glimpse(hlc_intervals)

# Calculate daily-level HLC summaries
hlc_daily <- summarise_hlc_daily(
  data = hlc_intervals,
  group = Treatment,
  day = exp.day,
  period = week,
  interval_min = 15
)

dplyr::glimpse(hlc_daily)

# Basic sanity checks
range(hlc_intervals$HLC_SD, na.rm = TRUE)
range(hlc_intervals$HLC_MAD, na.rm = TRUE)
range(hlc_intervals$HLC_IQR, na.rm = TRUE)
range(hlc_intervals$HLC_ENT, na.rm = TRUE)

range(hlc_intervals$HLC_SD_LYING, na.rm = TRUE)

table(hlc_intervals$sync70, useNA = "ifany")

hlc_daily |>
  dplyr::summarise(
    min_intervals = min(n_intervals, na.rm = TRUE),
    max_intervals = max(n_intervals, na.rm = TRUE),
    mean_intervals = mean(n_intervals, na.rm = TRUE)
  )
