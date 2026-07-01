test_that("HLC equals 1 when all animals have identical lying time", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:5),
    lying = rep(15, 5)
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  expect_equal(out$HLC_SD, 1)
  expect_equal(out$HLC_MAD, 1)
  expect_equal(out$HLC_IQR, 1)
  expect_equal(out$HLC_ENT, 1)
  expect_equal(out$lying_prop, 1)

  expect_equal(out$HLC_SD_LYING, 1)
  expect_equal(out$HLC_MAD_LYING, 1)
  expect_equal(out$HLC_IQR_LYING, 1)
  expect_equal(out$HLC_ENT_LYING, 1)
})


test_that("HLC can be high when all animals are standing, but lying-weighted HLC is zero", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:5),
    lying = rep(0, 5)
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  expect_equal(out$lying_prop, 0)

  # All animals are identical, so unweighted cohesion is high
  expect_equal(out$HLC_SD, 1)
  expect_equal(out$HLC_MAD, 1)
  expect_equal(out$HLC_IQR, 1)
  expect_equal(out$HLC_ENT, 1)

  # But no lying occurs, so lying-weighted HLC is zero
  expect_equal(out$HLC_SD_LYING, 0)
  expect_equal(out$HLC_MAD_LYING, 0)
  expect_equal(out$HLC_IQR_LYING, 0)
  expect_equal(out$HLC_ENT_LYING, 0)
})


test_that("HLC values are bounded between 0 and 1", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:6),
    lying = c(0, 0, 0, 15, 15, 15)
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  hlc_cols <- grep("^HLC_", names(out), value = TRUE)

  for (col in hlc_cols) {
    expect_true(all(out[[col]] >= 0, na.rm = TRUE))
    expect_true(all(out[[col]] <= 1, na.rm = TRUE))
  }
})


test_that("maximally split group gives low SD and MAD cohesion", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:10),
    lying = c(rep(0, 5), rep(15, 5))
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  # Population SD is 7.5, so HLC_SD = 1 - 7.5 / 7.5 = 0
  expect_equal(out$HLC_SD, 0)

  # Mean absolute deviation is also 7.5 in a 50:50 split
  expect_equal(out$HLC_MAD, 0)

  # IQR is 15 in a 50:50 split, so HLC_IQR = 0
  expect_equal(out$HLC_IQR, 0)

  # Binary entropy is maximal at p = 0.5, so HLC_ENT = 0
  expect_equal(out$HLC_ENT, 0)
})


test_that("sync thresholds use greater-than logic", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:5),
    lying = c(15, 15, 15, 0, 0)
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15,
    sync_thresholds = c(0.6, 0.7, 0.8)
  )

  # Mean lying = 9 min; lying_prop = 0.6
  # Function uses > threshold, not >= threshold
  expect_equal(out$lying_prop, 0.6)
  expect_equal(out$sync60, 0)
  expect_equal(out$sync70, 0)
  expect_equal(out$sync80, 0)
})


test_that("sync threshold is positive when lying proportion exceeds threshold", {

  dat <- data.frame(
    group = "A",
    day = 1,
    time = "00:00",
    cow = paste0("cow", 1:5),
    lying = c(15, 15, 15, 15, 0)
  )

  out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15,
    sync_thresholds = c(0.6, 0.7, 0.8, 0.9)
  )

  # Mean lying = 12 min; lying_prop = 0.8
  # > 0.6 and > 0.7 are TRUE; > 0.8 and > 0.9 are FALSE
  expect_equal(out$lying_prop, 0.8)
  expect_equal(out$sync60, 1)
  expect_equal(out$sync70, 1)
  expect_equal(out$sync80, 0)
  expect_equal(out$sync90, 0)
})


test_that("daily summary returns one row per group-day", {

  dat <- data.frame(
    group = rep("A", 10),
    day = rep(1, 10),
    time = rep(c("00:00", "00:15"), each = 5),
    cow = rep(paste0("cow", 1:5), times = 2),
    lying = c(rep(15, 5), rep(0, 5))
  )

  interval_out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  daily_out <- summarise_hlc_daily(
    data = interval_out,
    group = group,
    day = day,
    interval_min = 15
  )

  expect_equal(nrow(daily_out), 1)
  expect_equal(daily_out$n_intervals, 2)
})


test_that("daily summary calculates synchrony time in minutes", {

  dat <- data.frame(
    group = rep("A", 15),
    day = rep(1, 15),
    time = rep(c("00:00", "00:15", "00:30"), each = 5),
    cow = rep(paste0("cow", 1:5), times = 3),
    lying = c(
      rep(15, 5),            # lying_prop = 1.0, sync70 = 1
      c(15, 15, 15, 0, 0),   # lying_prop = 0.6, sync70 = 0
      c(15, 15, 15, 15, 0)   # lying_prop = 0.8, sync70 = 1
    )
  )

  interval_out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying,
    interval_min = 15,
    sync_thresholds = c(0.7)
  )

  daily_out <- summarise_hlc_daily(
    data = interval_out,
    group = group,
    day = day,
    interval_min = 15
  )

  # Two intervals have sync70 = 1, each worth 15 min
  expect_equal(daily_out$sync70_time_min, 30)
})


test_that("period column is retained in interval and daily outputs", {

  dat <- data.frame(
    group = rep("A", 10),
    day = rep(1, 10),
    week = rep(1, 10),
    time = rep(c("00:00", "00:15"), each = 5),
    cow = rep(paste0("cow", 1:5), times = 2),
    lying = c(rep(15, 5), rep(0, 5))
  )

  interval_out <- calculate_hlc(
    data = dat,
    group = group,
    animal = cow,
    day = day,
    period = week,
    interval = time,
    lying = lying,
    interval_min = 15
  )

  daily_out <- summarise_hlc_daily(
    data = interval_out,
    group = group,
    day = day,
    period = week,
    interval_min = 15
  )

  expect_true("week" %in% names(interval_out))
  expect_true("week" %in% names(daily_out))
})
