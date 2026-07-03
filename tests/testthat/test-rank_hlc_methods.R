test_that("rank_hlc_methods ranks available HLC methods", {

  data(example_lies)

  interval_out <- calculate_hlc(
    data = example_lies,
    group = group,
    animal = cow,
    day = day,
    period = week,
    interval = time,
    lying = lying
  )

  daily_out <- summarise_hlc_daily(
    data = interval_out,
    group = group,
    day = day,
    period = week
  )

  ranked <- rank_hlc_methods(
    data = daily_out,
    group = group,
    period = week,
    lying_prop = mean_lying_prop
  )

  expect_true(is.data.frame(ranked))
  expect_true(all(c(
    "method",
    "metric_column",
    "weighted_rank_score",
    "selected"
  ) %in% names(ranked)))

  expect_equal(sum(ranked$selected), 1)
  expect_true("HLC_SD" %in% ranked$method)
})


test_that("rank_hlc_methods works without group or period", {

  data(example_lies)

  interval_out <- calculate_hlc(
    data = example_lies,
    group = group,
    animal = cow,
    day = day,
    interval = time,
    lying = lying
  )

  daily_out <- summarise_hlc_daily(
    data = interval_out,
    group = group,
    day = day
  )

  ranked <- rank_hlc_methods(
    data = daily_out
  )

  expect_true(is.data.frame(ranked))
  expect_equal(sum(ranked$selected), 1)
})


test_that("rank_hlc_methods fails informatively if no HLC columns exist", {

  dat <- data.frame(
    group = "A",
    day = 1,
    value = 10
  )

  expect_error(
    rank_hlc_methods(dat),
    "No HLC metric columns were found"
  )
})


test_that("rank_hlc_methods accepts explicit metric columns", {

  dat <- data.frame(
    group = rep(c("A", "B"), each = 5),
    day = rep(1:5, times = 2),
    custom_hlc_1 = c(0.7, 0.8, 0.75, 0.82, 0.78, 0.4, 0.45, 0.5, 0.42, 0.48),
    custom_hlc_2 = c(0.6, 0.62, 0.61, 0.63, 0.64, 0.55, 0.56, 0.57, 0.58, 0.59)
  )

  ranked <- rank_hlc_methods(
    data = dat,
    group = group,
    metrics = c("custom_hlc_1", "custom_hlc_2")
  )

  expect_equal(nrow(ranked), 2)
  expect_equal(sum(ranked$selected), 1)
})

test_that("rank_hlc_methods auto-detects weekly-style HLC columns", {

  dat <- data.frame(
    group = rep(c("A", "B"), each = 5),
    week = rep(1:5, times = 2),
    HLC_SD = c(0.70, 0.72, 0.74, 0.73, 0.75, 0.40, 0.42, 0.43, 0.41, 0.44),
    HLC_MAD = c(0.68, 0.69, 0.70, 0.71, 0.72, 0.45, 0.46, 0.47, 0.46, 0.48),
    HLC_IQR = c(0.80, 0.79, 0.78, 0.81, 0.82, 0.50, 0.51, 0.49, 0.52, 0.53),
    HLC_ENT = c(0.60, 0.61, 0.59, 0.62, 0.63, 0.55, 0.54, 0.56, 0.57, 0.58),
    mean_lying_prop = c(0.5, 0.52, 0.51, 0.53, 0.54, 0.4, 0.42, 0.41, 0.43, 0.44)
  )

  ranked <- rank_hlc_methods(
    data = dat,
    group = group,
    period = week,
    lying_prop = mean_lying_prop
  )

  expect_true(is.data.frame(ranked))
  expect_true(all(c("HLC_SD", "HLC_MAD", "HLC_IQR", "HLC_ENT") %in% ranked$method))
  expect_equal(sum(ranked$selected), 1)
})
