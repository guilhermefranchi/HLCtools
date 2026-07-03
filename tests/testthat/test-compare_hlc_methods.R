test_that("compare_hlc_methods returns summaries for available HLC methods", {

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

  comparison <- compare_hlc_methods(daily_out)

  expect_true(is.data.frame(comparison))
  expect_true(all(c("method", "n", "mean", "sd", "median", "min", "max") %in% names(comparison)))
  expect_true("HLC_SD" %in% comparison$method)
})


test_that("compare_hlc_methods fails informatively when HLC columns are absent", {

  dat <- data.frame(
    group = "A",
    day = 1,
    value = 10
  )

  expect_error(
    compare_hlc_methods(dat),
    "No HLC columns"
  )
})

test_that("compare_hlc_methods auto-detects weekly-style HLC columns", {

  dat <- data.frame(
    HLC_SD = c(0.70, 0.72, 0.74),
    HLC_MAD = c(0.68, 0.69, 0.70),
    HLC_IQR = c(0.80, 0.79, 0.78),
    HLC_ENT = c(0.60, 0.61, 0.59)
  )

  comparison <- compare_hlc_methods(dat)

  expect_true(is.data.frame(comparison))
  expect_true(all(c("HLC_SD", "HLC_MAD", "HLC_IQR", "HLC_ENT") %in% comparison$method))
})

test_that("compare_hlc_methods detects lying-weighted HLC columns", {

  dat <- data.frame(
    HLC_SD_LYING = c(0.20, 0.25, 0.30),
    HLC_MAD_LYING = c(0.18, 0.22, 0.28),
    HLC_IQR_LYING = c(0.10, 0.15, 0.20),
    HLC_ENT_LYING = c(0.12, 0.14, 0.16)
  )

  comparison <- compare_hlc_methods(dat)

  expect_true(is.data.frame(comparison))
  expect_true("HLC_SD_LYING" %in% comparison$method)
  expect_true("HLC_MAD_LYING" %in% comparison$method)
  expect_true("HLC_IQR_LYING" %in% comparison$method)
  expect_true("HLC_ENT_LYING" %in% comparison$method)
})

