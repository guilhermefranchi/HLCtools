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
    "No daily HLC columns were found"
  )
})
