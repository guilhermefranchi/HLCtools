test_that("hlc_weighted_methods returns available lying-weighted methods", {

  methods <- hlc_weighted_methods()

  expect_true(is.character(methods))
  expect_true("HLC_SD_LYING" %in% methods)
  expect_true("HLC_MAD_LYING" %in% methods)
  expect_true("HLC_IQR_LYING" %in% methods)
  expect_true("HLC_ENT_LYING" %in% methods)
})
