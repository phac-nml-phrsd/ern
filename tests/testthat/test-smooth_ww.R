test_that("smooth_ww detects missing smoothing method",{
  load("../testdata/ww_test_params.RData")
  prm.smooth = list(
    span = 0.1
  )
  expect_error(smooth_ww(ww.conc,
                         prm.smooth = prm.smooth),
               "Method is not specified in prm.smooth. Please specify a smoothing
         method. Aborting!")
})

test_that("smooth_ww detects missing smoothing method",{
  load("../testdata/ww_test_params.RData")
  prm.smooth = list(
    span = 0.1,
    method = "rolloess"
  )
  expect_error(smooth_ww(ww.conc,
                         prm.smooth = prm.smooth),
               "Invalid smoothing method. Please specify either rollmean or loess
         as smoothing in prm.smooth. Aborting!")
})

test_that("smooth_ww detects invalid rollmean window",{
  load("../testdata/ww_test_params.RData")
  prm.smooth = list(
    method = 'rollmean',
    window = -14,
    align = "center"
  )
  expect_error(smooth_ww(ww.conc,
                         prm.smooth = prm.smooth),
               "Missing or invalid rollmean window. Please specify a rollmean value
           that is greater than 0 in prm.smooth. Aborting!")
})

test_that("smooth_ww detects invalid rollmean alignment",{
  load("../testdata/ww_test_params.RData")
  prm.smooth = list(
    method = 'rollmean',
    window = 14,
    align = "up"
  )
  expect_error(smooth_ww(ww.conc,
                         prm.smooth = prm.smooth),
               "Missing or invalid rollmean alignment. Please specify a valid
           alignment in prm.smooth. Aborting!")
})

test_that("smooth_ww detects invalid loess span",{
  load("../testdata/ww_test_params.RData")
  prm.smooth = list(
    method = 'loess',
    span = -200
  )
  expect_error(smooth_ww(ww.conc,
                         prm.smooth = prm.smooth),
               "Missing or invalid loess span. Please specify a loess span that is
           greater than 0 in prm.smooth. Aborting!")
})

test_that("smooth_ww returns an error when df is missing a date or val columns", {
  load("../testdata/ww_test_params.RData")
  df.missingdate = dplyr::select(ww.conc, -date)
  df.missingval = dplyr::select(ww.conc, -val)
  expect_error(smooth_ww(df.missingdate, prm.smooth))
  expect_error(smooth_ww(df.missingval, prm.smooth))
})

test_that("smooth_ww returns a dataframe", {
  load("../testdata/ww_test_params.RData")
  prm.smooth.rm = purrr:::list_modify(prm.smooth, method = "rollmean")
  expect_s3_class(smooth_ww(ww.conc, prm.smooth), "data.frame")
  expect_s3_class(smooth_ww(ww.conc, prm.smooth.rm), "data.frame")
})
