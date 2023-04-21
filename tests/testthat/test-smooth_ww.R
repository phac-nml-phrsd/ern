test_that("smooth_ww returns a dataframe with the expected format", {
  load("../testdata/ww_test_params.RData")
  prm.smooth.rm = purrr::list_modify(prm.smooth, method = "rollmean")

  res.loess <- smooth_ww(ww.conc, prm.smooth)
  res.rm <- smooth_ww(ww.conc, prm.smooth.rm)

  expect_s3_class(res.loess, "data.frame")
  expect_s3_class(res.rm, "data.frame")

  names_expected <- c("t", "obs", "date")
  expect_equal(
    names(res.loess),
    names_expected
  )
  expect_equal(
    names(res.rm),
    names_expected
  )
})
