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

y <- rnorm(10)
df <- data.frame(
  id = as.integer(1),
  date = seq.Date(as.Date("2023-04-21"),
                  by = 1,
                  length.out = length(y)),
  value = y,
  t = 1:length(y)
)

test_that("rollmean smoothing on clinical data produces expected results", {

  prm.smooth.rollmean <- list(method = "rollmean", window = 2, align = "center")

  res <- smooth_cl(
    df,
    prm.smooth = prm.smooth.rollmean
  )

  # calculate expected
  value_smoothed <- y
  for(i in 1:(length(y)-1)){
    value_smoothed[i] <- (y[i] + y[i+1])/2
  }

  expect_equal(
    res$value,
    value_smoothed
  )
})

test_that("loess smoothing on clinical data produces expected results", {
  prm.smooth.loess = list(
    method = "loess",
    span = 0.5
  )

  res <- smooth_cl(
    df,
    prm.smooth = prm.smooth.loess
  )

  # calculate expected
  z = stats::loess(formula = "value ~ t", 
                   data = df,
                   control = stats::loess.control(surface = 'direct'),
                   span = prm.smooth.loess$span)
  value_smoothed = z$fitted

  expect_equal(
    res$value,
    value_smoothed
  )
})


