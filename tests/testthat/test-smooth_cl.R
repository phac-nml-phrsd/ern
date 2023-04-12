test_that("smoothing is turned off when prm.smooth is NULL", {
  expect_identical(
    smooth_cl(cl.daily, prm.smooth = NULL),
    cl.daily
  )
})

test_that("rollmean smoothing on clinical data produces expected results", {
  value_smoothed <- value
  for(i in 1:(length(value)-1)){
    value_smoothed[i] <- (value[i] + value[i+1])/2
  }

  res <- smooth_cl(
    cl.daily,
    prm.smooth = list(method = "rollmean", window = 2)
  )

  expect_equal(
    res$value,
    value_smoothed
  )
})
