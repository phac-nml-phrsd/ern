test_that("rollmean smoothing on clinical data produces expected results", {
  value_smoothed <- value
  for(i in 1:(length(value)-1)){
    value_smoothed[i] <- (value[i] + value[i+1])/2
  }

  res <- smooth_cl(
    cl.daily,
    prm.smooth = list(method = "rollmean", window = 2, align = "center")
  )

  expect_equal(
    res$value,
    value_smoothed
  )
})

test_that("loess smoothing on clinical data produces expected results", {

  df <- data.frame(
    id = as.integer(1),
    date = seq.Date(as.Date("2023-04-21"),
                    by = 1,
                    length.out = length(value)),
    value = value,
    t = 1:length(value)
  )
  prm.smooth.loess = list(
    method = "loess",
    span = 0.5
  )

  z = stats::loess(formula = "value ~ t", data = df, span = prm.smooth.loess$span)
  value_smoothed = z$fitted

  res <- smooth_cl(
    df,
    prm.smooth = prm.smooth.loess
  )

  expect_equal(
    res$value,
    value_smoothed
  )
})

