n <- 4
start_date <- as.Date("2023-04-11")
date <- seq(start_date, start_date + lubridate::days(n-1), by = 1)
value <- (1:n*10)^2
cl.daily <- tibble::tibble(
  id = 1,
  date = date,
  t = 1:n,
  value = value
)

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
