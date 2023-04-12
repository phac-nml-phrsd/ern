n <- 100
start_date <- as.Date("2023-01-05")
reports.daily <- tibble::tibble(
  date = seq(start_date, start_date + lubridate::days(n-1), by = 1),
  value = rpois(n, 100)
)

# estimate_R_cl_single() --------------------------------------------------

test_that("estimate_R_cl_single() returns output of the expected format", {
  res <- ern::estimate_R_cl_single(
    cl.daily = cl.daily,
    dist.repfrac = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub = dist.incub,
    dist.gi = dist.gi,
    prm.R = prm.R
  )

  expect_s3_class(res, "data.frame")

  col_name <- c("date", "mean", "lo", "hi", "I")

  expect_equal(
    names(res),
    col_name
  )

  expect_equal(
    class(res$date),
    "Date"
  )

  for (i in 2:length(col_name)){
    expect_equal(
      class(res[, col_name[i]]),
      "numeric"
    )
  }
})

# reports_to_incidence ----------------------------------------------------

test_that("reports_to_incidence() returns messages when silent mode is off", {
  expect_message(reports_to_incidence(
    reports.daily = reports.daily,
    reporting.delay = dist.repdelay,
    incubation.period = dist.incub,
    silent = FALSE
  ))
})

test_that("reports_to_incidence() returns output of the expected format", {
  res <- reports_to_incidence(
    reports.daily = reports.daily,
    reporting.delay = dist.repdelay,
    incubation.period = dist.incub,
    silent = TRUE
  )

  test_output_tibble(
    res,
    col_name = c("date", "I"),
    col_class = c("Date", "numeric")
  )
})


# correct_underreporting() ------------------------------------------------

test_that("underreporting is properly corrected", {
  reporting.fraction <- 0.2
  expect_equal(
    correct_underreporting(reports.daily, reporting.fraction)$value[2],
    reports.daily$value[2]/reporting.fraction)
})

# deconv ------------------------------------------------------------------

dist <- c(0.04874081, 0.09006900, 0.11158173,
          0.11735695, 0.11284041, 0.10251266,
          0.08955388, 0.07602285, 0.06313891,
          0.05154077, 0.04148944, 0.03301525,
          0.02601865, 0.02033632, 0.01578237)

test_that("deconv triggers error when length of distribution vector is greater than number of observations", {
  expect_error(deconv(
    counts = rpois(length(dist)-1, 100),
    dist = dist,
    silent = TRUE
  ))
})

test_that("deconv returns messages when silent mode is off", {
  expect_message(deconv(
    counts = rpois(length(dist)+5, 100),
    dist = dist,
    max.iter = 10,
    silent = FALSE
  ))
})

test_that("deconv returns output of the expected format", {
  res <- deconv(
    counts = rpois(length(dist)+5, 100),
    dist = dist,
    max.iter = 10,
    silent = TRUE
  )

  expect_equal(
    names(res),
    c("t", "y")
  )
})
