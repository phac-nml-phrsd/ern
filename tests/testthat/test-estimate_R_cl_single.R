test_that("underreporting is properly corrected", {
  reports.daily <- tibble::tibble(
    date = as.Date("2023-01-05", "2023-01-06"),
    value = c(4, 8)
  )
  reporting.fraction <- 0.2
  expect_equal(
    correct_underreporting(reports.daily, reporting.fraction)$value[2],
    reports.daily$value[2]/reporting.fraction)
})
