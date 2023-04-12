cl.daily.test <- (cl.daily %>% dplyr::transmute(
  date,
  count = value
))

test_that("format_cl.daily() returns output in the expected format", {
  test_output_tibble(
    format_cl.daily(cl.daily.test),
    col_name = c("id", "date", "t", "value"),
    col_class = c("integer", "Date", "integer", "numeric"   ))
})

test_that("time index is attached to daily input data", {
  res <- attach_t(
    cl.daily.test
  )

  expect_true(
    "t" %in% names(res)
  )

  expect_identical(
    res$t,
    1:nrow(res)
  )
})
