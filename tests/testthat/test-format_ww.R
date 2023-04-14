test_that("format_ww.smooth() returns a dataframe of the correct format", {

  ww.conc <- data.frame(
    date = as.Date("2023-04-14"),
    value = 3.5
  )

  ww.smooth <- format_ww.smooth(ww.conc)

  expect_true(
    "data.frame" %in% class(ww.smooth),
  )

  expect_identical(
    names(ww.smooth),
    c("t", "obs", "date")
  )

})
