test_that("prm.daily defaults are added", {
  prm.daily <- list()

  expect_named(
    add_defaults_prm.daily(prm.daily),
    expected = c("burn", "iter", "chains"),
    ignore.order = TRUE
  )
})

test_that("prm.R defaults are added", {
  prm.R <- list(
    iter = 2
  )

  expect_named(
    add_defaults_prm.R(prm.R),
    expected = c("iter", "CI", "window"),
    ignore.order = TRUE
  )
})

test_that("generic default-setting works", {
  x <- list(
    string = "original"
  )

  # default value is added for missing element
  expect_equal(set_default(x, "number", 4),
               list(
                 string = "original",
                 number = 4
               ))

  # value is already specified
  expect_equal(set_default(x, "string", "wrong"),
               list(
                 string = "original"
               ))

  # name isn't string
  expect_error(set_default(x, 3, 3))
})
