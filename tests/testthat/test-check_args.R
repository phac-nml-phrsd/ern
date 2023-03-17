test_that("specifying a custom EpiEstim config in `prm.R` triggers a warning", {
  prm.R.1 <- list(
    config.EpiEstim = EpiEstim::make_config(t_start = c(6))
  )
  expect_warning(check_prm.R(prm.R.1))
})
