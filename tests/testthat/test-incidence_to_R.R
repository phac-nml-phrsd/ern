test_that("incidence_to_R detects invalid config.EpiEstim", {
  config = EpiEstim::make_config(t_start = c(6))
  config = config[names(config) != "si_distr"]
  prm.R.invalid = purrr::list_modify(prm.R,
                                     config.EpiEstim = config)
  expect_error(
    incidence_to_R(incidence, dist.gi, prm.R.invalid)
  )
})

test_that("incidence_to_R returns a dataframe", {
  expect_s3_class(
    incidence_to_R(incidence, dist.gi, prm.R),
    "data.frame"
  )
})
