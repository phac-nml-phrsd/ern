test_that("get_dist can detect valid and invalid distributions", {
  mean = 1
  sd = 2
  max = 100
  
  expect_no_error(
    def_dist(
      dist = "gamma",
      mean = mean,
      mean_sd = NA,
      sd = sd,
      sd_sd = NA,
      max = max
    )
  )
  
  expect_error(
    def_dist(
      dist = "logamma",
      mean = mean,
      mean_sd = NA,
      sd = sd,
      sd_sd = NA,
      max = max
    )
  )
})

test_that("gamma, norm, and lnorm distributions get specified correctly", {
  mean = 1
  sd = 2
  max = 100

  pars.sd <- list(
    dist = "gamma",
    mean = mean,
    mean_sd = NA,
    sd = sd,
    sd_sd = NA,
    max = max
  )

  pars.shape <- list(
    dist = "gamma",
    mean = mean,
    mean_sd = NA,
    shape = mean^2/sd^2,
    sd_sd = NA,
    max = max
  )

  pars.norm <- list(
    dist = "norm",
    mean = mean,
    sd = sd,
    max = max
  )

  pars.lnorm <- list(
    dist = "lnorm",
    meanlog = log(mean),
    sdlog = log(sd),
    max = max
  )

  expect_equal(
    get_discrete_dist(
      pars.sd
  ),
    get_discrete_dist(
      pars.shape
    )
  )

  expect_no_error(
    get_discrete_dist(
      pars.norm
    )
  )

  expect_no_error(
    get_discrete_dist(
      pars.lnorm
    )
  )
})

test_that("get_discrete_dist detects invalid distribution", {
  mean = 1
  sd = 2
  max = 100

  pars <- list(
    dist = "gammanorm",
    mean = mean,
    sd = sd,
    max = max
  )

  expect_error(
    get_discrete_dist(
      pars
    )
  )
})

test_that("sample_from_dist returns error when uniform distribution not defined,
          and returns numeric vector when uniform distribution defined",
          {
            param.error = list(
              dist = "gamma",
              min = 2,
              max = 200
            )
            param.correct = purrr::list_modify(param.error,
                                               dist = "unif")
            expect_error(
              sample_from_dist(
                n = 200,
                params = param.error
              )
            )

            expect_vector(
              sample_from_dist(
                n = 200,
                params = param.correct
              )
            )
          }
)
