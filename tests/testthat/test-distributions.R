test_that("distributions are initialized correctly", {
  for (f in ls(getNamespace("ern"), pattern = "def_dist_")) {
    expect_type(get(f)(), "list")
    expect_equal(names(get(f)())[1], "dist")
  }
})

test_that("distributions that require pathogen are initialized correctly,
          and return an error when an invalid pathogen is specified", {
  pathogens = c("sarscov2", "influenza", "rsv")
  for (p in pathogens) {
    expect_type(
      def_dist_incubation_period(pathogen = p), "list")
    expect_type(
      def_dist_generation_interval(pathogen = p), "list")
    expect_type(
      def_dist_fecal_shedding(pathogen = p), "list")
  }
  p.error = "sick"
  expect_error(
    def_dist_incubation_period(pathogen = p.error)
  )
  expect_error(
    def_dist_generation_interval(pathogen = p.error)
  )
  expect_error(
    def_dist_fecal_shedding(pathogen = p.error)
  )
})

test_that("gamma and lnorm distributions get specified correctly", {
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

  pars.lnorm <- list(
    dist = "lnorm",
    mean = mean,
    sd = sd,
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
          })
