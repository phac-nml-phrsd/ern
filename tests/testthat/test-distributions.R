test_that("distributions are initialized correctly", {
  for (f in ls(getNamespace("ern"), pattern = "def_dist_")) {
    expect_type(get(f)(), "list")
    expect_equal(names(get(f)())[1], "dist")
  }
})

test_that("gamma distributions get specified correctly", {
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

  expect_equal(
    get_discrete_dist(
      pars.sd
  ),
    get_discrete_dist(
      pars.shape
    )
  )
})

test_that("def_dist_fecal_shedding detects incorrect pathogen",
          {
            p = "sick"
            expect_error(def_dist_fecal_shedding(pathogen = p),
                         "Pathogen not found. Aborting!")
          })
