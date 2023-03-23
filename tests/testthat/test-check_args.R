test_that("specifying a custom EpiEstim config in `prm.R` triggers a warning", {
  prm.R.1 <- list(
    config.EpiEstim = EpiEstim::make_config(t_start = c(6))
  )
  expect_warning(check_prm.R(prm.R.1))
})

test_that("check_prm.R returns a warning message and a value of NULL
          when users passes their own config for R calculations", {
            prm.R = list(
              CI = 0.95,
              window = 10,
              config.EpiEstim = EpiEstim::make_config(seed = 15)
            )
            expect_warning(
              check_prm.R(prm.R, silent = FALSE)
            )
            expect_warning(
              check_prm.R(prm.R, silent = FALSE),
              NULL
            )
          }
)

test_that("check_prm.smooth returns an error when method is not specified or
          valid, returns an error when window or span is not specified or valid,
          and returns NULL when valid prm.smooth parameters are passed", {
            prm.smooth.valid.rm = list(
              window = 14,
              align = "center",
              method = "rollmean"
            )
            prm.smooth.valid.loess = list(
              method = "loess",
              span = 1
            )
            expect_equal(
              check_prm.smooth(prm.smooth.valid.rm),
              NULL
            )
            expect_equal(
              check_prm.smooth(prm.smooth.valid.loess),
              NULL
            )
            prm.smooth.missing.method =
              purrr::discard_at(prm.smooth.valid.loess, "method")
            expect_error(
              check_prm.smooth(prm.smooth.missing.method)
            )
            prm.smooth.invalid.method =
              purrr::list_modify(prm.smooth.valid.loess, method = "rollloess")
            expect_error(
              check_prm.smooth(prm.smooth.invalid.method)
            )
            prm.smooth.missing.window = purrr::discard_at(prm.smooth.valid.rm,
                                                          "window")
            expect_error(
              check_prm.smooth(prm.smooth.missing.window)
            )
            prm.smooth.invalid.window = purrr::list_modify(prm.smooth.valid.rm,
                                                           window = "14")
            expect_error(
              check_prm.smooth(prm.smooth.invalid.window)
            )
            prm.smooth.missing.span = purrr::discard_at(prm.smooth.valid.loess,
                                                        "span")
            expect_error(
              check_prm.smooth(prm.smooth.missing.span)
            )
            prm.smooth.invalid.span = purrr::list_modify(prm.smooth.valid.loess,
                                                         span = "1")
            expect_error(
              check_prm.smooth(prm.smooth.invalid.span)
            )
          })

test_that("check_dist returns an error when invalid distributions are
          passed, and returns NULL when valid distribution is passed", {
            fec = def_dist_fecal_shedding()
            fec.missing.shape = purrr::discard_at(fec, "shape")
            fec.sd = purrr::list_modify(fec,
                                        sd = 2)
            expect_error(
              check_dist(fec.missing.shape)
            )
            expect_error(
              check_dist(fec.sd)
            )
            expect_equal(
              check_dist(fec),
              NULL
            )
          }
)

test_that("check_for_deconv returns an error when number of observations <
          length of distribution vector, and returns NULL when obs >=
          length(dist)", {
            fec = get_discrete_dist(
              def_dist_fecal_shedding()
            )
            n.obs = 1:33
            n.obs.error = n.obs[-1]
            expect_error(
              check_for_deconv(
                obs = n.obs.error,
                dist = fec
              )
            )
            expect_equal(
              check_for_deconv(
                obs = n.obs,
                dist = fec
              ),
              NULL
            )
          })

test_that("check_data_clin returns an error when date and count columns
          are missing, and returns NULL when both columns are present in
          dateframe", {
            dat = data.frame(
              date = as.Date(character()),
              count = integer()
            )
            dat.rm.date = dplyr::select(dat, -date)
            dat.rm.count = dplyr::select(dat, -count)
            expect_error(
              check_data_clin(dat.rm.date)
            )
            expect_error(
              check_data_clin(dat.rm.count)
            )
            expect_equal(
              check_data_clin(dat),
              NULL
            )
          })
