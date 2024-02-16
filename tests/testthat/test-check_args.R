# get current defaults from estimate_R_cl as formals
defaults <- formals(estimate_R_cl)

# prm.daily ---------------------------------------------------------

# evaluate defaults
prm.daily <- eval(defaults$prm.daily)

test_that("check_prm.daily() fails when `method` is missing", {
  prm.daily2 = prm.daily
  prm.daily2$method <- NULL
  expect_error(check_prm.daily(prm.daily2))
})

test_that("check_prm.daily() fails when `method` name is unknown", {
  expect_error(check_prm.daily(purrr::list_modify(prm.daily, 
                                                  method = "foobar")))
})

test_that("check_prm.daily() fails when `renewal` mandatory elements are missing",{
  prm.daily2 = prm.daily
  prm.daily2$method = 'renewal'
  for(name in c("burn", "iter", "chains",
                "prior_R0_shape", "prior_R0_rate",
                "prior_alpha_shape", "prior_alpha_rate")){
    p = prm.daily2
    p[[name]] <- NULL
    expect_error(check_prm.daily(p))
  }
})

test_that("check_prm.daily() fails when list items are of wrong type", {
  p = prm.daily
  p$method <- "renewal"
  expect_error(check_prm.daily(purrr::list_modify(p, burn = "2")))
  expect_error(check_prm.daily(purrr::list_modify(p, iter = -2)))
  expect_error(check_prm.daily(purrr::list_modify(p, chains = 0.5)))
  expect_error(check_prm.daily(purrr::list_modify(p, first.agg.period = "-2")))
})

test_that("check_prm.daily() returns NULL when all checks are passed", {
  expect_null(check_prm.daily(prm.daily))
})

# prm.daily.check ---------------------------------------------------------

prm.daily.check <- eval(defaults$prm.daily.check)

test_that("check_prm.daily.check fails when list items are of wrong type", {
  expect_error(check_prm.daily.check(purrr::list_modify(prm.daily.check, agg.reldiff.tol = "20")))
  expect_error(check_prm.daily.check(purrr::list_modify(prm.daily.check, agg.reldiff.tol = -20)))
})

test_that("check_prm.daily returns NULL when NULL list is input", {
  expect_null(check_prm.daily.check(NULL))
})

test_that("check_prm.daily returns NULL when all checks are passed", {
  expect_null(check_prm.daily.check(prm.daily.check))
})


# prm.smooth --------------------------------------------------------------

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

            prm.smooth.invalid.window = purrr::list_modify(prm.smooth.valid.rm,
                                                           window = -14)
            expect_error(
              check_prm.smooth(prm.smooth.invalid.window)
            )

            prm.smooth.invalid.align = purrr::list_modify(prm.smooth.valid.rm,
                                                          align = "up")

            expect_error(
              check_prm.smooth(prm.smooth.invalid.align)
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

            prm.smooth.invalid.span = purrr::list_modify(prm.smooth.valid.loess,
                                                         span = -100)
            expect_error(
              check_prm.smooth(prm.smooth.invalid.span)
            )
          })



test_that("specifying a custom EpiEstim config in `prm.R` triggers a message", {
  expect_message(check_prm.R(
    purrr::list_modify(prm.R,
                       config.EpiEstim = EpiEstim::make_config(t_start = c(6)))))
})

test_that("check_prm.R fails when mandatory elements are missing", {
  for(name in c("iter", "CI", "window")){
    expect_error(check_prm.R(prm.R[setdiff(names(prm.R), name)]))
  }
})

test_that("check_prm.R fails when list items are of wrong type", {
  expect_error(check_prm.R(purrr::list_modify(prm.R, iter = "2")))
  expect_error(check_prm.R(purrr::list_modify(prm.R, CI = "2")))
  expect_error(check_prm.R(purrr::list_modify(prm.R, CI = 2)))
  expect_error(check_prm.R(purrr::list_modify(prm.R, window = "2")))
})

test_that("check_prm.R returns a message and a value of NULL
          when users passes their own config for R calculations", {
  expect_message(
    check_prm.R(
      purrr::list_modify(prm.R,
                         config.EpiEstim = EpiEstim::make_config(t_start = c(6)))
      , silent = FALSE)
  )
  expect_null(
    check_prm.R(
      purrr::list_modify(prm.R,
                         config.EpiEstim = EpiEstim::make_config(t_start = c(6)))
                , silent = TRUE)
  )
})


# distributions -----------------------------------------------------------

test_that("check_dist() returns an error when invalid distributions are passed, and returns NULL when valid distribution is passed", {

  # gamma
  dist.gamma = dist.fec
  dist.gamma.missing.shape = purrr::discard_at(dist.gamma, "shape")
  dist.gamma.sd = purrr::list_modify(dist.gamma, sd = 2)
  out <- capture_output(expect_error(
    check_dist(dist.gamma.missing.shape))
  ) # suppress additional printing in error
  out <- capture_output(expect_error(
    check_dist(dist.gamma.sd)
  ))
  out <- capture_output(expect_error(
    check_dist(norm.invalid.sd)
  ))
  out <- capture_output(expect_error(
    check_dist(lnorm.invalid.sdlog)
  ))
  expect_equal(
    check_dist(dist.gamma),
    NULL
  )

  # norm

  invalid.norm = purrr::list_modify(dist.norm, sd = -10)
  invalid.lnorm = purrr::list_modify(dist.lnorm, sdlog = -10)

  out <- capture.output(expect_error(
    check_dist(invalid.norm)
  ))
  out <- capture.output(expect_error(
    check_dist(invalid.lnorm)
  ))
  expect_equal(
    check_dist(dist.norm),
    NULL
  )
  expect_equal(
    check_dist(dist.lnorm),
    NULL
  )
})


# deconv ------------------------------------------------------------------

test_that("check_for_deconv returns an error when number of observations <
          length of distribution vector, and returns NULL when obs >=
          length(dist)", {
  fec = get_discrete_dist(
    dist.fec
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

# cl.input ---------------------------------------------------------------

test_that("check_cl.input_format() returns an error when date and count columns
          are missing, and returns NULL when both columns are present in
          dateframe", {
  dat = data.frame(
    date = as.Date(character()),
    value = integer()
  )
  dat.rm.date = dplyr::select(dat, -date)
  dat.rm.value = dplyr::select(dat, -value)
  expect_error(
    check_cl.input_format(dat.rm.date)
  )
  expect_error(
    check_cl.input_format(dat.rm.count)
  )
  expect_equal(
    check_cl.input_format(dat),
    NULL
  )
})

test_that("expected output of check_df.input_daily()", {

  # logical check
  expect_equal(
    class(check_df.input_daily(
      cl.input
    )),
    "logical"
  )

  # FALSE check
  expect_false(
    check_df.input_daily(
      cl.input
    )
  )

  # TRUE check
  expect_true(
    check_df.input_daily(
      cl.daily
    )
  )
})


# ww.conc -----------------------------------------------------------------

test_that("check_ww.conc_format() returns an error when df is missing a date or value columns", {
  load("../testdata/ww_test_params.RData")
  df.missingdate = dplyr::select(ww.conc, -date)
  df.missingval = dplyr::select(ww.conc, -value)
  expect_error(check_ww.conc_format(df.missingdate))
  expect_error(check_ww.conc_format(df.missingval))
})
