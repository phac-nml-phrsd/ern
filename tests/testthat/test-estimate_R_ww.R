test_that("estimate_R_ww detects missing variables in ww.conc",{
  load("../testdata/ww_test_params.RData")
  ww.conc = dplyr::select(ww.conc, -c("date"))
  expect_error(
    estimate_R_ww(ww.conc,
                  dist.fec,
                  dist.gi),
    "`date` and `value` columns are required. Please check `ww.conc`.
         Aborting!")

})

test_that("inc2R_one_iter returns a list of two dataframes", {
  load("../testdata/ww_test_params.RData")
  prm.R = list(
    CI = 0.95,
    window = 10,
    config.EpiEstim = NULL
  )
  ww.smooth = smooth_ww(ww.conc = ww.conc,
                        prm.smooth = prm.smooth)

  res = inc2R_one_iter(
    i = 1,
    dist.fec = dist.fec,
    dist.gi = dist.gi,
    ww.conc = ww.smooth,
    scaling.factor = 1,
    prm.R = prm.R,
    silent = TRUE,
    RL.max.iter = 9
  )

  expect_type(res, "list")
  expect_length(res,2)
})

test_that("estimate_R_ww returns a list of four dataframes", {
  load("../testdata/ww_test_params.RData")

  res = estimate_R_ww(
    ww.conc = ww.conc,
    dist.fec = dist.fec,
    dist.gi = dist.gi,
    prm.smooth = prm.smooth)

  expect_type(res ,"list")
  expect_length(res,4)
})

test_that("estimate_R_ww returns a message when silent mode is disabled", {
  load("../testdata/ww_test_params.RData")
  expect_message(
    estimate_R_ww(
      ww.conc = ww.conc,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      prm.smooth = prm.smooth,
      silent = FALSE
    )
  )
})

test_that("estimate_R_ww executes silently when silent mode is enabled", {
  load("../testdata/ww_test_params.RData")
  expect_silent(
    estimate_R_ww(
      ww.conc = ww.conc,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      prm.smooth = prm.smooth,
      silent = TRUE
    )
  )
})

test_that("defaults common between estimate_R_ww and estimate_R_cl have the same structure", {
  defaults_ww <- formals(estimate_R_ww)
  defaults_cl <- formals(estimate_R_cl)

  prm.list <- c("prm.R")

  for(prm.name in prm.list){
    ww <- names(eval(defaults_ww[[prm.name]]))
    cl <- names(eval(defaults_cl[[prm.name]]))

    expect_equal(ww, cl)
  }
})

test_that("smoothing is turned off for `estimate_R_ww()` when `prm.smooth = NULL`", {
  load("../testdata/ww_test_params.RData")

  # make daily data for checks where smoothing (and therefore interpolation) is turned off
  ww.conc.daily <- (ww.conc |> dplyr::select(date, value) |> tidyr::complete(date = seq(min(date), max(date), by = "days")) |> tidyr::fill(value))

  expect_warning(
    res <- estimate_R_ww(
    ww.conc = ww.conc.daily,
    dist.fec = dist.fec,
    dist.gi = dist.gi,
    prm.smooth = NULL
  ))

  expect_equal(
    res$ww.conc$value,
    res$ww.smooth$obs
  )
})

test_that("estimate_R_ww() returns an error when wastewater concentration data is not daily and smoothing is turned off", {
  expect_error(
    estimate_R_ww(
      ww.conc = data.frame(date = as.Date(c("2023-04-21", "2023-04-23")),
                           value = c(0.4, 0.6)),
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      prm.smooth = NULL
    )
  )
})
