test_that("estimate_R_ww detects missing variables in ww.conc",{
  load("../testdata/ww_test_params.RData")
  ww.conc = dplyr::select(ww.conc, -c("date"))
  expect_error(
    estimate_R_ww(ww.conc,
                  dist.fec,
                  dist.gi),
    "date and value columns are required. Please check ww.conc.
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
  expect_type(
    inc2R_one_iter(
      i = 1,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      ww.conc = ww.smooth,
      scaling.factor = 1,
      prm.R = prm.R,
      silent = TRUE
    ),
    "list"
  )
  expect_length(
    inc2R_one_iter(
      i = 1,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      ww.conc = ww.smooth,
      scaling.factor = 1,
      prm.R = prm.R,
      silent = TRUE
    ),
    2
  )
})

test_that("estimate_R_ww returns a list of four dataframes", {
  load("../testdata/ww_test_params.RData")
  expect_type(
    estimate_R_ww(
      ww.conc = ww.conc,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      prm.smooth = prm.smooth
    ),
    "list"
  )
  expect_length(
    estimate_R_ww(
      ww.conc = ww.conc,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      prm.smooth = prm.smooth
    ),
    4
  )
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
  # prm.list <- c("prm.R", "prm.smooth")

  for(prm.name in prm.list){
    ww <- names(eval(defaults_ww[[prm.name]]))
    cl <- names(eval(defaults_cl[[prm.name]]))

    expect_equal(ww, cl)
  }
})
