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
    config.EpiEstim = EpiEstim::make_config(seed = 15)
  )
  ww.smooth = smooth_ww(df = ww.conc,
                        prm.smooth = prm.smooth)
  expect_type(
    inc2R_one_iter(
      i = 1,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      wastewater = ww.smooth,
      scaling.factor = 1,
      prm.R = prm.R
    ),
    "list"
  )
  expect_length(
    inc2R_one_iter(
      i = 1,
      dist.fec = dist.fec,
      dist.gi = dist.gi,
      wastewater = ww.smooth,
      scaling.factor = 1,
      prm.R = prm.R
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
