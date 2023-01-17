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
