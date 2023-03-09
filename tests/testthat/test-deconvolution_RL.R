test_that("deconvolution_RL returns a message when verbose = TRUE,
          returns a warning when p_delay > 1, returns a dataframe for default
          parameters and with right_censor = FALSE", {
  load("../testdata/ww_test_params.RData")
  ww.smooth = smooth_ww(df = ww.conc,
                        prm.smooth = prm.smooth)
  f = get_discrete_dist(def_dist_fecal_shedding())
  f.warn = f + 0.01
  expect_message(
    deconvolution_RL(observed = ww.smooth$obs,
                     times = ww.smooth$t,
                     p_delay = f,
                     verbose = TRUE)
  )
  expect_warning(
    deconvolution_RL(observed = ww.smooth$obs,
                     times = ww.smooth$t,
                     p_delay = f.warn,
                     verbose = TRUE)
  )
  expect_s3_class(
    deconvolution_RL(observed = ww.smooth$obs,
                     times = ww.smooth$t,
                     p_delay = f),
    "data.frame"
  )
  expect_s3_class(
    deconvolution_RL(observed = ww.smooth$obs,
                     times = ww.smooth$t,
                     p_delay = f,
                     right_censor = FALSE),
    "data.frame"
  )
})

test_that("na_to_0 returns a warning message and a vector with no NAs
          when a vector containing NAs is passed", {
            v = c(0, NA, 12, 13, NA, 14, 15)
            expect_warning(
              na_to_0(v)
            )
            expect_vector(
              na_to_0(v)
            )
            expect_equal(
              any(is.na(na_to_0(v))),
              FALSE
            )
          })
