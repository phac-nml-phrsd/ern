test_that("plot_diagnostic_ww returns an object that has a class that includes
          'ggplot' and 'patchwork'", {
            load("../testdata/ww_test_params.RData")
            r.obj = estimate_R_ww(
              ww.conc = ww.conc,
              dist.fec = dist.fec,
              dist.gi = dist.gi,
              prm.smooth = prm.smooth,
              silent = TRUE
            )
            g = plot_diagnostic_ww(r.obj, caption = "test")
            expect_s3_class(
              g,
              "patchwork"
            )
            expect_s3_class(
              g,
              "ggplot"
            )
})

# test_that("plot_diagnostic_cl returns an object that has a class that includes
#           'ggplot' and 'patchwork'", {
#
#   # define inputs
#   # - - - - - - - - - - - - - - - - -
#   dat <- ern::cl.agg %>% dplyr::filter(pt == "bc") %>% dplyr::slice(1:4)
#   pathogen = 'sarscov2'
#   max.dists = 10 # need to truncate distributions if you're using a very short timeseries
#
#   dist.repdelay = list(
#     dist = 'gamma',
#     mean = 5,
#     mean_sd = 1,
#     sd = 1,
#     sd_sd = 0.1,
#     max = 10
#   )
#   dist.repfrac = list(
#     dist = "unif",
#     min = 0.1,
#     max = 0.3
#   )
#   dist.incub   = ern::def_dist_incubation_period(pathogen)
#   dist.incub$max = max.dists # need if we're talking fewer data points
#   dist.gi      = ern::def_dist_generation_interval(pathogen)
#   dist.gi$max = max.dists
#
#   popsize = 1e7
#
#   prm.daily = list(
#     burn = 1,
#     iter = 2,
#     chains = 1
#   )
#   prm.smooth = list(
#     method = "rollmean",
#     window = 3
#   )
#   prm.R = list(
#     iter = 10, # number of iterations in Rt ensemble
#     CI = 0.95, # 95% confidence interval
#     window = 7, # time window for each Rt estimate
#     config.EpiEstim = EpiEstim::make_config(
#       seed = 14
#     )
#   )
#   prm.daily.check = list(
#     agg.reldiff.tol = 200
#   )
#
#   # compute Rt and make plot
#   # - - - - - - - - - - - - - - - - -
#   r.obj = ern::estimate_R_cl(
#     cl.agg        = dat,
#     dist.repdelay = dist.repdelay,
#     dist.repfrac  = dist.repfrac,
#     dist.incub    = dist.incub,
#     dist.gi       = dist.gi,
#     popsize       = popsize,
#     prm.smooth    = prm.smooth,
#     prm.daily     = prm.daily,
#     prm.R         = prm.R,
#     prm.daily.check = prm.daily.check,
#     silent = TRUE
#   )
#
#   g = ern::plot_diagnostic_cl(r.obj)
#   expect_s3_class(
#     g,
#     "patchwork"
#   )
#   expect_s3_class(
#     g,
#     "ggplot"
#   )
# })
