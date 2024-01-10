# prm lists used in testing

prm.daily = list(
  burn = 1,
  iter = 1,
  chains = 1,
  prior_R0_shape = 2,
  prior_R0_rate = 0.6,
  prior_alpha_shape = 1,
  prior_alpha_rate = 1
)
prm.daily.check = list(
  agg.reldiff.tol = 10
)
prm.smooth = list(
  method = "rollmean",
  window = 7,
  align = "center"
)
prm.R = list(
  iter = 1,
  CI = 0.95,
  window = 7,
  config.EpiEstim = NULL
)
