# prm lists used in testing

prm.daily = list(
  burn = 1,
  iter = 1,
  chains = 1
)
prm.daily.check = list(
  agg.reldiff.tol = 10
)
prm.smooth = list(
  method = "rollmean",
  window = 7
)
prm.R = list(
  iter = 1,
  CI = 0.95,
  window = 7,
  config.EpiEstim = NULL
)
