# distributions used in testing

dist.fec = ern::def_dist(
  dist = "gamma",
  mean = 12.90215,
  mean_sd = 1.136829,
  shape = 1.759937,
  shape_sd = 0.2665988,
  max = 33
)
dist.norm = ern::def_dist(
  dist = "norm",
  mean = 2,
  sd = 1,
  max = 10
)
dist.lnorm = ern::def_dist(
  dist = "lnorm",
  meanlog = 2,
  sdlog = 1,
  max = 10
)
dist.repfrac = ern::def_dist(
  dist = "unif",
  min = 0.1,
  max = 0.3
)
dist.repdelay = ern::def_dist(
  dist = 'gamma',
  mean = 6.99,
  mean_sd = 0.2211,
  sd = 3.663,
  sd_sd = 0.1158,
  max = 21
)
dist.incub = ern::def_dist(
  dist     = "gamma",
  mean     = 3.49,
  mean_sd  = 0.1477,
  shape    = 8.5,
  shape_sd = 1.8945,
  max      = 8
)
dist.gi = ern::def_dist(
  dist     = "gamma",
  mean     = 6.84,
  mean_sd  = 0.7486,
  shape    = 2.39,
  shape_sd = 0.3573,
  max      = 15
)
