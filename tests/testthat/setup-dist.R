# distributions used in testing

dist.fec = ern::def_dist_fecal_shedding()
dist.norm = list(
  dist = "norm",
  mean = 2,
  sd = 1,
  max = 10
)
dist.lnorm = list(
  dist = "lnorm",
  meanlog = 2,
  sdlog = 1,
  max = 10
)
dist.repfrac = suppressWarnings(ern::def_dist_reporting_fraction())
dist.repdelay = ern::def_dist_reporting_delay()
dist.incub = ern::def_dist_incubation_period()
dist.gi = ern::def_dist_generation_interval()
