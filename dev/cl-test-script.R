devtools::load_all()
library(magrittr)

dat = readRDS('dev/tmp.rds') %>% dplyr::mutate(count = round(count/20))
dat = dat[1:6,]

max.dists = 10 # need to truncate distributions if you're using a very short timeseries

dist.repdelay = list(
  dist = 'lnorm',
  mean = 1.5,
  mean_sd = 0.05,
  sd = 0.64,
  sd_sd = 0.05,
  max = max.dists
)
dist.repfrac = def_dist_reporting_fraction()
dist.incub   = def_dist_incubation_period()
dist.incub$max = max.dists # need if we're talking fewer data points
dist.gi      = def_dist_generation_interval()
dist.gi$max = max.dists

prm.daily = list(
  burn = 30,
  iter = 30,
  chains = 2
)
prm.smooth = list(window = 7)
prm.R = list(
  iter = 2,
  # window = 10, # if window isn't specified, assume 7 day est window for Rt
  config.EpiEstim = EpiEstim::make_config(
    seed = 14
  )
)

# prm.daily.check = NULL
prm.daily.check = list(agg.reldiff.tol = 200)

p1 <- ggplot2::ggplot(dat,ggplot2::aes(x=date, y=count)) +
  ggplot2::geom_line()+ggplot2::geom_point() +
  ggplot2::labs(title = "weekly reports") +
  ggplot2::theme(axis.title = ggplot2::element_blank())

r.estim = estimate_R_cl(
  cl.agg        = dat,
  dist.repdelay = dist.repdelay,
  dist.repfrac  = dist.repfrac,
  dist.incub    = dist.incub,
  dist.gi       = dist.gi,
  popsize       = 1e7,
  prm.smooth    = prm.smooth,
  prm.daily     = prm.daily,
  prm.R         = prm.R,
  prm.daily.check = prm.daily.check
)

# DC: this is broken
# pd = plot_diagnostic_cl(r.estim)


p2 <- ggplot2::ggplot(r.estim$R,ggplot2::aes(x = date)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  ggplot2::geom_line(ggplot2::aes(y = mean)) +
  ggplot2::scale_x_date(limits = range(dat$date)) +
  ggplot2::labs(title = "estimated Rt") +
  ggplot2::theme(axis.title = ggplot2::element_blank())

print(p1 + p2 + patchwork::plot_layout(ncol = 1))
