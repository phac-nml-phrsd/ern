suppressMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2) ; theme_set(theme_bw())
  library(lubridate)
  library(stringr)
  library(patchwork)
})

if(0){
  tok = as.character(read.table('ghtoken.txt'))
  devtools::install_github('phac-nml-phrsd/ern',
                           auth_token = tok,
                           force = TRUE)
}

library(ern)  ; packageVersion('ern')
source('utils.R')


date.start = today() - 120

dat = fetch_clin_data(virus = 'FLUA') %>%
  filter(between(date, date.start, today()-10))

provs = c('ON', 'BC')

dat = filter(dat, prov == provs[1])
plot_clin_data(dat)


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
dist.incub$max = max.dists # need if we're taking fewer data points
dist.gi      = def_dist_generation_interval()
dist.gi$max = max.dists

prm.daily = list(
  burn = 100,
  iter = 100,
  chains = 2
)
prm.smooth = list(window = 7)
prm.R = list(
  CI = 0.95,
  iter = 2,
  window = 10, # if window isn't specified, assume 7 day est window for Rt
  config.EpiEstim = EpiEstim::make_config(
    seed = 14
  )
)

# prm.daily.check = NULL
prm.daily.check = list(agg.reldiff.tol = 200)


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

g.r = r.estim$R %>%
  ggplot(aes(x=date, y=mean))+
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha= 0.3)
g.r

g.d = plot_diagnostic_cl(r.estim)
plot(g.d)
