devtools::load_all()
suppressMessages({
  library(magrittr)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(stringr)
  library(patchwork)
})
set.seed(1234)
sim.data = 1

if(sim.data){
  r0 = 1.4
  hz = 70
  popsize = 2000
  pathogen = 'FLUA'

  dist.gi = def_dist_generation_interval(pathogen)
  gi = get_discrete_dist(dist.gi)
  # gi = c(0.05, 0.2, 0.35, 0.20, 0.2)
  sum(gi)
  ng = length(gi)
  s = inc = numeric(hz)
  inc[1:ng] = rpois(ng, (1:ng)*r0)
  s[1:ng] = popsize - inc[1:ng]
  t = ng+1

  for(t in (ng+1):hz ){
    q = gi * inc[(t-1):(t-ng)]
    inc[t] = round(sum(q) * r0 * s[t-1]/popsize)
    s[t] = s[t-1] - inc[t]
  }
  dailyinc = data.frame(t=1:hz, inc= inc) %>%
    mutate(date = ymd('2022-01-01')+t)
  dat = dailyinc %>%
    mutate(count = cumsum(inc),
           wk = t %/% 7 +1) %>%
    group_by(wk) %>%
    summarise(count = sum(inc)) %>%
    mutate(date = ymd('2022-01-01')+ wk*7) %>%
    filter(wk < 6)


    g = ggplot(dat, aes(x=date, y=count)) +
      geom_step()+ geom_point()+
      geom_line(data=dailyinc, aes(x=date, y=inc))+
      geom_point(data=dailyinc, aes(x=date, y=inc))
    # g
}

max.dists = 10 # need to truncate distributions if you're using a very short timeseries

dist.repdelay = list(
  dist = 'gamma',
  mean = 3,
  mean_sd = 1,
  sd = 1,
  sd_sd = 0.1,
  max = 10
)
dist.repfrac = list(
  dist = "unif",
  min = 0.99,
  max = 0.999
)
dist.incub   = def_dist_incubation_period(pathogen)
# dist.incub$mean <- 8
dist.incub$max = max.dists # need if we're talking fewer data points
dist.gi      = def_dist_generation_interval(pathogen)
dist.gi$max = max.dists

prm.daily = list(
  burn = 300,
  iter = 300,
  chains = 2
)
prm.smooth = NULL# list(window = 1)
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

# # ---DEBUG
# dist.gi =  list(
#   dist = "gamma",
#   mean = 5 ,
#   mean_sd = 0.7486,
#   shape = 2.39,
#   shape_sd = 0.3573,
#   max = 10)
# ---END DEBUG

r.estim = estimate_R_cl(
  cl.agg        = dat,
  dist.repdelay = dist.repdelay,
  dist.repfrac  = dist.repfrac,
  dist.incub    = dist.incub,
  dist.gi       = dist.gi,
  popsize       = popsize,
  prm.smooth    = prm.smooth,
  prm.daily     = prm.daily,
  prm.R         = prm.R,
  prm.daily.check = prm.daily.check,
  silent = FALSE
)

z = r.estim$cl.daily %>%
  group_by(date) %>%
  summarise(m = mean(value))


gg = ggplot(z, aes(x=date, y=m)) +
  geom_line()+ geom_point()+
  geom_point(data=dailyinc, aes(x=date, y=inc), shape=4)+
  scale_x_date(date_minor_breaks = '1 day')
gg
# pd = plot_diagnostic_cl(r.estim)
# plot(pd)

