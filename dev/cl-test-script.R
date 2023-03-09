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

if(0){
dat = readRDS('dev/tmp.rds') %>% dplyr::mutate(count = round(count/20))
dat = dat[1:6,]
popsize = 1e7
pathogen = 'sarscov2'
}

if(1){

  pt = 'AB'
  vg = 'RSV'
  pathogen = vg

  dat = readRDS('dev/RVDSS.rds') %>%
    ungroup() %>%
    filter(season == '2022/2023',
           prov == pt,
           virus.group == vg,
           type == 'npos',
           between(date,
                   ymd('2022-10-25'),
                   ymd('2023-01-22'))) %>%
    rename(count = v)

  ggplot(dat,aes(x=date, y=count))+geom_line()+geom_point()+
    labs(title=paste(vg, pt))

  get_popsize <- function(prov) {

    res = switch(EXPR = prov,
                 BC = 5e6,
                 AB = 4e6,
                 SK = 1e6,
                 MB = 1e6,
                 ON = 14e6,
                 QC = 8e6
    )
    return(res)
  }
  popsize = get_popsize(pt)
}

max.dists = 10 # need to truncate distributions if you're using a very short timeseries

dist.repdelay = list(
  dist = 'gamma',
  mean = 5,
  mean_sd = 1,
  sd = 1,
  sd_sd = 0.1,
  max = 10
)
dist.repfrac = list(
  dist = "unif",
  min = 0.1,
  max = 0.3
)
dist.incub   = def_dist_incubation_period(pathogen)
dist.incub$max = max.dists # need if we're talking fewer data points
dist.gi      = def_dist_generation_interval(pathogen)
dist.gi$max = max.dists

prm.daily = list(
  burn = 300,
  iter = 300,
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
  silent = TRUE
)

pd = plot_diagnostic_cl(r.estim)
plot(pd)

p2 <- ggplot2::ggplot(r.estim$R,ggplot2::aes(x = date)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  ggplot2::geom_line(ggplot2::aes(y = mean)) +
  ggplot2::scale_x_date(limits = range(dat$date)) +
  ggplot2::labs(title = "estimated Rt") +
  ggplot2::theme(axis.title = ggplot2::element_blank())

print(p1 + p2 + patchwork::plot_layout(ncol = 1))
