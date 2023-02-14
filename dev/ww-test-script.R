suppressMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2) ; theme_set(theme_bw())
  library(lubridate)
  library(stringr)
  library(patchwork)
})

devtools::load_all()

fakedata = 0

if(!fakedata){
  source('../ern-dev/utils.R')
  siteid = 'TAB'
  date.start =  "2022-10-15"
  ww.conc = readRDS("../nml-azure-to-r/out/nml-database.RDS") %>%
    clean_ww(gene_target = "covN2") %>%
    filter(ww.loc == siteid, date >= date.start)
}

if(fakedata){
  set.seed(1234)
  n=60
  x=1:n

  val = x * (40 - x) * rnorm(n, mean=1, sd=0.02)
  val = val[val>0]
  val
  dd = ymd('2022-11-01') + 1:length(val)
  dd

  ww.conc = data.frame(date = dd, val=val)
}
# ww.conc %>% ggplot(aes(x=date, y=val)) + geom_point()

# ---- Parameters ----
prm.smooth = list(
  align  = 'center',
  method = 'loess',
  span   = 0.29
)

# prm.R = list(
#   config.EpiEstim = NULL
# )
prm.R = list(
  window = 10,
  config.EpiEstim = EpiEstim::make_config(seed = 15)
)

dist.fec = ern::get_fecal_shedding('sarscov2')

z = c( rep(1,6), c(2,3,3,3,2), rep(1,5))
# dist.fec = z/sum(z)
# plot(dist.fec, typ='o')

dist.gi  = ern::def_dist_generation_interval(pathogen = "sarscov2")

# ---- Estimation ----

r.estim = ern::estimate_R_ww(
  ww.conc        = ww.conc,
  dist.fec       = dist.fec,
  dist.gi        = dist.gi,
  scaling.factor = 1,
  prm.smooth     = prm.smooth,
  prm.R = prm.R
)


# r.estim$R %>% ggplot(aes(x=date, y=mean)) + geom_line()

g = ern::plot_diagnostic_ww(r.estim, caption = ifelse(fakedata,'fake',siteid))
plot(g)

r.estim$R %>%
  select(date, mean) %>%
  filter(date > ymd('2022-11-20'))

