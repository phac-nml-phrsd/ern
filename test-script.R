library(ern)
library(tidyverse)

siteid = 'TAB'
date.start =  "2022-10-15"

ww.conc = readRDS("../nml-azure-to-r/out/nml-database.RDS") %>%
  clean_ww(gene_target = "covN2") %>%
  filter(ww.loc == siteid, date >= date.start)

# ---- Parameters ----


prm.smooth = list(
  window = 14,
  align  = 'center',
  method = 'loess',
  span   = 0.20
)

dist.fec = ern::get_fecal_shedding('sarscov2')
dist.gi  = ern::get_gi('sarscov2')
dist.gi$sd <- 1
dist.gi$sd_sd <- 0.1
# ---- Estimation ----

r.estim = ern::estimate_R_ww(
  ww.conc        = ww.conc,
  dist.fec       = dist.fec,
  dist.gi        = dist.gi,
  scaling.factor = 2,
  prm.smooth     = prm.smooth
)


g = ern::plot_diagnostic_ww(r.estim, caption = siteid)
plot(g)
