suppressMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2) ; theme_set(theme_bw())
  library(lubridate)
  library(stringr)
  library(patchwork)
})
xsc = scale_x_date(limits = c(ymd('2022-10-15'), today()), date_breaks = '2 weeks')

gfoo = ggplot(ww.conc, aes(x=date)) +
  geom_step(aes(y=val))+
  geom_line(data=inc$ww.smooth, aes(y=obs), color='steelblue1')+
  geom_line(data=inc$inc, aes(y=inc.deconvol), color='red')+
  xsc

gr = ggplot(est_df, aes(x=date, y=mean)) +
  geom_line()+
  geom_hline(yintercept = 1)+
  xsc


gfoo / gr
