#' Diagnostic plot for R estimation from wastewater
#'
#' @param r.estim Object as returned from \code{estimate_R_ww()}.
#' @param caption String. Optional caption.
#'
#' @return A ggplot object.
#' @export
#'
#' @seealso [estimate_R_ww()]
#'
#' @examples 1+1

plot_diagnostic_ww <- function(r.estim, caption=NULL) {

  ggplot2::theme_set(theme_bw())
  date.start = r.estim$date.start

  xsc = scale_x_date(limits = c(lubridate::ymd(date.start),today()))

  g.ww = r.estim$ww.conc %>%
    filter(date >= date.start) %>%
    ggplot(aes(x = date, y = val)) +
    geom_step() +
    geom_line(
      data = r.estim$ww.smooth,
      aes(y = obs),
      color = 'steelblue4',
      size = 1,
      alpha = 0.5
    ) + xsc+
    labs(title = 'Wastewater concentration',
         x='collection date', y='concentration')

  g.inc = r.estim$inc %>%
    ggplot(aes(x=date, y = inc.deconvol)) +
    geom_line()+
    labs(title='Deconvoluted incidence', x = 'infection date', y='cases')+
    xsc

  g.r = r.estim$R %>%
    ggplot(aes(x=date, y=mean)) +
    geom_hline(yintercept = 1, color = 'grey50', linetype='dashed')+
    geom_ribbon(aes(ymin=qvlo, ymax = qvhi), alpha=0.2)+
    geom_line() + xsc +
    labs(title = 'Effective Reproduction Number')


  g = patchwork::wrap_plots(g.ww, g.inc, g.r, ncol=1)

  if(!is.null(caption)) g=g+labs(caption=caption)

  return(g)

}
