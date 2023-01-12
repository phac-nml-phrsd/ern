#' Diagnostic plot for R estimation from wastewater
#'
#' @param r.estim Object as returned from \code{estimate_R_ww()}.
#' @param caption String. Optional caption.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @seealso [estimate_R_ww()]
plot_diagnostic_ww <- function(r.estim, caption=NULL) {

  ggplot2::theme_set(ggplot2::theme_bw())
  date.start = r.estim$date.start

  xsc = ggplot2::scale_x_date(
    limits = c(lubridate::ymd(date.start), lubridate::today())
  )

  g.ww = r.estim$ww.conc %>%
    dplyr::filter(date >= date.start) %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = .data$val)) +
    ggplot2::geom_step() +
    ggplot2::geom_line(
      data = r.estim$ww.smooth,
      ggplot2::aes(y = .data$obs),
      color = 'steelblue4',
      size = 1,
      alpha = 0.5
    ) +
    xsc +
    ggplot2::labs(
      title = 'Wastewater concentration',
      x='collection date', y='concentration'
    )

  g.inc = r.estim$inc %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y = .data$inc.deconvol)) +
    ggplot2::geom_line()+
    ggplot2::labs(title='Deconvoluted incidence', x = 'infection date', y='cases')+
    xsc

  g.r = r.estim$R %>%
    ggplot2::ggplot(ggplot2::aes(x=date, y=mean)) +
    ggplot2::geom_hline(yintercept = 1, color = 'grey50', linetype='dashed')+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$qvlo, ymax = .data$qvhi), alpha=0.2)+
    ggplot2::geom_line() +
    xsc +
    ggplot2::labs(title = 'Effective Reproduction Number')


  g = patchwork::wrap_plots(g.ww, g.inc, g.r, ncol=1)

  if(!is.null(caption)) g = g + ggplot2::labs(caption=caption)

  return(g)

}

#' Diagnostic plot for R estimation from clinical data
#'
#' @param r.estim Object as returned from \code{estimate_R_cl()}.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom patchwork plot_layout
# need this to get the S3 method "/"
#' @importFrom rlang .data
#'
#' @seealso [estimate_R_cl()]
plot_diagnostic_cl <- function(
    r.estim
){

  alpha_scale <- c(0.1, 0.3)
  linetype_scale <- c("dotted", "solid")
  names(alpha_scale) <- names(linetype_scale) <- c("FALSE", "TRUE")

  ggplot2::theme_set(ggplot2::theme_bw())

  th <- ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = NA),
    legend.text = ggplot2::element_text(size = 8),
    legend.margin = ggplot2::margin(t = -5, r = 5, b = 0, l = 0),
    legend.spacing = ggplot2::unit(0, units = "pt"),
    panel.spacing = ggplot2::unit(5, units = "pt"),
    plot.margin = ggplot2::margin(t=5, r=0, b=5, l=0, unit="pt")
  )

  # Rt plot
  # -------------------------

  ylim <- (r.estim$R
   %>% dplyr::filter(.data$use)
   %>% tidyr::pivot_longer(c(.data$lwr, .data$upr))
   %>% dplyr::pull(.data$value)
   %>% range()
  )

  p1 <- (ggplot2::ggplot(r.estim$R, ggplot2::aes(x = date))
   + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", na.rm = TRUE)
   + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lwr, ymax = .data$upr,
                          alpha = .data$use),
                          na.rm = TRUE)
   + ggplot2::geom_line(ggplot2::aes(y = .data$med, linetype = .data$use),
                        linewidth = 1, na.rm = TRUE)
   + ggplot2::scale_alpha_manual(values = alpha_scale)
   + ggplot2::scale_linetype_manual(values = linetype_scale)
   + ggplot2::coord_cartesian(ylim = ylim)
   + ggplot2::guides(alpha = "none", linetype = "none")
   + ggplot2::labs(title = paste0("Effective reproduction number"))
   + th
  )

  # original input (weekly cases)
  # -------------------------
  p2 <- (ggplot2::ggplot(
    (r.estim$cl.weekly
     %>% dplyr::filter(dplyr::between(date, min(r.estim$R$date), max(r.estim$R$date)))),
     ggplot2::aes(x = date, y = .data$count))
     + ggplot2::geom_col(na.rm = TRUE)
     + ggplot2::scale_x_date(limits = c(min(r.estim$R$date), max(r.estim$R$date)))
     + ggplot2::labs(subtitle = "Original signal: weekly case reports")
     + th
  )

  # inferred input (smoothed daily cases)
  # -------------------------
  p3 <- (ggplot2::ggplot(
    (r.estim$cl.input
     %>% summarise_by_date()
     %>% dplyr::filter(dplyr::between(date, min(r.estim$R$date), max(r.estim$R$date)))),
         ggplot2::aes(x = date))
     + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lwr, ymax = .data$upr), alpha = alpha_scale[2])
     + ggplot2::geom_line(ggplot2::aes(y = .data$med), linewidth = 1)
     + ggplot2::labs(subtitle = "Inferred signal: daily case reports (smoothed)")
     + th
  )

  # composite plot
  # -------------------------
  p1 / p2 / p3
}
