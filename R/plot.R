#' Diagnostic plot for R estimation from wastewater data
#'
#' @param r.estim List. Output of [estimate_R_ww()].
#' @param caption Character. Optional plot caption.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @seealso [estimate_R_ww()] [plot_diagnostic_cl()]
#'
#' @examples 
#' 
#' # Load data of viral concentration in wastewater
#' data("ww.input")
#' 
#' # Estimate Rt based on wastewater data
#' x = estimate_R_ww(
#'   ww.conc  = ww.input,
#'   dist.fec = ern::def_dist(
#'     dist = "gamma",
#'     mean = 12.90215,
#'     mean_sd = 1.136829,
#'     shape = 1.759937,
#'     shape_sd = 0.2665988,
#'     max = 33
#'     ),
#'   dist.gi  = ern::def_dist(
#'     dist     = "gamma",
#'     mean     = 6.84,
#'     mean_sd  = 0.7486,
#'     shape    = 2.39,
#'     shape_sd = 0.3573,
#'     max      = 15
#'     ), 
#'   silent   = TRUE
#' )
#' 
#' # Diagnostic plot
#' g = plot_diagnostic_ww(x)
#' plot(g)
#'
#'
plot_diagnostic_ww <- function(r.estim, caption=NULL) {

  ggplot2::theme_set(ggplot2::theme_bw())
  date.start = min(r.estim$R$date)
  date.end   = max(r.estim$R$date)

  xsc = ggplot2::scale_x_date(
    limits = c(lubridate::ymd(date.start),
               lubridate::ymd(date.end))
  )

  g.ww = r.estim$ww.conc |>
    dplyr::filter(date >= date.start) |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_step() +
    ggplot2::geom_line(
      data = r.estim$ww.smooth,
      ggplot2::aes(y = obs),
      color = 'steelblue4',
      linewidth = 1,
      alpha = 0.5
    ) +
    xsc +
    ggplot2::labs(
      title = 'Wastewater concentration',
      x='collection date', y='concentration'
    )

  g.inc = r.estim$inc |>
    ggplot2::ggplot(ggplot2::aes(x=date, y = mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha=0.2)+
    ggplot2::geom_line()+
    ggplot2::labs(title ='Inferred incidence',
                  x = 'infection date', y='infections')+
    xsc

  g.r = r.estim$R |>
    ggplot2::ggplot(ggplot2::aes(x=date, y=mean)) +
    ggplot2::geom_hline(yintercept = 1, color = 'grey50', linetype='dashed')+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha=0.2)+
    ggplot2::geom_line() +
    xsc +
    ggplot2::labs(title = 'Effective Reproduction Number')


  g = patchwork::wrap_plots(g.ww, g.inc, g.r, ncol=1)

  if(!is.null(caption)) g = g + ggplot2::labs(caption=caption)

  return(g)
}

#' Diagnostic plot for R estimation from clinical report data
#'
#' @param r.estim List. Output of [estimate_R_cl()].
#'
#' @return A `ggplot` object
#' @export
#'
#' @importFrom patchwork plot_layout
# need this to get the S3 method "/"
#'
#' @seealso [estimate_R_cl()]
#' 
#' 
#' 
#' @examples 
#' 
#' #' # -- THIS EXAMPLE TAKES ABOUT 30 SECONDS TO RUN --
#' 
#' # Load SARS-CoV-2 reported cases in Ontario
#' # during the Omicron wave
#' data('cl.input')
#' dat = cl.input[cl.input$pt == 'on' & 
#'                  cl.input$date > as.Date('2021-11-30') & 
#'                  cl.input$date < as.Date('2022-01-31'),] 
#' 
#' # Estimate Rt
#' \dontrun{
#' x = estimate_R_cl(
#'   cl.input = dat,
#'   dist.repdelay = ern::def_dist(
#'     dist = 'gamma',
#'     mean = 6.99,
#'     mean_sd = 0.2211,
#'     sd = 3.663,
#'     sd_sd = 0.1158,
#'     max = 21
#'     ), 
#'   dist.repfrac = ern::def_dist(
#'     dist = "unif",
#'     min = 0.1,
#'     max = 0.3
#'     ),
#'   dist.incub = ern::def_dist(
#'     dist     = "gamma",
#'     mean     = 3.49,
#'     mean_sd  = 0.1477,
#'     shape    = 8.5,
#'     shape_sd = 1.8945,
#'     max      = 8
#'     ),
#'   dist.gi = ern::def_dist(
#'     dist     = "gamma",
#'     mean     = 6.84,
#'     mean_sd  = 0.7486,
#'     shape    = 2.39,
#'     shape_sd = 0.3573,
#'     max      = 15
#'     ),
#'   popsize = 14e6, # population of Ontario in 2023
#'   prm.daily = list(
#'     # Very low number of MCMC iterations
#'     # for this example to run fast.
#'     # Increase `burn`, `iter` and `chains` 
#'     # for better accuracy
#'     burn = 50, iter = 50, chains = 1, 
#'     # first.agg.period = NULL,
#'     prior_R0_shape = 2, prior_R0_rate = 0.6, 
#'     prior_alpha_shape = 1, prior_alpha_rate = 1),
#'   silent = TRUE
#' )
#' 
#' # Diagnostic plot for Rt estimates 
#' # from clinical data
#' plot_diagnostic_cl(x)
#' }
#' 
plot_diagnostic_cl <- function(
    r.estim
){
  
  # ==== plot setup ====
  
  alpha <- 0.3 # for CI ribbons
  # linetype_scale <- c("dotted", "solid")
  # names(alpha_scale) <- names(linetype_scale) <- c("FALSE", "TRUE")
  
  ggplot2::theme_set(ggplot2::theme_bw())
  
  th <- ggplot2::theme(
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(fill = NA),
    legend.text = ggplot2::element_text(size = 8),
    legend.margin = ggplot2::margin(t = -5, r = 5, b = 0, l = 0),
    legend.spacing = ggplot2::unit(0, units = "pt"),
    panel.spacing = ggplot2::unit(5, units = "pt"),
    plot.margin = ggplot2::margin(t=5, r=0, b=5, l=0, unit="pt")
  )
  
  # to maintain consistent x-axis between panels
  date.range <- range(r.estim$R$date)
  
  # ==== Observed data (optionally vs inferred aggregates) ====
  p1 <- (r.estim$cl.input
         |> ggplot2::ggplot(ggplot2::aes(x=date, y=value)) 
         + ggplot2::geom_col() 
         + ggplot2::labs(
           title = 'Observed case reports',
           x = 'report date',
           y = 'cases'
         )
         + ggplot2::coord_cartesian(xlim = date.range)
         + th
  )
  
  # ==== Modified input (smoothed daily cases, optionally inferred) ====
  
  p2 <- (r.estim$cl.daily 
         |> summarise_by_date_iters()
         |> ggplot2::ggplot(ggplot2::aes(x = date)) 
         + ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr),
                                alpha = alpha) 
         + ggplot2::geom_line(ggplot2::aes(y = mean)) 
         + ggplot2::coord_cartesian(xlim = date.range)
         + th
  )
  
  if(!is.null(r.estim$inferred.agg)){
    
    # add diagnostic plot
    p3 <- (r.estim$inferred.agg 
           |> ggplot2::ggplot(ggplot2::aes(x=date)) 
           + ggplot2::geom_point(ggplot2::aes(y=obs), size=2) 
           + ggplot2::geom_line(ggplot2::aes(y=obs))
           + ggplot2::geom_line(ggplot2::aes(y=mean.agg), color= 'red2', alpha=0.3)
           + ggplot2::geom_pointrange(
             ggplot2::aes(y=mean.agg, ymin=lwr.agg, ymax=upr.agg),
             color= 'red2', alpha=0.6)
           + ggplot2::coord_cartesian(xlim = date.range)
           + ggplot2::labs(
             title = 'Aggregated case reports: observed (black) vs. inferred (red)',
             x = 'report date',
             y = 'cases'
           ) 
           + th
    )
    
    p2 <- ((p2 
            + ggplot2::labs(
              title = "Daily case reports (smoothed and inferred)",
              x = 'report date',
              y = "cases"
            )
    ) / p3)
    
    # panel heights for final plot
    heights <- c(1,2.5,1)
  } else {
    p2 <- (p2 
           + ggplot2::labs(
             title = "Daily case reports (smoothed)",
             x = 'report date',
             y = "cases"
           )
    )
    
    # panel heights for final plot
    heights <- rep(1,3)
  }
  
  # ==== Rt plot ====
  
  p3 <- (r.estim$R 
         |> tidyr::drop_na(date)
         |> ggplot2::ggplot(ggplot2::aes(x = date))
         + ggplot2::geom_hline(yintercept = 1, linetype = "dashed", na.rm = TRUE)
         + ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr),
                                alpha = alpha,
                                na.rm = TRUE)
         + ggplot2::geom_line(ggplot2::aes(y = mean), na.rm = TRUE)
         + ggplot2::labs(
           title = "Effective Reproduction Number",
           x = 'date',
           y = "mean"
         )
         + th
  )
  
  # ==== composite plot ====
  g = patchwork::wrap_plots(p1,p2,p3, ncol=1, heights = heights)
  
  return(g)
}




#' Plot a distribution
#'
#' @param d List that defines the distribution (as returned by `def_dist_incubation_period()` for example)
#'
#' @return A ggplot object.
#' @export
#' 
#' @examples 
#' # Define a `ern` distribution:
#' gi  = ern::def_dist(
#'   dist     = "gamma",
#'   mean     = 6.84,
#'   mean_sd  = 0.7486,
#'   shape    = 2.39,
#'   shape_sd = 0.3573,
#'   max      = 15
#'   )
#' 
#' # Plot can be customized like any `ggplot` object:
#' g = plot_dist(gi) + ggplot2::labs(subtitle = 'your subtitle')
#' plot(g)
#' 
#' 
plot_dist <- function(d) {

  a = get_discrete_dist(d)

  emp.mean = sum(c(1:d$max)*a)

  dplot = data.frame(
    x = 1:d$max,
    y = a
  )

  g = ggplot2::ggplot(dplot, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_line(linewidth = 2) +
    ggplot2::geom_point(size=3, fill='white', shape = 21, stroke = 1) +
    ggplot2::geom_vline(xintercept = emp.mean, linetype = 'dashed')+
    ggplot2::annotate(geom = 'label', x=emp.mean*1, y=0,
                      label = paste('mean =',round(emp.mean,2)), size=3)+
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())+
    ggplot2::scale_x_continuous(breaks = 0:d$max)+
    ggplot2::labs(title = d$dist, x='',y='')

  return(g)
}


