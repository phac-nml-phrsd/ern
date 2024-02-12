
#' @title Extract MCMC chains from a JAGS object
#'
#' @param chain Integer. Chain number.
#' @param jags.obj JAGS object as returned by `code.sample()`
#'
#' @return A dataframe of the chain values for selected parameters.
#'
extract_mcmc_values <- function(chain, jags.obj) {
  a  = jags.obj[[chain]]
  cn = colnames(a)
  
  # Arbitrary taking some times. 
  # Taking _all_ the times would be too much to display.
  # Giving the choice to user may be overwhelming. 
  
  t = stringr::str_extract(cn, '\\d+') |> as.integer()
  t.max = max(t, na.rm = TRUE)
  t1 = round(1*t.max/6)
  t2 = round(2*t.max/6)
  t3 = round(3*t.max/6)
  t4 = round(4*t.max/6)
  t5 = round(5*t.max/6)
  
  # Extract the parameters
  res = a[,c('R0', 'alpha', 
             'I[3]', 
             paste0('I[',t1,']'), 
             paste0('I[',t2,']'), 
             paste0('I[',t3,']'), 
             paste0('I[',t4,']'), 
             paste0('I[',t5,']'), 
             paste0('I[',t.max,']'))] |> 
    as.data.frame() |>
    dplyr::mutate(chain = chain, 
           iter = dplyr::row_number())

  return(res)
}


#' @title Plot MCMC traces
#'
#' @param jags.obj JAGS object as returned by `code.sample()`
#'
#' @return A `ggplot` plot.
#'
plot_traces <- function(jags.obj) {
  
  df = lapply(seq_along(jags.obj), extract_mcmc_values, jags.obj=jags.obj) |>
    dplyr::bind_rows() |>
    dplyr::mutate(chain = as.factor(chain))|>
    tidyr::pivot_longer(-c(iter, chain)) |>
    # More explicit variable names
    dplyr::mutate(name = ifelse(grepl('I\\[',name), 
                         stringr::str_replace(name,'I\\[','daily.inc\\['),
                         name))
    
    
  g = ggplot2::ggplot(df, ggplot2::aes(x=iter, y=value, color = chain)) + 
    ggplot2::geom_line(alpha = 0.5) + 
    ggplot2::facet_wrap(~name, scales = 'free_y') + 
    ggplot2::theme(panel.grid= ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.8)),
          strip.background = ggplot2::element_rect(fill = 'steelblue4'),
          strip.text = ggplot2::element_text(color = 'white')) + 
    ggplot2::labs(title = 'MCMC diagnostic', subtitle = 'Traces plot',
         x = 'MCMC iteration (post burn-in)') + 
    ggplot2::scale_color_brewer(palette = 'Dark2')
  
  return(g)  
}


#' @title Plot the Gelman Rubin statistic for all parameters.
#'
#' @param jags.obj JAGS object as returned by `code.sample()`
#'
#' @return A `ggplot` plot.
#'
plot_gelman_rubin <- function(jags.obj) {
  
  a = coda::gelman.diag(jags.obj)  
  df = data.frame(a$psrf)
  
  # Clearer parameter names
  x = rownames(df)
  y = stringr::str_extract(x, '\\d+') |>
    stringr::str_pad(width=3, side = 'left', pad=0)
  xx = stringr::str_replace(x, '\\[\\d+',paste0('[',y )) |> 
    stringr::str_replace('I\\[', 'daily.inc\\[')
  df$parameter <- xx
  
  
  g = ggplot2::ggplot(df, ggplot2::aes(x=parameter, y=`Point.est.`)) + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, 
                                          vjust = 0., size = ggplot2::rel(0.7)),
      panel.grid.minor.y = ggplot2::element_blank()) +
    ggplot2::geom_hline(yintercept = 1, linetype = 'dashed') + 
    ggplot2::geom_segment(ggplot2::aes(xend = parameter, 
                                       y =`Point.est.`, 
                                       yend = `Upper.C.I.`))+
    ggplot2::geom_point()+
    ggplot2::labs(y='point estimate and upper CI',
                  title = 'MCMC diagnostic', 
                  subtitle = paste0('Gelman Rubin statistic',
                                    '\n(multivariate potential scale reduction factor = ',
                                    round(a$mpsrf,3),')'))
  return(g)
}


