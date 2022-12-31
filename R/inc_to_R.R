#' @title Estimating Rt from incidence
#' @description Function estimates Rt from incidence data
#'
#' @param df dataframe
#' @param start.date start date of data
#' @param gi generation interval
#'
#' @return dataframe with Rt estimates

inc_to_R <- function(df, start.date, gi){

  dat = df %>%
    dplyr::mutate(I = inc.deconvol) %>%
    dplyr::select(date, I) %>%
    dplyr::filter(date >= start.date) %>%
    tidyr::drop_na()

  config = EpiEstim::make_config(list(
    mean_si     = gi$mean,
    std_mean_si = gi$mean_sd,
    min_mean_si = gi$mean - 2*gi$mean_sd,
    max_mean_si = gi$mean + 2*gi$mean_sd,
    std_si      = gi$sd,
    std_std_si  = gi$sd_sd,
    min_std_si  = gi$sd - 2*gi$sd_sd,
    max_std_si  = gi$sd + 2*gi$sd_sd,
    n1 = 200,
    n2 = 50
    ))

  tmp = EpiEstim::estimate_R(incid  = dat,
                             method = 'uncertain_si',
                             config = config)

  start_date = min(dat$date)

  est_df = tmp$R %>%
    dplyr::mutate(date = as.Date(start_date) + t_end) %>%
    dplyr::rename(
      mean   = `Mean(R)`,
      median = `Median(R)`,
      qvlo   = `Quantile.0.025(R)`,
      qvhi   = `Quantile.0.975(R)`,
      sd     = `Std(R)`
    )

  return(est_df)
}
