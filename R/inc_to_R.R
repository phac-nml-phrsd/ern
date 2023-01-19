#' @title Estimating Rt from incidence
#' @description Function estimates Rt from incidence data
#'
#' @param df dataframe
#' @param gi generation interval
#'
#' @importFrom rlang .data
#'
#' @return dataframe with Rt estimates
inc_to_R <- function(df, gi){

  dat = df %>%
    dplyr::mutate(I = .data$inc.deconvol) %>%
    select(I, date, t) %>%
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

  est_df = tmp$R %>%
    dplyr::left_join(dat, by = c("t_start" = "t")) %>%
    dplyr::rename(
      mean   = .data$`Mean(R)`,
      median = .data$`Median(R)`,
      qvlo   = .data$`Quantile.0.025(R)`,
      qvhi   = .data$`Quantile.0.975(R)`,
      sd     = .data$`Std(R)`
    )

  return(est_df)
}
