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

  tmp = EpiEstim::estimate_R(incid  = dat,
                             method = 'si_from_sample',
                             si_sample = matrix(c(0, get_discrete_dist(gi))))

  est_df = tmp$R %>%
    dplyr::left_join(dat, by = c("t_end" = "t")) %>%
    dplyr::rename(
      mean   = .data$`Mean(R)`,
      median = .data$`Median(R)`,
      qvlo   = .data$`Quantile.0.025(R)`,
      qvhi   = .data$`Quantile.0.975(R)`,
      sd     = .data$`Std(R)`
    )

  return(est_df)
}
