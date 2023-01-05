#' @title Deconvoluting Wastewater Data to Incidence
#' @description Function estimates incidence from smoothed data
#'
#' @param d ww dataframe
#' @param fec fecal shedding distribution
#' @param scaling.factor scaling factor of ww data.
#'
#' @importFrom rlang .data
#'
#' @return Dataframe with deconvoluted incidence
deconv_ww_inc <- function(d, fec, scaling.factor){

  d$obs_scal = d$obs * scaling.factor

  start_date = as.Date(dplyr::first(d$date))

  inc = deconvolution_RL(observed = d$obs_scal,
                         times = d$t,
                         p_delay = fec) %>%
    # Forces incidence to be a positive integer:
    dplyr::mutate(
      inc.deconvol = as.integer(ifelse(
        .data$RL_result>0, .data$RL_result, 0))) %>%
    # Retreive corresponding dates:
    dplyr::rename(t = .data$time) %>%
    dplyr::filter(t > 0) %>%
    dplyr::mutate(date = start_date + t)

  res = list(
    inc         = inc,
    ww.smooth   = d,
    fec         = fec,
    scaling.factor = scaling.factor
  )
  return(res)
}
