
#' @title Deconvoluting Wastewater Data to Incidence
#' @description Function estimates incidence from smoothed data
#' @param d ww dataframe
#' @param fec fecal shedding distribution
#' @param scaling.factor scaling factor of ww data.
#' @return Dataframe with deconvoluted incidence

deconv_ww_inc <- function(d, fec, scaling.factor){

  d$obs_scal = d$obs * scaling.factor

  start_date = as.Date(first(d$date))

  inc = deconvolution_RL(observed = d$obs_scal,
                         times = d$t,
                         p_delay = fec) %>%
    # Forces incidence to be a positive integer:
    mutate(inc.deconvol = as.integer(ifelse(RL_result>0, RL_result, 0))) %>%
    # Retreive corresponding dates:
    rename(t = time) %>%
    filter(t > 0) %>%
    mutate(date = start_date + t)

  res = list(
    inc         = inc,
    ww.smooth   = d,
    fec         = fec,
    scaling.factor = scaling.factor
  )
  return(res)
}
