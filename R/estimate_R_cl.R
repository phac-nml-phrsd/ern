#' @title Estimate the effective reproduction from clinical report data
#'
#' @inheritParams weekly_to_daily
#' @inheritParams smooth_cl
#' @inheritParams estimate_R_cl_rep
#' @param prm.daily.check list. parameters for checking weekly to daily report inference. set this parameter to `NULL` to use inferred daily reports as is. list elements include:
#' - `agg.reldiff.tol`: numerical tolerance (%) for relative error between aggregated inferred daily reports and original weekly reports. observations outside of this tolerance are dropped.
#'
#' @importFrom magrittr %>%
#'
#' @return List. Elements include:
#' - `cl.weekly` original weekly reports signal
#' - `cl.input` reports as input for Rt calculation (inferred daily counts, smoothed)
#' - `R` the effective R estimate (summary from ensemble)
#' @export
estimate_R_cl <- function(
  cl.weekly,
  dist.repdelay,
  dist.repfrac,
  dist.incub,
  dist.gi,
  popsize,
  prm.daily = list(
    burn = 500,
    iter = 2e3,
    chains = 20
  ),
  prm.daily.check = list(
    agg.reldiff.tol = 10
  ),
  prm.smooth = list(
    window = 7
  ),
  prm.R = list(
    iter = 10
  )
) {

  # attach time-index column to observed weekly reports
  cl.weekly <- attach_t_agg(cl.weekly)

  # estimate daily reports using JAGS model
  cl.daily = weekly_to_daily(
    cl.weekly = cl.weekly,
    dist.gi   = dist.gi,
    popsize   = popsize,
    prm.daily = prm.daily
  )

  # smooth daily reports before deconvolutions
  cl.smooth = smooth_cl(
    cl.daily   = cl.daily,
    prm.smooth = prm.smooth
  )

  # trim smoothed reports based on relative error criterion
  if(!is.null(prm.daily.check)){
    if(is.null(prm.daily.check)) stop("please specify agg.reldiff.tol in prm.daily.check")
    cl.use.dates = get_use_dates(
      cl.smooth,
      cl.weekly,
      prm.daily.check$agg.reldiff.tol
    )
    cl.input = (cl.smooth
                %>% dplyr::filter(date %in% cl.use.dates)
    )
  } else {
    cl.input = cl.smooth
  }

  # estimate Rt many times and return summary
  R = estimate_R_cl_rep(
    cl.input      = cl.input,
    dist.repfrac  = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub    = dist.incub,
    dist.gi       = dist.gi,
    prm.R         = prm.R
  )

  # Calculate the aggregated incidence
  # from the inferred daily incidence:
  inferred.aggreg = get_use_dates(
    reports.daily   = cl.input,
    reports         = cl.weekly,
    agg.reldiff.tol = Inf,
    dates.only      = FALSE ) %>%
    dplyr::filter(!is.na(obs)) %>%
    dplyr::select(date, obs, matches('agg$'))

  res = list(
    cl.weekly  = cl.weekly,
    cl.input = cl.input,
    inferred.aggreg = inferred.aggreg,
    R = R
  )

  return(res)
}
