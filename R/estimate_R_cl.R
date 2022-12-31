#' @title Estimate the effective reproduction from clinical report data
#'
#' @inheritParams weekly_to_daily
#' @inheritParams smooth_cl
#' @inheritParams estimate_R_cl_rep
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
  prm.smooth = list(
    window = 7
  ),
  prm.R = list(
    iter = 10
  )
) {

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
  cl.use.dates = get_use_dates(
    cl.smooth,
    cl.weekly
  )
  cl.input = (cl.smooth
    %>% dplyr::filter(date %in% cl.use.dates)
  )

  # estimate Rt many times and return summary
  R = estimate_R_cl_rep(
    cl.input      = cl.input,
    dist.repfrac  = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub    = dist.incub,
    dist.gi       = dist.gi,
    prm.R         = prm.R
  )

  # return
  list(
    cl.weekly  = cl.weekly,
    cl.input = cl.input,
    R = R
  )
}
