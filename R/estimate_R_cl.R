#' @title Estimate the effective reproduction from clinical report data
#'
#' @inheritParams agg_to_daily
#' @inheritParams smooth_cl
#' @inheritParams estimate_R_cl_rep
#' @param prm.daily.check list. parameters for checking aggregated to daily report inference. set this parameter to `NULL` to use inferred daily reports as is. list elements include:
#' - `agg.reldiff.tol`: numerical tolerance (%) for relative error between aggregated inferred daily reports and original aggregated reports. observations outside of this tolerance are dropped.
#'
#' @importFrom magrittr %>%
#'
#' @return List. Elements include:
#' - `cl.agg` original aggregated reports signal
#' - `cl.daily` reports as input for Rt calculation (inferred daily counts, smoothed)
#' - `R` the effective R estimate (summary from ensemble)
#' @export
estimate_R_cl <- function(
  cl.agg,
  dist.repdelay,
  dist.repfrac,
  dist.incub,
  dist.gi,
  popsize,
  prm.daily = list(
    burn = 500,
    iter = 2e3,
    chains = 20,
    first.agg.period = NULL
  ),
  prm.daily.check = list(
    agg.reldiff.tol = 10
  ),
  prm.smooth = list(
    window = 7
  ),
  prm.R = list(
    iter = 10,
    window = 7,
    config.EpiEstim = NULL
  )
) {

  # Checking arguments
  check_prm.R(prm.R)

  # ==== Aggregated -> daily reports ====

  # attach time-index column to observed aggregated reports
  cl.agg <- attach_t_agg(
    cl.agg,
    first.agg.period = prm.daily$first.agg.period
  )

  # estimate daily reports using JAGS model
  cl.daily.raw = agg_to_daily(
    cl.agg = cl.agg,
    dist.gi   = dist.gi,
    popsize   = popsize,
    prm.daily = prm.daily
  )

  # ==== Smooth daily reports =====

  # smooth daily reports before deconvolutions
  cl.daily = smooth_cl(
    cl.daily   = cl.daily.raw,
    prm.smooth = prm.smooth
  )

  # trim smoothed reports based on relative error criterion

  if(!is.null(prm.daily.check)){
    if(is.null(prm.daily.check$agg.reldiff.tol)) stop("please specify agg.reldiff.tol in prm.daily.check")
    message("-----
- Aggregating inferred daily reports back using the original
reporting schedule, and calculating relative difference with
original reports...")

    cl.use.dates = get_use_dates(
      reports.daily   = cl.daily,
      reports         = cl.agg,
      agg.reldiff.tol = prm.daily.check$agg.reldiff.tol)

    message(paste0("- Filtering out any daily inferred reports associated
with inferred aggregates outside of the specified tolerance of ",
                   prm.daily.check$agg.reldiff.tol, "%..."
    ))
    message(paste0("Before filtering: ", nrow(cl.daily), " daily reports"))
    message(paste0("After filtering: ", length(cl.use.dates), " daily reports"))
    message("To reduce the number of observations dropped in filtering, either:
  - adjust MCMC parameters in prm.daily (burn, iter, chains) to
      improve chances of MCMC convergence,
  - increase tolerance for this check (prm.daily.check$agg.reldiff.tol)")
    cl.daily = (cl.daily
       %>% dplyr::filter(date %in% cl.use.dates)
    )
  }

  # Estimate Rt in an ensemble and return summary

  R = estimate_R_cl_rep(
    cl.daily      = cl.daily,
    dist.repfrac  = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub    = dist.incub,
    dist.gi       = dist.gi,
    prm.R         = prm.R
  )

  # Calculate the aggregated reports from the inferred daily reports

  inferred.agg = get_use_dates(
    reports.daily   = cl.daily,
    reports         = cl.agg,
    agg.reldiff.tol = Inf,
    dates.only      = FALSE ) %>%
    dplyr::filter(!is.na(obs)) %>%
    dplyr::select(date, obs, matches('agg$'))

  # Return results

  res = list(
    cl.agg  = cl.agg,
    cl.daily = cl.daily,
    inferred.agg = inferred.agg,
    R = R
  )

  return(res)
}
