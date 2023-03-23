#' @title Estimate the effective reproduction from clinical report data
#'
#' @param cl.agg Data frame. Must have variables:
#' \itemize{
#'  \item `date`: calendar date of report
#'  \item `count`: count of reported cases
#' }
#' @param dist.repfrac List. Parameters for the reporting fraction distribution in the same format as returned by [`def_dist_reporting_fraction()`].
#' @param dist.repdelay List. Parameters for the reporting delay distribution in the same format as returned by [`def_dist_reporting_delay()`].
#' @param dist.incub List. Parameters for the incubation period distribution in the same format as returned by [`def_dist_incubation_period()`].
#' @template param-dist.gi
#' @param popsize Integer. Population size to use in MCMC simulation to infer daily observations from aggregated input data.
#' @param prm.daily List. Parameters for daily report inference via MCMC. Elements include:
#' \itemize{
#'  \item `burn`: length of burn-in period (number of days)
#'  \item `iter`: number of iterations after burn-in period (number of days)
#'  \item `chains`: number of chains to simulate
#'  \item `first.agg.period`: length of aggregation period for first aggregated observation (number of days); if NULL, assume same aggregation period as observed for second observation (gap between first and second observations)
#' }
#' @param prm.daily.check List. Parameters for checking aggregated to daily report inference. Elements include:
#' \itemize{
#'  \item `agg.reldiff.tol`: numerical tolerance (%) for relative error between aggregated inferred daily reports and original aggregated reports; chronological observations are dropped until this tolerance is first acheived (convergence at the start of the timeseries is often the worst, need to maintain uninterrupted daily timeseries for input into Rt calculation).
#' }
#' Set this entire argument to `NULL` to use inferred daily reports as is.
#' @param prm.smooth List. Smoothing parameters for input signal (daily reports) into Rt calculation. Elements include:
#' \itemize{
#'  \item `method`: smoothing method to use; currently only `rollmean` (centred rolling average) is implemented
#'  \item `window`: width of smoothing window (number of days)
#' }
#' Set this entire argument to `NULL` to turn off smoothing.
#' @template param-prm.R
#' @template param-silent
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return List. Elements include:
#' \itemize{
#'  \item `cl.agg`: original aggregated reports signal
#'  \item `cl.daily`: reports as input for Rt calculation (inferred daily counts, smoothed)
#'  \item `R`: the effective R estimate (summary from ensemble)
#' }
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
  ),
  silent = FALSE
) {

  # Checking arguments
  check_prm.R(prm.R, silent = silent)
  check_data_clin(cl.agg, silent = silent)

  # ==== Aggregated -> daily reports ====

  # attach time-index column to observed aggregated reports
  cl.agg <- attach_t_agg(
    cl.agg = cl.agg,
    prm.daily = prm.daily,
    silent = silent
  )

  # estimate daily reports using JAGS model
  cl.daily.raw = agg_to_daily(
    cl.agg    = cl.agg,
    dist.gi   = dist.gi,
    popsize   = popsize,
    prm.daily = prm.daily,
    silent    = silent
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
    if(!silent){
      message("-----
- Aggregating inferred daily reports back using the original
reporting schedule, and calculating relative difference with
original reports...")
    }

    cl.use.dates = get_use_dates(
      cl.daily = cl.daily,
      cl.agg = cl.agg,
      prm.daily.check = prm.daily.check
    )

    if(!silent){
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
    }
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
    prm.R         = prm.R,
    silent        = silent
  )

  # Calculate the aggregated reports from the inferred daily reports

  inferred.agg = (get_use_dates(
      cl.daily        = cl.daily,
      cl.agg          = cl.agg,
      prm.daily.check = list(agg.reldiff.tol = Inf),
      dates.only      = FALSE
  )
    %>% dplyr::filter(!is.na(obs))
    %>% dplyr::select(date, obs, dplyr::matches('agg$'))
  )

  # Return results

  res = list(
    cl.agg  = cl.agg,
    cl.daily = cl.daily,
    inferred.agg = inferred.agg,
    R = R
  )

  return(res)
}
