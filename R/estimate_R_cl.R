#' @title Estimate the effective reproduction from clinical report data
#'
#' @template param-cl.data
#' @param dist.repfrac List. Parameters for the reporting fraction distribution in the same format as returned by [`def_dist()`].
#' @param dist.repdelay List. Parameters for the reporting delay distribution in the same format as returned by [`def_dist()`].
#' @param dist.incub List. Parameters for the incubation period distribution in the same format as returned by [`def_dist()`].
#' @template param-dist.gi
#' @param prm.daily List. Parameters for daily report inference via MCMC. Elements include:
#' \itemize{
#'  \item `method`: String. Method name to infer the daily incidence reports from aggregated ones. 
#'  Either \code{linear} or \code{renewal} is currently implemented. 
#'  The \code{linear} method simply performs a linear interpolation that matches the aggregated values.
#'  The \code{renewal} method fits a SIR-like model using a renewal equation to infer the daily incidence. 
#'  In this case, the fitting algorithm is a Markov Chain Monte Carlo (MCMC) implemented in JAGS
#'  and needs the parameters below (e.g., \code{burn,iter,chains,...}). 
#'  The \code{renewal} method is more adapted for short single wave epidemics as this models
#'  i) naturally fits a single wave and ii) has longer computing time. 
#'  For longer time series, user may perfer the \code{linear} method.  
#'  \item `popsize`: Integer. Population size to use in MCMC simulation to infer daily observations from aggregated input data.
#'  \item `burn`: Numeric. Length of burn-in period (number of days).
#'  \item `iter`: Numeric. Number of iterations after burn-in period (number of days).
#'  \item `chains`: Numeric. Number of chains to simulate.
#'  \item `prior_R0_shape`: Shape of the (hyper-)parameter for the prior Gamma distribution for R0.
#'  \item `prior_R0_rate`: Rate of the (hyper-)parameter for the prior Gamma distribution for R0.
#'  \item `prior_alpha_shape`: Shape of the (hyper-)parameter for the prior Gamma distribution for alpha.
#'  \item `prior_alpha_rate`: Rate of the (hyper-)parameter for the prior Gamma distribution for alpha.
#'  \item `first.agg.period`: length of aggregation period for first aggregated observation (number of days); if NULL, assume same aggregation period as observed for second observation (gap between first and second observations)
#' }
#' @param prm.daily.check List. Parameters for checking aggregated to daily report inference. Elements include:
#' \itemize{
#'  \item `agg.reldiff.tol`: numerical tolerance (%) for relative error between aggregated inferred daily reports and original aggregated reports; chronological observations are dropped until this tolerance is first acheived (convergence at the start of the timeseries is often the worst, need to maintain uninterrupted daily timeseries for input into Rt calculation).
#' }
#' Set this entire argument to `NULL` to use inferred daily reports as is.
#' @template param-prm.smooth
#' @param RL.max.iter Integer. Maximum of iterations for the Richardson-Lucy deconvolution algorithm.
#' @template param-prm.R
#' @template param-silent
#'
#' @return List. Elements include:
#' \itemize{
#'  \item `cl.data`: original aggregated reports signal
#'  \item `cl.daily`: reports as input for Rt calculation (inferred daily counts, smoothed)
#'  \item `inferred.agg`: inferred daily reports aggregated on the reporting schedule as input in `cl.data`
#'  \item `R`: the effective R estimate (summary from ensemble)
#' }
#' @export
#' 
#' @seealso [plot_diagnostic_cl()] [estimate_R_ww()]
#' 
#' @examples 
#' 
#' # -- THIS EXAMPLE TAKES ABOUT 30 SECONDS TO RUN --
#' 
#' # Load SARS-CoV-2 reported cases in Ontario
#' # during the Omicron wave
#' data('cl.data')
#' dat = cl.data[cl.data$pt == 'on' & 
#'                  cl.data$date > as.Date('2021-11-30') & 
#'                  cl.data$date < as.Date('2022-01-31'),] 
#' 
#' # Estimate Rt
#' \dontrun{
#' x = estimate_R_cl(
#'   cl.data = dat,
#'   dist.repdelay = ern::def_dist(
#'     dist = 'gamma',
#'     mean = 6.99,
#'     mean_sd = 0.2211,
#'     sd = 3.663,
#'     sd_sd = 0.1158,
#'     max = 21
#'     ), 
#'   dist.repfrac = ern::def_dist(
#'     dist = "unif",
#'     min = 0.1,
#'     max = 0.3
#'     ),
#'   dist.incub = ern::def_dist(
#'     dist     = "gamma",
#'     mean     = 3.49,
#'     mean_sd  = 0.1477,
#'     shape    = 8.5,
#'     shape_sd = 1.8945,
#'     max      = 8
#'     ),
#'   dist.gi = ern::def_dist(
#'     dist     = "gamma",
#'     mean     = 6.84,
#'     mean_sd  = 0.7486,
#'     shape    = 2.39,
#'     shape_sd = 0.3573,
#'     max      = 15
#'     ),
#'   prm.daily = list(
#'     method = 'renewal',
#'     popsize = 14e6, # population of Ontario in 2023
#'     # Very low number of MCMC iterations
#'     # for this example to run fast.
#'     # Increase `burn`, `iter` and `chains` 
#'     # for better accuracy
#'     burn = 50, iter = 50, chains = 1, 
#'     # first.agg.period = NULL,
#'     prior_R0_shape = 2, prior_R0_rate = 0.6, 
#'     prior_alpha_shape = 1, prior_alpha_rate = 1),
#'   silent = TRUE
#' )
#' 
#' # Rt estimates
#' print(x$R)
#' }
#' 
#'  
#' 
estimate_R_cl <- function(
  cl.data,
  dist.repdelay,
  dist.repfrac,
  dist.incub,
  dist.gi,
  prm.daily = list(
    method = 'linear',  # c('linear', 'renewal')
    popsize = NULL,
    burn = 500,
    iter = 2e3,
    chains = 3,
    prior_R0_shape = 2,
    prior_R0_rate = 0.6,
    prior_alpha_shape = 1,
    prior_alpha_rate = 1,
    first.agg.period = NULL
  ),
  prm.daily.check = list(
    agg.reldiff.tol = 10
  ),
  prm.smooth = list(
    method = 'rollmean',
    align  = 'right',
    window = 7
  ),
  prm.R = list(
    iter = 10,
    CI = 0.95,
    window = 7,
    config.EpiEstim = NULL
  ),
  RL.max.iter = 10,
  silent = FALSE
) {

  # Checking if JAGS is installed on machine when daily inference via renewal method is requested. Stop function if not found
  if(!is.null(prm.daily)){
    if(prm.daily$method == "renewal"){
    suppressWarnings(j <- runjags::testjags(silent = TRUE))

    if(isFALSE(j$JAGS.found)){
      stop(
        "JAGS is not installed on this machine but is required for\n",
        "Rt estimations on clinical testing data using ern::estimate_Rt_cl()\n",
        "with the \"renewal\" method (argument `prm.daily(method=\'renewal\')`).\n",
        "Your options are:\n",
        "  - Either, install JAGS (https://sourceforge.net/projects/mcmc-jags/files/)\n",
        "  - Or, use the \"linear\" method: `prm.daily(method=\'renewal\')`\n\n",
        "See README for more details.\n\n")
    }}
  }
  
  # Checking argument formats
  check_prm.R(prm.R, silent = silent)
  check_cl.input_format(cl.data, silent = silent)

  # Check whether input data is daily
  # if not, need to do aggregated -> daily inference
  is.daily <- check_df.input_daily(cl.data)

  
  if(is.daily){
    # Data is already daily 
    cl.daily.raw <- (cl.data
      |> attach_t()
      |> dplyr::transmute(
       id = as.integer(1),
       date,
       t,
       value
    ))
    diagnostic.mcmc = NULL # Irrelevant, no MCMC needed in this case.
    
  } else {
    if(!silent){
      message("-----\nThe clinical testing data you input is not daily.",
              "\n`ern` requires daily data to compute Rt,",
              " so it will infer daily reports from your inputs.",
              "\nInference method for daily incidence: `", prm.daily$method,"`",
              "\nSee `prm.daily` and `prm.daily.check` arguments of ",
              "`estimate_R_cl()` for daily inference options.\n-----")
    }
    # ==== Aggregated -> daily reports ====

    # check daily inference params
    check_prm.daily(prm.daily)

    # attach time-index column to observed aggregated reports
    cl.data <- attach_t_agg(
      cl.data  = cl.data,
      prm.daily = prm.daily,
      silent    = silent
    )

    if(prm.daily$method == 'renewal'){
      # estimate daily reports using 
      # the epidemic model based on
      # the renewal equation and 
      # the JAGS software for inference.
      a = agg_to_daily(
        cl.data  = cl.data,
        dist.gi   = dist.gi,
        prm.daily = prm.daily,
        silent    = silent
      )
      
      cl.daily.raw = a[['df']]
      jags.obj     = a[['jags.object']]
      
      plot.gelmanrubin = NULL 
      
      if(prm.daily$chains > 1){
        gelrub = coda::gelman.diag(jags.obj)
        plot.gelmanrubin = plot_gelman_rubin(jags.obj)
        if(gelrub$mpsrf > 1.025) 
          warning("\n * * * MCMC warning * * * \n",
                  "The MCMC may not have properly converged according to ",
                  "the Gelman-Rubin R statistic (R_GelmanRubin = ",
                  round(gelrub$mpsrf,3),"> 1.025). \nConsider increasing the ",
                  "number of burn-in and iterations (`prm.daily` argument of `ern::estimate_R_cl()`).")
      }
      
      diagnostic.mcmc = list(
        plot.traces      = plot_traces(jags.obj),
        plot.gelmanrubin = plot.gelmanrubin,
        jags.obj         = jags.obj
      )
    }
    
    if(prm.daily$method == 'linear'){
     cl.daily.raw = linear_int_daily(cl.data)
     diagnostic.mcmc = NULL
    }
    
  } # end if not daily

     
  # ==== Smooth daily reports =====

  if(!is.null(prm.smooth)){
  # smooth daily reports before deconvolutions
  cl.daily = smooth_cl(
    cl.daily   = cl.daily.raw,
    prm.smooth = prm.smooth)
  } else {
  # match format
    cl.daily <- cl.daily.raw |> 
      dplyr::select(id, date, value, t)
  }
  
  # Trim smoothed reports based on 
  # relative error criterion
  if(!is.daily & 
     !is.null(prm.daily.check) ){
    
    if(prm.daily$method == 'renewal'){
      
      if(!silent){
        message("Aggregating inferred daily reports back ",
                "using the original reporting schedule, ",
                "and calculating relative difference ",
                "with original reports...")
      }
      
      cl.use.dates = get_use_dates(
        cl.daily = cl.daily,
        cl.data = cl.data,
        prm.daily.check = prm.daily.check )
      
      if(!silent){
        message("Filtering out any daily inferred reports associated ",
                "with inferred aggregates outside of the specified ",
                "tolerance of ", prm.daily.check$agg.reldiff.tol, "%...")
        
        dates.before = unique(cl.daily$date)
        message("  Before filtering : ", length(dates.before), " daily reports")
        message("  After filtering  :  ", length(cl.use.dates), " daily reports")
        message("To reduce the number of observations dropped in filtering,",
                "either:\n",
                "- adjust MCMC parameters in prm.daily (burn, iter, chains)",
                " to improve chances of MCMC convergence,\n",
                "- increase tolerance for this check (prm.daily.check$agg.reldiff.tol)")
      }
      cl.daily = cl.daily |> 
        dplyr::filter(date %in% cl.use.dates)
    } # end if method == 'renewal'
  }
  
  # Estimate Rt in an ensemble and return summary

  R = estimate_R_cl_rep(
    cl.daily      = cl.daily,
    dist.repfrac  = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub    = dist.incub,
    dist.gi       = dist.gi,
    prm.R         = prm.R,
    RL.max.iter   = RL.max.iter,
    silent        = silent
  )

  # Get inferred aggregates, if relevant

  if(is.daily){
    inferred.agg = NULL
  } else {
    # Calculate the aggregated reports from the inferred daily reports
    inferred.agg = (get_use_dates(
      cl.daily        = cl.daily,
      cl.data        = cl.data,
      prm.daily.check = list(agg.reldiff.tol = Inf),
      dates.only      = FALSE
    )
    |> dplyr::filter(!is.na(obs))
    |> dplyr::select(date, obs, dplyr::matches('agg$'))
    )
  }

  # Return results

  res = list(
    cl.data  = cl.data,
    cl.daily = cl.daily,
    inferred.agg = inferred.agg,
    R = R,
    diagnostic.mcmc = diagnostic.mcmc
  )

  return(res)
}
