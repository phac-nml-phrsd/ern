#' @title Infer daily counts from aggregates
#'
#' @inheritParams estimate_R_cl
#'
#' @importFrom utils capture.output
#'
#' @return A list containing a data frame with individual realizations of 
#' daily reported cases and the JAGS object.
#' @export
#' 
#' @examples 
#' 
#' # Importing data attached to the `ern` package
#' # and selecting the Omicron wave in Ontario, Canada.
#' # This is *weekly* incidence.
#' data(cl.data)
#' data = cl.data[cl.data$pt == 'on' & 
#'                   cl.data$date > as.Date('2021-11-30') & 
#'                   cl.data$date < as.Date('2021-12-31'),] 
#' head(data)
#' dist.gi = ern::def_dist(
#'  dist     = "gamma",
#'  mean     = 6.84,
#'  mean_sd  = 0.7486,
#'  shape    = 2.39,
#'  shape_sd = 0.3573,
#'  max      = 15
#' )
#' 
#' a = agg_to_daily(
#' cl.data = data, 
#' dist.gi = dist.gi, 
#'   prm.daily = list(
#'   method = "renewal",
#'   popsize = 14e6,
#'   # MCMC parameters.
#'   # small values for computation speed for this example.
#'   # Increase for better accuracy
#'   burn = 100,
#'   iter = 100,
#'   chains = 2,
#'   # - - - - - 
#'   prior_R0_shape = 2,
#'   prior_R0_rate = 0.6,
#'   prior_alpha_shape = 1,
#'   prior_alpha_rate = 1
#' ))

#' # This is a Bayesian inference, so we 
#' # have a posterior distribution of  
#' # daily incidences. Here we just plot
#' # one single draw:
#'       
#'  df = a$df
#'  df1 = df[df$id==1,]
#'  plot(x = df1$t, y = df1$value, typ = 'o',
#'       xlab = 'days', ylab = 'daily incidence',
#'       main = 'Posterior daily incidence infered from weekly incidence')
#'  
#'  # Extract of the parameters values from the first chain
#'  a$jags.object[[1]][1:9,1:9]
#'  


agg_to_daily <- function(
  cl.data,
  dist.gi,
  prm.daily,
  silent = FALSE
) {

  gi = get_discrete_dist(dist.gi)
  
  # in case the data supplied does not have a time variable
  if( ! 't' %in% names(cl.data) ){
    dt1 = as.integer(cl.data$date[2] - cl.data$date[1])
    cl.data$t <-  as.integer(cl.data$date - cl.data$date[1]) + dt1
    warning('`agg_to_daily()`: time variable not supplied in data, deducting it from the dates.')
  }

  jags.fit = fit_jags_aggreg(
    g = gi,
    N = prm.daily$popsize,
    obs.times = cl.data$t,
    Y = cl.data$value,
    prm.daily = prm.daily,
    silent = silent) 
  
  df.daily.inc = jags.fit |>
    reshape_fit_jags() |>
    get_realizations(cl.data)

  return(list(
    jags.object = jags.fit, 
    df = df.daily.inc))
}

# helpers -----------------------------------------------------------------

#' @title Attach time index (number of days) column. Exclude first day since we don't necessarily know over which period of time that data was aggregated
#'
#' @inheritParams estimate_R_cl
#' @keywords internal
#' @return Data frame
attach_t_agg <- function(cl.data, prm.daily = NULL, silent = FALSE){

  first.agg.period <- prm.daily$first.agg.period

  # Handling the first aggregation
  if(is.null(first.agg.period)){
    fa = as.integer(cl.data$date[2]-cl.data$date[1])
    if(!silent){
      message(paste0("-----
Assuming the first observed report (from ", cl.data$date[1], ")
is aggregated over ", fa , " previous days
(second observation's aggregation period).
This can be changed in `estimate_R_cl()`, using the
`prm.daily` argument (set a value for `first.agg.period`
in this parameter list)."))
    }
  }
  if(!is.null(first.agg.period)){
    fa = first.agg.period
    if(!silent){
      message(paste('
    Aggregation period for first observed report
    is set to', fa, 'days.'))
    }
  }

  date.min = min(cl.data$date)

  res = cl.data |>
    dplyr::mutate(t = as.numeric(date - date.min) + fa) |>
    dplyr::arrange(t)

  return(res)
}

#' @title Fit JAGS model to aggregated data
#'
#' @param obs.times Numeric. Vector of observation times.
#' @param Y Numeric. Vector of aggregated counts.
#' @param g Numeric. Vector of discretized generation interval density.
#' @param N Numeric. Scalar population size.
#' @param n.days Numeric. Total number of days. if `NULL`, use `max(obs.times)`
#' @inheritParams estimate_R_cl
#' @keywords internal
fit_jags_aggreg <- function(
    obs.times,
    Y, g, N,
    n.days = NULL,
    prm.daily,
    silent = FALSE
){

  if(is.null(n.days)) n.days = max(obs.times)

  # --- JAGS setup

  # initial guess for the *daily* incidence
  # (assumes same aggregation period for the first
  # data point (Y[1]) as the second one. `+1` prevents Iinit=0)
  Iinit = Y[1] / (obs.times[2] - obs.times[1]) + 1

  if(Iinit <=0) stop('Initial incidence cannot be negative. ABORTING!')
  if(Iinit > N) stop('Initial incidence cannot be larger than population size. ABORTING!')

  data_jags = list(
    obs.times = obs.times,
    n.days    = n.days,
    Y         = Y,
    g         = g,
    N         = N,
    nobs      = length(obs.times),
    ng        = length(g),
    Iinit     = Iinit,
    prior_R0_shape    = prm.daily[['prior_R0_shape']],
    prior_R0_rate     = prm.daily[['prior_R0_rate']],
    prior_alpha_shape = prm.daily[['prior_alpha_shape']],
    prior_alpha_rate  = prm.daily[['prior_alpha_rate']]
  )
  
  params = c("R0", "alpha", "I")

  inits <- function() {
    return(list(
      "R0"    = 1.0,
      "alpha" = 0.0
    ))
  }

  # --- JAGS model definition

  model.text <- "model{
    # === Initial period ===

    I[1] ~ dpois(Iinit)
    S[1] <- N - I[1] #ifelse(I[1] < N, N - I[1], 1)

    for(t in 2:ng){
      tmp[t] <- sum(g[1:(t-1)] * I[t-1:(t-1)])
      Im[t]  <- R0 * tmp[t] * (S[t-1]/N)^(1+alpha) + 0.9   # <-- adding `0.9` avoids JAGS error `invalid parent value`... not fully clear why.
      I[t]   ~ dpois(Im[t])
      S[t]   <- ifelse(N > sum(I[1:t]), N - sum(I[1:t]), 0)
    }

    # === Main time loop ===

     # -- Renewal equation

    for(t in (ng+1):n.days){
      tmp[t] <- sum(g[1:ng] * I[t-(1:ng)])
      S[t]   <- ifelse(N > sum(I[1:t]), N - sum(I[1:t]), 0)
      Im[t]  <- R0 * tmp[t] * (S[t-1]/N)^(1+alpha) + 0.9
      I[t]   ~ dpois(Im[t])
    }

    # -- Aggregation

    A[obs.times[1]] <- sum(I[1:obs.times[1]])

    for(i in 2:nobs){
      A[obs.times[i]] <- sum( I[(obs.times[i-1]+1):obs.times[i]] )
    }

    # === observation times only ===

    for(i in 1:nobs){
      Y[i] ~ dpois(A[obs.times[i]])
    }

    # === priors ===

    R0    ~ dgamma(prior_R0_shape, prior_R0_rate)
    alpha ~ dgamma(prior_alpha_shape, prior_alpha_rate)
  }"

  if(!silent){
    message("-----
Running MCMC model to infer daily reports from aggregated reports...
")
  }

  mod <- rjags::jags.model(
    file = textConnection(model.text),
    data = data_jags,
    inits = inits,
    n.chains = prm.daily$chains,
    quiet = silent
  )


  # --- MCMC run
  
  if(!silent){
    message('MCMC paramters:',
            '\n  Number of chains   : ', prm.daily$chains,
            '\n  Burn-in iterations : ', prm.daily$burn,
            '\n  MCMC iterations    : ', prm.daily$iter        )
  }
  # Burn-in period:
  n.iter = prm.daily$burn
  if(!silent) stats::update(mod, n.iter = n.iter)
  if(silent) capture.output(stats::update(mod, n.iter = n.iter))

  # Posterior iterations:
  n.iter = prm.daily$iter
  if(silent) capture.output(mod_sim <- rjags::coda.samples(
    model = mod,
    variable.names = params,
    n.iter = n.iter))

  if(!silent) mod_sim <- rjags::coda.samples(model = mod,
                                             variable.names = params,
                                             n.iter = n.iter)
  
  return(mod_sim)
}

#' @title Reshape JAGS fit object
#'
#' @param x Data frame. JAGS output from [`fit_jags_aggreg()`].
#' @keywords internal
#'
reshape_fit_jags <- function(x){
  (lapply(x, tibble::as_tibble)
   |> dplyr::bind_rows()
   |> (\(x){dplyr::mutate(x, iteration = 1:nrow(x))})()
   |> tidyr::pivot_longer(-iteration)
   |> tidyr::separate(
     name,
     into = c("var", "t", "trash"),
     sep = "\\[|\\]",
     fill = "right"
   )
   |> dplyr::select(-trash)
   |> dplyr::mutate(t = as.integer(t))
  )
}

#' @title Retrieve realizations for aggregated to daily inference
#'
#' @param fit.reports.daily Data frame. Realizations from daily report inference. Must at least have `t` (time index), `var` (variable name), `iteration` (realization number), and `value` (inferred count) columns.
#' @param reports Data frame. Original aggregated reports. Must at least have `date` column.
#'
#' @seealso [agg_to_daily()]
#' @keywords internal
#'
#' @return Data frame
get_realizations <- function(
    fit.reports.daily, reports
){

  date_lookup <- (reports
    |> attach_startdate_agg()
    |> dplyr::select(date)
    |> tidyr::complete(date = seq(min(date), max(date), by = "days"))
    |> (\(x){dplyr::mutate(x, t = 1:nrow(x))})()
  )

  # extract fitted daily reports
  # and mark each iteration (across iter #, batch #, rep #)
  # with unique id variable
  (fit.reports.daily
    |> dplyr::filter(var == "I")
    |> dplyr::left_join(date_lookup, by = "t")
    |> dplyr::group_by(iteration)
    |> dplyr::mutate(
      id = dplyr::cur_group_id(),
    )
    |> dplyr::ungroup()
    |> dplyr::select(
      id, date, t, value
    )
  )
}

#' @title Attach start date from first observation for aggregated data from time (day number)
#' @keywords internal
#' @param x dataframe. only has columns `date`, `value`, and `t`
attach_startdate_agg <- function(x){
  start_date <- (x
   |> dplyr::arrange(date)
   |> dplyr::slice(1)
   |> dplyr::mutate(start_date = date - lubridate::days(t-1))
   |> dplyr::pull(start_date)
  )

  dplyr::bind_rows(
    tibble::tibble(
      date = start_date,
      value = NA,
      t = 1
    ),
    (x |> dplyr::arrange(date))
  )
}
