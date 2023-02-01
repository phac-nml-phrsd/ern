#' Infer daily counts from aggregates
#'
#' @param cl.agg dataframe. must have variables \code{date} for the calendar
#' date of the observation, \code{count} for the count of reported cases.
#' @param popsize population size.
#' @param prm.daily parameters for daily report inference (via MCMC).
#' @inheritParams estimate_R_cl_single
#'
#' @return Dataframe with individual realizations of daily reported cases
#' @export
agg_to_daily <- function(
  cl.agg,
  dist.gi,
  popsize,
  prm.daily
) {

  gi = get_discrete_dist(sample_a_dist(dist.gi))

  (fit_jags_aggreg(
    g = gi, N = popsize,
    obs.times = cl.agg$t,
    Y = cl.agg$count,
    mcmc.params = prm.daily
  )
    %>% reshape_fit_jags()
    %>% get_realizations(cl.agg)
  )
}

# helpers -----------------------------------------------------------------

#' Attach time index (number of days) column
#' Exclude first day since we don't necessarily know over which period
#' of time that data was aggregated
#'
#' @param x dataframe. must at least have a `date` column
#' @param first.agg.period numeric. number of days over which reports were summed for the first observed report. if NULL, assumes same aggregation period as for second observation.
#'
#' @return dataframe.
attach_t_agg <- function(x, first.agg.period = NULL){

  # Handling the first aggregation
  if(is.null(first.agg.period)){
    fa = as.integer(x$date[2]-x$date[1])
    message(paste0("-----
Assuming the first observed report (from ", x$date[1], ")
is aggregated over ", fa , " previous days
(second observation's aggregation period).
This can be changed in `estimate_R_cl()`, using the
`prm.daily` argument (set a value for `first.agg.period`
in this parameter list)."))
  }
  if(!is.null(first.agg.period)){
    fa = first.agg.period
    message(paste('
    Aggregation period for first observed report
    is set to', fa, 'days.'))
  }

  res = (x
   %>% dplyr::mutate(t = as.numeric(date - min(date)) + fa)
   %>% dplyr::arrange(t)
  )

  return(res)
}

#' Fit JAGS model to aggregated data
#'
#' @param obs.times numeric. vector of observation times
#' @param Y numeric. vector of aggregated counts
#' @param g numeric. vector of discretized generation interval density.
#' @param N numeric. scalar population size.
#' @param n.days numeric. total number of days. if `NULL`, use `max(obs.times)`
#' @param mcmc.params list. MCMC parameters:
#' * `burn`: burn-in period (days)
#' * `iter`: iterations after burn-in (days)
#' * `chains`: number of chains
fit_jags_aggreg <- function(
    obs.times,
    Y, g, N,
    n.days = NULL,
    mcmc.params = list(burn = 1e3,
                       iter = 3e3,
                       chains = 3)
){

  if(is.null(n.days)) n.days = max(obs.times)

  # --- JAGS setup

  # initial guess for the *daily* incidence
  # (assumes same aggregation period for the first
  # data point (Y[1]) as the second one. `+1` prevents Iinit=0)
  Iinit = Y[1] / (obs.times[2] - obs.times[1]) + 1

  data_jags = list(
    obs.times = obs.times,
    n.days = n.days,
    Y = Y,
    g = g,
    N = N,
    nobs = length(obs.times),
    ng = length(g),
    Iinit = Iinit
  )

  message(print(paste('Iinit =', data_jags$Iinit)))

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
      Im[t]  <- R0 * tmp[t] * (S[t-1]/N)^(1+alpha)
      I[t]   ~ dpois(Im[t])
      S[t]   <- ifelse(N > sum(I[1:t]), N - sum(I[1:t]), 0)
    }

    # === Main time loop ===

     # -- Renewal equation

    for(t in (ng+1):n.days){
      tmp[t] <- sum(g[1:ng] * I[t-(1:ng)])
      S[t]   <- ifelse(N > sum(I[1:t]), N - sum(I[1:t]), 0)
      Im[t]  <- R0 * tmp[t] * (S[t-1]/N)^(1+alpha)
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

    R0 ~ dgamma(2,1)
    alpha ~ dgamma(1,1)
  }"

  mod <- rjags::jags.model(
    file = textConnection(model.text),
    data = data_jags,
    inits = inits,
    n.chains = mcmc.params$chains
  )

  # --- MCMC run

  # Burn-in period:
  stats::update(mod, n.iter = mcmc.params$burn)

  # Posterior iterations:
  mod_sim <- rjags::coda.samples(model = mod,
                          variable.names = params,
                          n.iter = mcmc.params$iter)

  return(mod_sim)
}

#' Reshape JAGS fit object
#'
#' @param x dataframe. JAGS output from [`fit_jags_aggreg()`].
#'
#' @importFrom rlang .data
#'
#' @seealso [`fit_jags_aggreg()`]
reshape_fit_jags <- function(x){
  (lapply(x, tibble::as_tibble)
   %>% dplyr::bind_rows()
   %>% dplyr::mutate(iteration = 1:nrow(.))
   %>% tidyr::pivot_longer(-.data$iteration)
   %>% tidyr::separate(
     .data$name,
     into = c("var", "t", "trash"),
     sep = "\\[|\\]",
     fill = "right"
   )
   %>% dplyr::select(-.data$trash)
   %>% dplyr::mutate(t = as.integer(t))
  )
}

#' Retrieve realizations for aggregated -> daily inference
#'
#' @param fit.reports.daily dataframe. realizations from daily report inference. must at least have `t` (time index), `var` (variable name), `iteration` (realization number), and `value` (inferred count) columns.
#' @param reports dataframe. original aggregated reports. must at least have `date` column
#'
#' @seealso [agg_to_daily()]
#'
#' @importFrom rlang .data
get_realizations <- function(
    fit.reports.daily, reports
){

  date_lookup <- (reports
    %>% attach_startdate_agg()
    %>% dplyr::select(date)
    %>% tidyr::complete(date = seq(min(date), max(date), by = "days"))
    %>% dplyr::mutate(t = 1:nrow(.))
  )

  # extract fitted daily reports
  # and mark each iteration (across iter #, batch #, rep #)
  # with unique id variable
  (fit.reports.daily
    %>% dplyr::filter(.data$var == "I")
    %>% dplyr::left_join(date_lookup, by = "t")
    %>% dplyr::group_by(.data$iteration)
    %>% dplyr::mutate(
      id = dplyr::cur_group_id(),
    )
    %>% dplyr::ungroup()
    %>% dplyr::select(
      .data$id, .data$date, .data$t, .data$value
    )
  )
}

#' Attach start date from first observation for aggregated data
#' from time (day number)
#'
#' @param x dataframe. only has columns `date`, `count`, and `t`
attach_startdate_agg <- function(x){
  start_date <- (x
   %>% dplyr::arrange(date)
   %>% dplyr::slice(1)
   %>% dplyr::mutate(start_date = date - lubridate::days(t-1))
   %>% dplyr::pull(start_date)
  )

  dplyr::bind_rows(
    tibble::tibble(
      date = start_date,
      count = NA,
      t = 1
    ),
    (x %>% dplyr::arrange(date))
  )
}
