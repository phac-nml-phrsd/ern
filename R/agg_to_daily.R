#' Infer daily counts from aggregates
#'
#' @inheritParams estimate_R_cl
#'
#' @return Dataframe with individual realizations of daily reported cases
#' @export
agg_to_daily <- function(
  cl.agg,
  dist.gi,
  popsize,
  prm.daily,
  silent = FALSE
) {

  gi = get_discrete_dist(dist.gi)

  df.daily.inc = fit_jags_aggreg(
    g = gi,
    N = popsize,
    obs.times = cl.agg$t,
    Y = cl.agg$count,
    mcmc.params = prm.daily,
    silent = silent) %>%
    reshape_fit_jags() %>%
    get_realizations(cl.agg)

  return(df.daily.inc)
}

# helpers -----------------------------------------------------------------

#' Attach time index (number of days) column
#' Exclude first day since we don't necessarily know over which period
#' of time that data was aggregated
#'
#' @inheritParams estimate_R_cl
#'
#' @return dataframe.
attach_t_agg <- function(cl.agg, prm.daily = NULL, silent = FALSE){

  first.agg.period <- prm.daily$first.agg.period

  # Handling the first aggregation
  if(is.null(first.agg.period)){
    fa = as.integer(cl.agg$date[2]-cl.agg$date[1])
    if(!silent){
      message(paste0("-----
Assuming the first observed report (from ", cl.agg$date[1], ")
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

  date.min = min(cl.agg$date)

  res = cl.agg %>%
    dplyr::mutate(t = as.numeric(date - date.min) + fa) %>%
    dplyr::arrange(t)

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
#' @inheritParams estimate_R_cl
fit_jags_aggreg <- function(
    obs.times,
    Y, g, N,
    n.days = NULL,
    mcmc.params = list(burn = 1e3,
                       iter = 3e3,
                       chains = 3),
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
    n.days = n.days,
    Y = Y,
    g = g,
    N = N,
    nobs = length(obs.times),
    ng = length(g),
    Iinit = Iinit
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

    R0 ~ dgamma(2,0.6)
    alpha ~ dgamma(1,1)
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
    n.chains = mcmc.params$chains,
    quiet = silent
  )


  # --- MCMC run

  # Burn-in period:
  output <- capture.output(stats::update(mod, n.iter = mcmc.params$burn))

  if(!silent) print(output)

  # Posterior iterations:
  output <- capture.output(mod_sim <- rjags::coda.samples(
    model = mod,
    variable.names = params,
    n.iter = mcmc.params$iter
  ))

  if(!silent) print(output)

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
