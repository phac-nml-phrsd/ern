#' Infer daily counts from weekly aggregates
#'
#' @param cl.weekly Dataframe. Must have variables \code{date} for the calendar
#' date of the observation, \code{count} for the count of reported cases,
#' and \code{t} for the number of days from the first count#' @param dist.gi parameters for the generation interval distribution
#' @param popsize population size
#' @param prm.daily parameters for daily report inference (via MCMC)
#' @inheritParams estimate_R_cl_single
#'
#' @return Dataframe with individual realizations of daily reported cases
#' @export
weekly_to_daily <- function(
  cl.weekly,
  dist.gi,
  popsize,
  prm.daily
) {

  gi = get_discrete_dist(sample_a_dist(dist.gi))

  (fit_jags_aggreg(
    g = gi, N = popsize,
    obs.times = cl.weekly$t,
    Y = cl.weekly$count,
    mcmc.params = prm.daily,
    path.jags = 'R/'
  )
    %>% reshape_fit_jags()
    %>% get_realizations(cl.weekly)
  )
}

# helpers -----------------------------------------------------------------

#' Fit JAGS model to aggregated data
fit_jags_aggreg <- function(
    obs.times,
    Y, g, N,
    n.days = NULL,
    mcmc.params = list(burn = 1e3, iter = 3e3, chains = 3),
    path.jags = 'R/'
){

  if(is.null(n.days)) n.days = max(obs.times)

  # --- JAGS setup

  data_jags = list(
    obs.times = obs.times,
    n.days = n.days,
    Y = Y,
    g = g,
    N = N,
    nobs = length(obs.times),
    ng = length(g)
  )

  params = c("R0", "alpha", "I")

  inits <- function() {
    return(list(
      "R0"    = 1.0,
      "alpha" = 0.0
    ))
  }

  # --- JAGS model definition

  fname = paste0(path.jags,'reem-fit-noww.jags')

  mod <- rjags::jags.model(
    file = fname,
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
#' @importFrom rlang .data
reshape_fit_jags <- function(x){
  (lapply(x, tibble::as_tibble)
   %>% dplyr::bind_rows()
   %>% dplyr::mutate(iteration = 1:nrow(.data))
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

#' Retrieve realizations for weekly -> daily inference
#'
#' @seealso [weekly_to_daily()]
#'
#' @importFrom rlang .data
get_realizations <- function(
    fit.reports.daily, reports
){

  date_lookup <- (reports
    %>% attach_startdate_agg()
    %>% dplyr::select(date)
    %>% tidyr::complete(date = seq(min(date), max(date), by = "days"))
    %>% dplyr::mutate(t = 1:nrow(.data))
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
#' counterpart function to `attach_t_agg()`
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
