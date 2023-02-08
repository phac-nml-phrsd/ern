#' Estimate Rt using EpiEstim
#'
#' @param incidence dataframe. estimated incidence. includes at least `date`, `I`, and `t` columns.
#' @param generation.interval list. parameters for generation interval from [`def_dist_generation_interval()`].
#' @param prm.R list. settings for the ensemble when calculating Rt. elements include:
#' \itemize{
#'  \item{`iter`: }{number of iterations for ensemble}
#'  \item{`window`: }{length of time window to use for each Rt estimate. if `t_end` is specified in `config.EpiEstim`, this option will override it.}
#'  \item{`config.EpiEstim`: }{configuration for `EpiEstim` defined via `EpiEstim::make_config()`. if `NULL`, will use default config from `EpiEstim`.}
#' }
#'
#' @importFrom rlang .data
#'
#' @seealso [def_dist_generation_interval()]
#' @seealso [EpiEstim::make_config()]
incidence_to_R <- function(
    incidence,
    generation.interval,
    prm.R
){
  # === prep inputs ====

  # make config
  incid  <- incidence$I
  method <- "non_parametric_si"
  if(is.null(prm.R$config.EpiEstim)){
    config.EpiEstim <- suppressMessages(EpiEstim::make_config(
      incid    = incid,
      method   = method,
      si_distr = c(0, get_discrete_dist(generation.interval))
    ))
  } else {
    prm.R$config.EpiEstim$si_distr = c(0, get_discrete_dist(generation.interval))
    config.EpiEstim <- suppressMessages(EpiEstim::make_config(
      incid = incid,
      method = method,
      config = prm.R$config.EpiEstim
    ))
  }

  # update t_end in config if window is specified in prm.R
  # NOTE: this overrides specification of t_end in config.EpiEstim
  if(!is.null(prm.R$window)){
    t_start <- config.EpiEstim$t_start
    t_end <- as.integer(t_start + (prm.R$window - 1)) # -1 because window includes endpoints
    # trim off start/end dates where end date exceeds
    # number of incidence observations
    valid <- t_end <= length(incid)
    config.EpiEstim$t_start <- t_start[valid]
    config.EpiEstim$t_end <- t_end[valid]
  }

  # ==== Calculate Rt ====
  # calculate Rt based on _one_ generation interval
  # (handle GI sampling outside of this function)

  R <- EpiEstim::estimate_R(
    incid = incid,
    method = method,
    config = config.EpiEstim
  )$R

  # ==== prep output ====

  res = (R
    %>% dplyr::left_join(incidence, by = c("t_end" = "t"))
    %>% dplyr::select(-dplyr::starts_with("t_"))
    %>% dplyr::transmute(
      date,
      mean = .data$`Mean(R)`,
      lo   = .data$`Quantile.0.025(R)`,
      hi   = .data$`Quantile.0.975(R)`,
      I )
  )
  return(res)
}

# - - - - - - - - -


