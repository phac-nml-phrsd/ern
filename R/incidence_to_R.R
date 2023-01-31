#' Estimate Rt using EpiEstim
#'
#' @param incidence dataframe. estimated incidence. includes at least `date`, `I`, and `t` columns.
#' @param generation.interval list. parameters for generation interval from [`def_dist_generation_interval()`].
#' @param config.EpiEstim configuration for `EpiEstim` defined via `EpiEstim::make_config()`. if `NULL`, use default config from `EpiEstim`.
#'
#' @importFrom rlang .data
#'
#' @seealso [def_dist_generation_interval()]
#' @seealso [EpiEstim::make_config()]
incidence_to_R <- function(
    incidence,
    generation.interval,
    config.EpiEstim = NULL
){
  # prep inputs
  # -------------------------
  incid <- incidence$I
  method <- "si_from_sample"
  if(is.null(config.EpiEstim)){
    config.EpiEstim <- suppressMessages(EpiEstim::make_config(
      incid = incid,
      method = method
    ))
  } else {
    if(!is.null(config.EpiEstim$method)) stop("EpiEstim method cannot be changed for ern")
    if(!is.null(config.EpiEstim$n2)) stop("parameters specific to EpiEstim method 'si_from_sample' cannot be changed for ern")
    config.EpiEstim <- suppressMessages(EpiEstim::make_config(
      incid = incid,
      method = method,
      config = config.EpiEstim
    ))
  }

  # calculate Rt based on _one_ generation interval
  # (handle GI sampling outside of this function)
  # -------------------------
  R <- EpiEstim::estimate_R(
    incid = incid,
    method = method,
    si_sample = matrix(c(0, get_discrete_dist(generation.interval))),
    config = config.EpiEstim
  )$R

  # prep output
  # -------------------------
  (R
    %>% dplyr::left_join(incidence, by = c("t_end" = "t"))
    %>% dplyr::select(-dplyr::starts_with("t_"))
    %>% dplyr::transmute(
      date,
      mean = .data$`Mean(R)`,
      I
    )
  )
}
