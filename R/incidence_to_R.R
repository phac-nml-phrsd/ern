#' Estimate Rt using EpiEstim
#'
#' @param incidence dataframe. estimated incidence. includes at least `date`, `I`, and `t` columns.
#' @param generation.interval list. parameters for generation interval from [`def_dist_generation_interval()`].
#'
#' @importFrom rlang .data
#'
#' @seealso [`def_dist_generation_interval()`]
incidence_to_R <- function(
    incidence,
    generation.interval
){

  # calculate Rt based on _one_ generation interval
  # (handle GI sampling outside of this function)
  # -------------------------
  (EpiEstim::estimate_R(
    incidence$I,
    method = "si_from_sample",
    si_sample = matrix(c(0, get_discrete_dist(generation.interval)))
    # method = "parametric_si",
    # config = EpiEstim::make_config(list(
    #   # mean distribution parameters
    #   mean_si = generation.interval$mean,
    #   # sd distribution parameters
    #   std_si = generation.interval$sd
    # ))
  )$R
  %>% dplyr::left_join(incidence, by = c("t_end" = "t"))
  %>% dplyr::select(-dplyr::starts_with("t_"))
  %>% dplyr::transmute(
    date,
    mean = .data$`Mean(R)`,
    I
  )
  )
}
