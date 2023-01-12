#' A single realization of the Rt estimate
#'
#' @param cl.input dataframe. realizations of daily report counts. must include at least `date` and `value` columns.
#' @param dist.repfrac parameters for the reporting fraction distribution
#' @param dist.repdelay parameters for the reporting delay distribution
#' @param dist.incub parameters for the incubation period distribution
#' @param dist.gi parameters for the generation interval distribution
#'
#' @importFrom rlang .data
#'
#' @export
estimate_R_cl_single <- function(
    cl.input,
    dist.repfrac,
    dist.repdelay,
    dist.incub,
    dist.gi
){

  # sample one realization of reports.daily (smoothed)
  id.list <- unique(cl.input$id)
  the_id <- sample(id.list, size = 1)
  df.draw <- (cl.input
     %>% dplyr::filter(.data$id == the_id)
  )

  # sample reporting fraction
  reporting.fraction.draw <- sample_from_dist(
    n = 1,
    params = dist.repfrac
  )

  # sample reporting delay distribution
  reporting.delay <- sample_a_dist(dist.repdelay)

  # sample incubation period
  incubation.period <- sample_a_dist(dist.incub)

  # sample generation interval
  generation.interval <- sample_a_dist(dist.gi)

  # correct for underreporting
  reports.daily.scaled <- correct_underreporting(
    df.draw,
    reporting.fraction.draw
  )

  # reports deconvoluted with reporting delay distribution
  # and then with incubation period distribution
  incidence <- reports_to_incidence(
    reports.daily.scaled,
    reporting.delay,
    incubation.period,
  )

  # estimate Rt
  incidence_to_R(
    incidence,
    generation.interval
  )

}


# helpers -----------------------------------------------------------------

#' Correct underreporting by scaling up
#'
#' @param reports.daily dataframe of daily reported cases. must at least include `value` column with counts.
#' @param reporting.fraction numeric. proportion of incidence that is reported.
#'
#' @importFrom rlang .data
# to avoid "no visible binding" notes
correct_underreporting <- function(
    reports.daily,
    reporting.fraction
){
  (reports.daily
   %>% dplyr::mutate(value = .data$value/reporting.fraction)
  )
}

#' Infer incidence from reports via deconvolutions
#'
#' @param reports.daily dataframe. daily report counts. includes at least `date` and `value` columns.
#' @param reporting.delay list. parameters for reporting delay distribution (following same format as other `def_dist_*()` function.
#' @param incubation.period list. parameters for incubation period from [`def_dist_incubation_period()`].
#' @inheritParams deconv
#'
#' @importFrom rlang .data
#'
#' @seealso [`def_dist_incubation_period()`]
reports_to_incidence <- function(
    reports.daily,
    reporting.delay,
    incubation.period,
    max.iter = 50
){

  # reports -> onsets
  # -------------------------

  # deconv reports with reporting delay distribution
  onsets <- deconv(
    counts = reports.daily$value,
    dist = get_discrete_dist(reporting.delay),
    max.iter = max.iter
  )

  # onsets -> incidence
  # -------------------------

  # deconv onsets with incubation period distribution
  incidence <- deconv(
    counts = onsets$y,
    dist = get_discrete_dist(incubation.period),
    max.iter = max.iter
  )

  # output
  # -------------------------

  date.lookup <- tibble::tibble(
    date = reports.daily$date,
    t = 1:nrow(reports.daily)
  )

  (incidence
    # filter out first x days, where x is the sum of
    # the max reporting delay and the max incubation period
    # (i.e. disregard reports before a full observation period is complete)
    %>% dplyr::filter(t >= incubation.period$max + reporting.delay$max)
    %>% dplyr::left_join(date.lookup, by = "t")
    %>% dplyr::transmute(
      date,
      value = .data$y
    )
    %>% tibble::as_tibble()
  )
}

#' Wrapper for deconvolution with a given distribution
#'
#' @param counts numeric. a vector of daily counts
#' @param dist numeric. a vector of truncated daily discrete distribution (vector) with which we're deconvoluting the counts, _e.g._, produced by [`get_discrete_dist()`]
#' @param max.iter numeric. maximum number of Richardson-Lucy iterations
#'
#' @importFrom rlang .data
#'
#' @seealso [`get_discrete_dist()`]
deconv <- function(
    counts,
    dist,
    max.iter = 10
){
  # perform Richardson-Lucy deconvolution
  (deconvolution_RL(
    observed = counts,
    times = 1:length(counts),
    p_delay = dist,
    max_iter = max.iter
  )
  %>% tidyr::drop_na()
  %>% dplyr::rename(
    t = .data$time,
    y = .data$RL_result)
  )
}

#' Estimate Rt using EpiEstim
#'
#' @param incidence dataframe. estimated incidence. includes at least `date` and `value` columns.
#' @param generation.interval list. parameters for generation interval from [`def_dist_generation_interval()`].
#'
#' @importFrom rlang .data
#'
#' @seealso [`def_dist_generation_interval()`]
incidence_to_R <- function(
    incidence,
    generation.interval
){

  date.lookup <- (incidence
  %>% dplyr::transmute(
    date,
    t = 1:nrow(.))
  )

  # calculate Rt based on _one_ generation interval
  # (handle GI sampling outside of this function)
  # -------------------------

  (EpiEstim::estimate_R(
    incidence$value,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      # mean distribution parameters
      mean_si = generation.interval$mean,
      # sd distribution parameters
      std_si = generation.interval$sd
    ))
  )$R
  # just take mean estimate of R
  # TODO: where does the rest of the uncertainty come from??
  %>% janitor::clean_names()
  %>% dplyr::left_join(date.lookup, by = c("t_end" = "t"))
  %>% dplyr::select(-dplyr::starts_with("t_"))
  %>% dplyr::transmute(
    date,
    value = .data$mean_r
  )
  )
}
