#' A single realization of the Rt estimate
#'
#' @param cl.daily dataframe. realizations of daily report counts. must include at least `date` and `value` columns.
#' @param dist.repfrac parameters for the reporting fraction distribution
#' @param dist.repdelay parameters for the reporting delay distribution
#' @param dist.incub parameters for the incubation period distribution
#' @param dist.gi parameters for the generation interval distribution
#' @inheritParams incidence_to_R
#'
#' @importFrom rlang .data
#'
#' @export
estimate_R_cl_single <- function(
    cl.daily,
    dist.repfrac,
    dist.repdelay,
    dist.incub,
    dist.gi,
    prm.R
){

  # sample one realization of reports.daily (smoothed)
  id.list <- unique(cl.daily$id)
  the_id <- sample(id.list, size = 1)
  df.draw <- (cl.daily
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
  incidence <- (reports_to_incidence(
    reports.daily.scaled,
    reporting.delay,
    incubation.period,
  )
    # attach time index to incidence
    %>% dplyr::mutate(
      t = 1:nrow(.)
    )
  )

  # estimate Rt
  (incidence_to_R(
    incidence,
    generation.interval,
    prm.R
  )
    %>% dplyr::transmute(
      .data$date,
      value = .data$mean
    )
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
  message("-----
Deconvolving inferred daily reports with reporting delay
distribution to get daily onsets...")
  onsets <- deconv(
    counts = reports.daily$value,
    dist = get_discrete_dist(reporting.delay),
    max.iter = max.iter
  )

  # onsets -> incidence
  # -------------------------

  # deconv onsets with incubation period distribution
  message("-----
Deconvolving daily onsets with incubation period
distribution to get daily incidence...")
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
    # %>% dplyr::filter(t >= incubation.period$max + reporting.delay$max)
    %>% dplyr::left_join(date.lookup, by = "t")
    %>% dplyr::transmute(
      date,
      I = .data$y
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
  # check args
  # -------------------------
  check_for_deconv(counts, dist)

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
