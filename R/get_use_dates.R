#' Get dates for which to use (trust) inferred daily reports
#'
#' @inheritParams smooth_cl
#' @inheritParams estimate_R_cl
#' @param dates.only Logical. Return use dates only or all columns of `cl.daily`.
#'
#' @importFrom rlang .data
get_use_dates <- function(
    cl.daily,
    cl.agg,
    prm.daily.check,
    dates.only = TRUE
){

  cl.daily <- cl.daily %>%
    summarise_by_date_iters()

  # make unified df with observed data
  cl.agg <- (cl.agg
    # make date of report column to track aggregation periods
    %>% dplyr::mutate(date.report = date)
    # attach start date based on time index column
    %>% attach_startdate_agg()
    %>% dplyr::select(-t)
    # complete date sequence
    %>% tidyr::complete(
      date = seq(min(date), max(date), by = "day")
    )
    # fill report date up to make grouping variable to aggregate
    # inferred daily reports
    %>% tidyr::fill(.data$date.report, .direction = "up")
    # rename count as obs to make col clearer in unified data below
    %>% dplyr::rename(obs = .data$count)
  )

  # unified data with aggregates and relative differences
  res = (cl.daily
    %>% dplyr::full_join(cl.agg, by = "date")
    %>% summarise_report_counts(prm.daily.check = prm.daily.check)
  )

  if(dates.only) {
    res = res  %>%
      dplyr::filter(.data$use) %>%
      dplyr::pull(date)
  }

  return(res)
}


# helpers -----------------------------------------------------------------

#' Summarise observations by date for iterated data
#'
#' @description This function summarises raw iterations of data.
#'
#' @param df Data frame. Must have `date` and `value` columns.
#'
#' @importFrom rlang .data
summarise_by_date_iters <- function(df){
  res = df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      mean = mean(.data$value),
      lwr  = stats::quantile(.data$value, probs = 0.025),
      upr  = stats::quantile(.data$value, probs = 0.975),
      .groups = "drop" )

  return(res)
}

#' Summarise observations by date for iterated ensemble data
#'
#' @description This function summarises iterations of ensemble data.
#'
#' @param df Data frame. Must have `date`, `mean`, `lo`, and `hi` columns.
#' @param CI Numeric. Confidence interval width for the summary, as a proportion (`CI = 0.95` uses the 95% confidence interval)
#'
#' @importFrom rlang .data
summarise_by_date_ens <- function(df, CI = 0.95){
  res = df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      mean = mean(.data$mean),
      #
      # what we do below for 'lwr' and 'upr' is not statistically correct,
      # but likely "good enough" for now.
      #
      lwr = stats::quantile(.data$lo, probs = 0.5 - CI/2),
      upr = stats::quantile(.data$hi, probs = 0.5 + CI/2),
      .groups = "drop" )

  return(res)
}

#' Summarise daily inferred reports
#' based on original reporting schedule and calculate error
#'
#' @param df Data frame. As output by [`get_use_dates()`].
#' @inheritParams estimate_R_cl
#'
#' @importFrom rlang .data
summarise_report_counts <- function(df, prm.daily.check){
  agg.reldiff.tol <- prm.daily.check$agg.reldiff.tol

  df <- (df
   # aggregated fitted reports
   %>% dplyr::group_by(.data$date.report)
   %>% dplyr::mutate(
     dplyr::across(c(.data$mean, .data$lwr,.data$ upr), sum,
                   .names = "{.col}.agg"))
   %>% dplyr::ungroup()
   %>% dplyr::mutate(
     # relative diffs with observed
     dplyr::across(dplyr::ends_with("agg"), ~ (. - obs)/obs*100,
            .names = "{.col}.reldiff")
   )
  )

  # figure out which daily inferred reports should actually be used
  # we want to filter out start of inferred daily reports until
  # estimates have converged to below a specified tolerance
  use.dates <- (df
    %>% dplyr::select(.data$date.report, .data$mean.agg.reldiff)
    %>% tidyr::drop_na()
    %>% dplyr::mutate(
      # set "use" flag for fitted aggregated reports within a
      # 10% tolerance
      use = abs(.data$mean.agg.reldiff) < agg.reldiff.tol
    )
    # figure out first date where fitted aggregated reports fall
    # within above threshold for relative error
    # drop values before that point in time
    %>% dplyr::mutate(use.cumm = cumsum(.data$use))
    %>% dplyr::filter(.data$use.cumm > 0)
    %>% dplyr::pull(.data$date.report)
  )

  # TODO: show use.dates here

  # attach use flag to output data
  (df %>% dplyr::mutate(use = .data$date.report %in% use.dates))
}
