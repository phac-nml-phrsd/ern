#' Get dates for which to use (trust) inferred daily reports
#'
#' @inheritParams correct_underreporting
#' @inheritParams get_realizations
#'
#' @importFrom rlang .data
get_use_dates <- function(
    reports.daily, reports
){
  reports.daily <- summarise_by_date(reports.daily)

  # make unified df with observed data
  reports <- (reports
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
  (reports.daily
    %>% dplyr::full_join(reports, by = "date")
    %>% summarise_report_counts()
    %>% dplyr::filter(.data$use)
    %>% dplyr::pull(date)
  )
}


# helpers -----------------------------------------------------------------

#' Summarise observations by date
#'
#' @param df dataframe. has at least `date` and `value` columns.
#'
#' @importFrom rlang .data
summarise_by_date <- function(df){
  (df
   %>% dplyr::group_by(date)
   %>% dplyr::summarise(
     med = stats::median(.data$value),
     lwr = stats::quantile(.data$value, probs = 0.05),
     upr = stats::quantile(.data$value, probs = 0.95),
     .groups = "drop"
   )
  )
}

#' Summarise daily inferred reports
#' based on original reporting schedule and calculate error
#'
#' @param df dataframe. as prepared in [`get_use_dates()`].
#'
#' @importFrom rlang .data
summarise_report_counts <- function(df){
  df <- (df
   # aggregated fitted reports
   %>% dplyr::group_by(.data$date.report)
   %>% dplyr::mutate(
     dplyr::across(c(.data$med, .data$lwr,.data$ upr), sum,
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
    %>% dplyr::select(.data$date.report, .data$med.agg.reldiff)
    %>% tidyr::drop_na()
    %>% dplyr::mutate(
      # set "use" flag for fitted aggregated reports within a
      # 10% tolerance
      use = abs(.data$med.agg.reldiff) < 10
    )
    # figure out first date where fitted aggregated reports fall
    # within above threshold for relative error
    # drop values before that point in time
    %>% dplyr::mutate(use.cumm = cumsum(.data$use))
    # %>% select(date, med.agg.reldiff, use, use.cumm)
    %>% dplyr::filter(.data$use.cumm > 0)
    %>% dplyr::pull(.data$date.report)
  )

  # attach use flag to output data
  (df %>% dplyr::mutate(use = .data$date.report %in% use.dates))
}
