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
    %>% tidyr::fill(date.report, .direction = "up")
    # rename count as obs to make col clearer in unified data below
    %>% dplyr::rename(obs = count)
  )

  # unified data with aggregates and relative differences
  (reports.daily
    %>% dplyr::full_join(reports, by = "date")
    %>% summarise_report_counts()
    %>% dplyr::filter(use)
    %>% dplyr::pull(date)
  )
}


# helpers -----------------------------------------------------------------

summarise_by_date <- function(df){
  (df
   %>% dplyr::group_by(date)
   %>% dplyr::summarise(
     med = stats::median(value),
     lwr = stats::quantile(value, probs = 0.05),
     upr = stats::quantile(value, probs = 0.95),
     .groups = "drop"
   )
  )
}

summarise_report_counts <- function(df){
  df <- (df
   # aggregated fitted reports
   %>% dplyr::group_by(date.report)
   %>% dplyr::mutate(
     dplyr::across(c(med, lwr, upr), sum,
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
    %>% dplyr::select(date.report, med.agg.reldiff)
    %>% tidyr::drop_na()
    %>% dplyr::mutate(
      # set "use" flag for fitted aggregated reports within a
      # 10% tolerance
      use = abs(med.agg.reldiff) < 10
    )
    # figure out first date where fitted aggregated reports fall
    # within above threshold for relative error
    # drop values before that point in time
    %>% dplyr::mutate(use.cumm = cumsum(use))
    # %>% select(date, med.agg.reldiff, use, use.cumm)
    %>% dplyr::filter(use.cumm > 0)
    %>% dplyr::pull(date.report)
  )

  # attach use flag to output data
  (df %>% dplyr::mutate(use = date.report %in% use.dates))
}
