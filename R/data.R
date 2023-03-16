#' Sample of aggregated clinical reports
#'
#' A subset of COVID-19 weekly reports in the Government of Canada Health Infobase
#'
#' @format ## `cl.agg`
#' A data frame with 84 rows and 4 columns:
#' \itemize{
#'   \item `pt`: standard two-character abbreviation (lowercase) of the province name (based on \href{https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=T1_8}{Statcan 2021 census abbreviations})
#'   \item `date`: report date
#'   \item `count`: count of reported cases for the previous week
#' }
"cl.agg"

