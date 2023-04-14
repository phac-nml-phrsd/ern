#' Sample of aggregated clinical reports
#'
#' A subset of COVID-19 weekly reports in the Government of Canada Health Infobase
#'
#' @format ## `cl.input`
#' A data frame with 96 rows and 3 columns:
#' \itemize{
#'   \item `pt`: standard two-character abbreviation (lowercase) of the province name (based on \href{https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=T1_8}{Statcan 2021 census abbreviations})
#'   \item `date`: report date
#'   \item `count`: count of reported cases for the previous week
#' }
#'
#' Filter indicating a specific province to extract a sample dataset for use with [estimate_R_cl()], _e.g._
#'
#' `estimate_R_cl(cl.input = dplyr::filter(cl.input, pt == 'bc'), ...)`
"cl.input"

