#' @title Sample of aggregated clinical reports
#'
#' @description A subset of COVID-19 weekly reports in the Government of Canada Health Infobase
#'
#' @format ## `cl.input`
#' A data frame with 96 rows and 3 columns:
#' \itemize{
#'   \item `pt`: standard two-character abbreviation (lowercase) of the province name (based on Statcan 2021 census abbreviations)
#'   \item `date`: report date
#'   \item `value`: count of reported cases for the previous week
#' }
#'
#' Filter indicating a specific province to extract a sample dataset for use with [estimate_R_cl()], _e.g._
#'
#' `estimate_R_cl(cl.input = dplyr::filter(cl.input, pt == 'bc'), ...)`
"cl.input"

#' @title Sample of wastewater concentration
#'
#' @description A subset of SARS-CoV-2 (N2 gene) concentration data in wastewater sampled from the Iona Island wastewater treatment plant in Vancouver between 7 July 2023 and 5 November 2023.
#'
#' @format ## `ww.input`
#' A data frame with 47 rows and 3 columns:
#' \itemize{
#'   \item `date`: report date
#'   \item `value`: mean sample concentration between multiple replicates
#' }
"ww.input"
