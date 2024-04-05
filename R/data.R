#' @title Sample of aggregated clinical reports
#'
#' @description A subset of COVID-19 weekly reports in the Government of Canada Health Infobase.
#' See \url{https://health-infobase.canada.ca/covid-19/}
#'
#' @format ## `cl.data`
#' A data frame with 96 rows and 3 columns:
#' \itemize{
#'   \item `pt`: standard two-character abbreviation (lowercase) 
#'   of the province name (based on Statistics Canada 2021 census abbreviations)
#'   \item `date`: report date
#'   \item `value`: count of reported cases for the previous week
#' }
#'
#' Filter indicating a specific province to extract a sample dataset for use with [estimate_R_cl()], _e.g._
#'
#' `estimate_R_cl(cl.data = dplyr::filter(cl.data, pt == 'bc'), ...)`
"cl.data"

#' @title Sample of wastewater concentration
#'
#' @description A subset of SARS-CoV-2 (N2 gene) concentration data in 
#' wastewater sampled from the Iona Island wastewater treatment plant 
#' in Vancouver between 7 July 2023 and 5 November 2023.
#' Units are in N2 gene copies per milliliter of wastewater. 
#' Concentration was measured using RT-qPCR assays; RNA was extracted from
#' suspended solids. 
#' See \url{https://health-infobase.canada.ca/covid-19/wastewater/}
#' 
#' @format ## `ww.data`
#' A data frame with 47 rows and 3 columns:
#' \itemize{
#'   \item `date`: sampling date
#'   \item `value`: mean sample concentration between multiple replicates
#' }
"ww.data"
