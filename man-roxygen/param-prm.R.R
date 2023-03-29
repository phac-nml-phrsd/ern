#' @param prm.R List. Settings for the ensemble when calculating Rt.
#'  Elements include:
#'  \itemize{
#'   \item `iter`: Integer. Number of iterations for the Rt ensemble
#'   \item `CI`: Numeric between 0 and 1. Confidence interval width for Rt
#'   estimates after sampling uncertain distributions. If `NULL`, will default to 0.95.
#'   \item `window`: Integer. Number of days defining the window of data used by
#'    `EpiEstim` to estimate Rt. If `NULL`, will default to 7.
#'   \item `config.EpiEstim`: (optional) configuration for `EpiEstim` defined via
#'    [EpiEstim::make_config()]. If `NULL`, will use default config from
#'    `EpiEstim`.
#' }
