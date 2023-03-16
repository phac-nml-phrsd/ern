#' @param prm.R List. Settings for the ensemble when calculating Rt.
#'  Elements include:
#'  \itemize{
#'   \item `window`: number of days defining the window of data used by
#'    `EpiEstim` to estimate Rt
#'   \item `CI`: Numeric between 0 and 1. Confidence interval width for Rt
#'   estimates after sampling uncertain distributions.
#'   \item `config.EpiEstim`: configuration for `EpiEstim` defined via
#'    [EpiEstim::make_config()]. If `NULL`, will use default config from
#'    `EpiEstim`.
#' }
