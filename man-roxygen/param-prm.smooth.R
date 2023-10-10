#' @param prm.smooth List. list of smoothing parameters. Parameters should be
#'  specified as followed:
#'  \itemize{
#'   \item `method`: smoothing method, either `'rollmean'` (rolling mean) or `'loess'` (LOESS smoothing)
#'   \item `window`: for `method = 'rollmean` only; width of smoothing window in days
#'   \item `align`: for `method = 'rollmean` only; smoothing alignment, either `'center'`, `'left'`, `'right'`
#'   \item `span`: for `method = 'loess'` only; smoothing span (see the documentation for `stats::loess()` for details)
#'   \item `floor`: optional call for wastewater concentration smoothing with `method = 'loess'` only; user defined minimum smoothing concentration
#'  }
#'  Set this entire list to `NULL` to turn off smoothing
