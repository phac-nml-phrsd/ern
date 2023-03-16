#' @param prm.smooth List. list of smoothing parameters. Parameters should be
#'  specified as followed:
#'  \enumerate{
#'   \item method - smoothing method (either 'rollmean' or 'loess')
#'   \item window - smoothing window (number of days for rollmean smoothing)
#'   \item align - smoothing alignment (for rollmean smoothing, either
#'    'center', 'left', 'right')
#'   \item span - smoothing span (for loess smoothing)
#'  }
