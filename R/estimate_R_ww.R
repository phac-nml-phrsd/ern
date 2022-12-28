

#' @title Estimate the effective reproduction from wastewater concentration data.
#'
#' @param ww.conc Dataframe. Must have variables named \code{date} for the wastewater
#' collection date and \code{value} for the pathogen concentration.
#' @param date.start Date. First date of observation used to estimate Rt.
#' @param dist.fec Numerical vector. Distribution of fecal shedding (time unit=day).
#' @param dist.gi Numerical vector. Distribution of the generation interval (time unit=day).
#' @param scaling.factor Numeric. Scaling from wastewater concentration to prevalence.
#' This value may be assumed or independently calibrated to data.
#' @param prm.smooth List. Parameters for the smoothing algorithm of the wastewater signal.
#'
#' @return An object.
#' @export
#'
#' @examples 1+1
#'
#'
estimate_R_ww <- function(ww.conc,
                          date.start,
                          dist.fec,
                          dist.gi,
                          scaling.factor = 1,
                          prm.smooth = list(
                            window = 14,
                            align  = 'center',
                            method = 'loess',
                            span   = 0.20
                          )
) {

  # Smooth the wastewater signal, if requested
  ww.smooth = ww.conc
  if(!is.null(prm.smooth)){
    ww.smooth = smooth_ww(df         = ww.conc,
                          date.start = date.start,
                          prm.smooth = prm.smooth)
  }

  # Infer the incidence deconvoluting the (smoothed) wastewater signal
  # and using the fecal shedding distribution as the kernel:
  inc = deconv_ww_inc(d              = ww.smooth,
                      fec            = dist.fec,
                      scaling.factor = scaling.factor)

  # Use the estimated incidence to calculate R:
  rt = inc_to_R(df         = inc[["inc"]],
                start.date = date.start,
                gi         = dist.gi)

  return(list(
    ww.conc   = ww.conc,
    ww.smooth = ww.smooth,
    inc       = inc[['inc']],
    R         = rt,
    date.start= date.start
  ))
}
