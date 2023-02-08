
#' Helper function.
#' Converts incidence to Rt after sampling one generation interval distribution
#'
#' @param i Iteration index. (not used but required when using `lapply()`)
#' @param dist.gi Generation interval distribution.
#' @param incidence Incidence object.
#' @param prm.R List of configuration parameters for EpiEstim.
#'
#' @return
inc2R_one_gi <- function(i, dist.gi, incidence, prm.R) {
  sample.gi = sample_a_dist(dist.gi)
  res = incidence_to_R(incidence = incidence,
                       generation.interval = sample.gi,
                       prm.R = prm.R)
  return(res)
}


#' @title Estimate the effective reproduction from wastewater concentration data.
#'
#' @param ww.conc Dataframe. Must have variables named \code{date} for the wastewater
#' collection date and \code{val} for the pathogen concentration.
#' @param dist.fec Numerical vector. Distribution of fecal shedding (time unit=day).
#' @param dist.gi Numerical vector. Distribution of the generation interval (time unit=day).
#' @param scaling.factor Numeric. Scaling from wastewater concentration to prevalence.
#' This value may be assumed or independently calibrated to data.
#' @param prm.smooth List. Parameters for the smoothing algorithm of the wastewater signal.
#' @param prm.R List. Settings for the ensemble when calculating Rt. Elements include:
#' \itemize{
#'  \item{`config.EpiEstim`: }{configuration for `EpiEstim` defined via `EpiEstim::make_config()`. if `NULL`, will use default config from `EpiEstim`.}
#' @param iter Integer. Number of samples for the (uncertain) generation interval distribution.
#' }
#' @return List. Elements include:
#' - `ww.conc` original wastewater signal
#' - `ww.smooth` smoothed wastewater signal
#' - `inc` inferred incidence
#' - `R` the effective R estimate
#'
#' @export
#'
#'
estimate_R_ww <- function(
    ww.conc,
    dist.fec,
    dist.gi,
    scaling.factor = 1,
    prm.smooth = list(
      window = 14,
      align  = 'center',
      method = 'loess',
      span   = 0.20
    ),
    prm.R = list(
      window = 7,
      config.EpiEstim = NULL
    ),
    iter = 100
) {

  # Checking arguments
  check_prm.R(prm.R)

  # Checking if ww.conc df contains required variables
  if(!isTRUE("date" %in% names(ww.conc)) |
     !isTRUE("val" %in% names(ww.conc))
     ){
    stop("date and value columns are required. Please check ww.conc.
         Aborting!")
  }

  # Smooth the wastewater signal, if requested
  ww.smooth = ww.conc
  if(!is.null(prm.smooth)){
    ww.smooth = smooth_ww(df         = ww.conc,
                          prm.smooth = prm.smooth)
  }

  # Infer the incidence deconvoluting the (smoothed) wastewater signal
  # and using the fecal shedding distribution as the kernel:
  inc = deconv_ww_inc(d              = ww.smooth,
                      fec            = dist.fec,
                      scaling.factor = scaling.factor)

  idf = inc[["inc"]] %>%
    dplyr::mutate(I = .data$inc.deconvol) %>%
    select(date,I, t) %>%
    tidyr::drop_na()

  # Use the estimated incidence to calculate R:
  r = lapply(X = 1:iter, FUN = inc2R_one_gi,
             dist.gi = dist.gi, incidence = idf, prm.R = prm.R) %>%
    dplyr::bind_rows()

  rt = r %>%
    summarise_by_date()

  return(list(
    ww.conc   = ww.conc,
    ww.smooth = ww.smooth,
    inc       = inc[['inc']],
    R         = rt
  )
  )
}
