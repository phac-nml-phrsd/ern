
#' Helper function.
#' Converts wastewater to Rt after sampling one fecal shedding and
#'  one generation interval distribution.
#'
#' @param i Iteration index. (not used but required when using `lapply()`)
#' @param dist.fec Fecal shedding distribution.
#' @param dist.gi Generation interval distribution.
#' @param wastewater Smoothed wastewater time-series.
#' @param scaling.factor Scaling from wastewater concentration to prevalence.
#'  This value may be assumed or independently calibrated to data.
#' @param prm.R List of configuration parameters for EpiEstim.
#'
#' @return
inc2R_one_iter <- function(i, dist.fec, dist.gi, wastewater,
                         scaling.factor, prm.R) {
  set.seed(i)
  sample.fec = sample_a_dist(dist = dist.fec)
  sample.gi = sample_a_dist(dist = dist.gi)

  inc = deconv_ww_inc(d              = wastewater,
                      fec            = sample.fec,
                      scaling.factor = scaling.factor)

  i.df = inc[["inc"]] %>%
    dplyr::mutate(I = .data$inc.deconvol) %>%
    select(date,I, t) %>%
    tidyr::drop_na()

  rt = incidence_to_R(incidence = i.df,
                       generation.interval = sample.gi,
                       prm.R = prm.R)

  r = list(
    i = i.df,
    rt = rt
  )
  return(r)
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
  # and using the fecal shedding distribution as the kernel
  # Use the estimated incidence to calculate R:
  r = lapply(X = 1:iter, FUN = inc2R_one_iter,
             dist.gi = dist.gi, dist.fec = dist.fec,
             wastewater = ww.smooth, scaling.factor = scaling.factor,
             prm.R = prm.R)

  i = lapply(r, `[[`, 1) %>%
    dplyr::bind_rows() %>%
    transmute(value = .data$I,
              date) %>%
    summarise_by_date_iters()

  rt = lapply(r, `[[`, 2) %>%
    dplyr::bind_rows() %>%
    summarise_by_date_ens()

  return(list(
    ww.conc   = ww.conc,
    ww.smooth = ww.smooth,
    inc       = i,
    R         = rt
  )
  )
}
