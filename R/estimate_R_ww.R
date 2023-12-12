#' @title Estimate the effective reproduction from wastewater concentration
#'  data.
#'
#' @param ww.conc Data frame. Must have variables:
#' \itemize{
#'  \item `date`: calendar date of wastewater collection
#'  \item `value`: pathogen concentration
#' }
#' @param dist.fec List. Parameters for the fecal shedding distribution in the same format as returned by [`def_dist_fecal_shedding()`].
#' @template param-dist.gi
#' @param scaling.factor Numeric. Scaling from wastewater concentration to
#'  prevalence. This value may be assumed or independently calibrated to data.
#' @template param-prm.smooth
#' @template param-prm.R
#' @template param-silent
#' @param RL.max.iter Integer. Maximum of iterations for the Richardson-Lucy deconvolution algorithm.
#' @return List. Elements include:
#' \itemize{
#'  \item `ww.conc`: original wastewater signal
#'  \item `ww.smooth`: smoothed wastewater signal
#'  \item `inc`: inferred incidence
#'  \item `R`: the effective reproduction number estimate
#' }
#'
#' @export
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
      iter   = 10,
      CI     = 0.95,
      window = 7,
      config.EpiEstim = NULL
    ),
    silent = FALSE,
    RL.max.iter = 9
) {

  # Checking arguments
  check_prm.R(prm.R, silent = silent)

  # Checking if ww.conc df contains required variables
  check_ww.conc_format(ww.conc)

  # Check that the input data is daily
  # OK if smoothing as there will be interpolation,
  # not OK if smoothing is turned off
  is.daily <- check_df.input_daily(ww.conc)

  if(!is.daily & is.null(prm.smooth)) stop("You have input non-daily wastewater concentration data. This data must be smoothed in order to be interpolated into a daily signal as required by `ern`. Please specify the `prm.smooth` argument of `estimate_R_ww()`.")

  # Attach internal time index column

  # Smooth the wastewater signal, if requested
  if(!is.null(prm.smooth)){
    ww.smooth <- smooth_ww(
      ww.conc = ww.conc,
      prm.smooth = prm.smooth,
      silent = silent
    )
  } else {
    warning("\n-----
You are not passing smoothing parameters.
Smoothing parameters are strongly recommended
to obtain accurate Rt estimates using wastewater data.\n")

    # format
    ww.smooth <- (ww.conc
      |> attach_t()
      |> dplyr::transmute(
        t,
        obs = value,
        date
      ))
  }

  # Infer the incidence deconvoluting the (smoothed) wastewater signal
  # and using the fecal shedding distribution as the kernel
  # Use the estimated incidence to calculate R:
  r = lapply(
    X           = 1:prm.R$iter,
    FUN         = inc2R_one_iter,
    dist.gi     = dist.gi,
    dist.fec    = dist.fec,
    ww.conc     = ww.smooth,
    scaling.factor = scaling.factor,
    prm.R       = prm.R,
    silent      = silent,
    RL.max.iter = RL.max.iter
  )

  inc = lapply(r, `[[`, 'inc') |>
    dplyr::bind_rows() |>
    dplyr::transmute(value = I, date) |>
    summarise_by_date_iters()

  rt = lapply(r, `[[`, 2) |>
    dplyr::bind_rows() |>
    summarise_by_date_ens(CI = prm.R$CI)

  return(list(
    ww.conc   = ww.conc,
    ww.smooth = ww.smooth,
    inc       = inc,
    R         = rt
  ))
}



#' Helper function.
#' Converts wastewater to Rt after sampling one fecal shedding and
#'  one generation interval distribution.
#'
#' @param i Numeric. Iteration index. (not used but required when using
#'  `lapply()`)
#' @inheritParams estimate_R_ww
#' @template param-silent
#' @param RL.max.iter Integer. Maximum of iterations for the Richardson-Lucy deconvolution algorithm.
#'
#' @return List. Elements include `inc` (incidence) and `rt`
#'  (reproduction number)
inc2R_one_iter <- function(i, dist.fec, dist.gi, ww.conc,
                           scaling.factor, prm.R, silent,
                           RL.max.iter) {
  # set.seed(i)
  sample.fec = sample_a_dist(dist = dist.fec)
  sample.gi  = sample_a_dist(dist = dist.gi)

  inc = deconv_ww_inc(d              = ww.conc,
                      fec            = sample.fec,
                      scaling.factor = scaling.factor,
                      silent         = silent,
                      RL.max.iter    = RL.max.iter)

  i.df = inc[["inc"]] |>
    dplyr::mutate(I = inc.deconvol) |>
    dplyr::select(date,I, t) |>
    tidyr::drop_na() |>
    dplyr::mutate(iter = i)

  rt = incidence_to_R(incidence = i.df,
                      generation.interval = sample.gi,
                      prm.R = prm.R) |>
    dplyr::mutate(iter = i)

  r = list(
    inc = i.df,
    rt = rt
  )
  return(r)
}
