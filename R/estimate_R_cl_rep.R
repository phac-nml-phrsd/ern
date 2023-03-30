#' Ensemble estimate of Rt
#'
#' @param cl.daily Dataframe of inferred daily incidence.
#' @inheritParams estimate_R_cl
#'
#' @return A summary of individual Rt realizations with 95% confidence intervals
#' @export
#' @seealso [EpiEstim::make_config()]
estimate_R_cl_rep <- function(
    cl.daily,
    dist.repfrac,
    dist.repdelay,
    dist.incub,
    dist.gi,
    prm.R,
    silent = FALSE
){

  if(is.null(prm.R$config.EpiEstim) & !silent) message("Using default config in `EpiEstim::estimate_R()`.")

  # TODO: parallelize in a generic way (using a custom
  # iteration function that we use both on the clinical
  # and ww side)
  R = NULL
  for(i in 1:prm.R$iter){
    R = dplyr::bind_rows(
      R,
      estimate_R_cl_single(
        cl.daily,
        dist.repfrac,
        dist.repdelay,
        dist.incub,
        dist.gi,
        prm.R,
        silent = silent
      )
    )
  }

  # return summary of ensembles
  (R
    %>% summarise_by_date_ens(CI = prm.R$CI) # 95% CIs
    # flag which points to trust or not
    # beginning of estimate takes a bit to converge
    # use one max generation interval as rule of thumb
    %>% dplyr::mutate(use = (date >= min(date) + lubridate::days(dist.gi$max)))
  )

}
