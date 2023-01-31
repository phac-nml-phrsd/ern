#' Ensemble estimate of Rt
#'
#' @inheritParams estimate_R_cl_single
#' @param prm.R settings for the ensemble
#'
#' @return A summary of individual Rt realizations with 95% confidence intervals
#' @export
#'
estimate_R_cl_rep <- function(
    cl.daily,
    dist.repfrac,
    dist.repdelay,
    dist.incub,
    dist.gi,
    prm.R = list(
      iter = 100
    )) {

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
        dist.gi
      )
    )
  }

  # return summary of ensembles
  (R
    %>% summarise_by_date() # 95% CIs
    # flag which points to trust or not
    # beginning of estimate takes a bit to converge
    # use one max generation interval as rule of thumb
    %>% dplyr::mutate(use = (date >= min(date) + lubridate::days(dist.gi$max)))
  )

}
