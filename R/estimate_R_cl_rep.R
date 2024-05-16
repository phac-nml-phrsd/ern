#' Ensemble estimate of Rt
#'
#' @param cl.daily Dataframe of inferred daily incidence.
#' @inheritParams estimate_R_cl
#' @keywords internal
#' @return A summary of individual Rt realizations with 95% confidence intervals
#' @seealso [EpiEstim::make_config()]
#' 
estimate_R_cl_rep <- function(
    cl.daily,
    dist.repfrac,
    dist.repdelay,
    dist.incub,
    dist.gi,
    prm.R,
    silent = FALSE,
    RL.max.iter = 10
){

  if(is.null(prm.R$config.EpiEstim) & !silent) 
    message("Using default config in `EpiEstim::estimate_R()`.")

  # TODO: parallelize in a generic way (using a custom
  # iteration function that we use both on the clinical
  # and ww side)

  tmp = tmpnew = list()
  
  for(i in 1:prm.R$iter){
    tmp[[i]] = estimate_R_cl_single(
      cl.daily      = cl.daily,
      dist.repfrac  = dist.repfrac,
      dist.repdelay = dist.repdelay,
      dist.incub    = dist.incub,
      dist.gi       = dist.gi,
      prm.R         = prm.R,
      silent        = silent,
      RL.max.iter   = RL.max.iter
    )
  }
  R = dplyr::bind_rows(tmp) 
  
  # Thu May 16 07:43:15 2024 ------------------------------
  
  for(i in 1:prm.R$iter){
    tmpnew[[i]] = estimate_R_cl_single_new(
      cl.daily      = cl.daily,
      dist.repfrac  = dist.repfrac,
      dist.repdelay = dist.repdelay,
      dist.incub    = dist.incub,
      dist.gi       = dist.gi,
      prm.R         = prm.R,
      silent        = silent,
      RL.max.iter   = RL.max.iter
    )
  }
  Rnew = dplyr::bind_rows(tmpnew) |> filter(!is.na(date))
  

  # Return summary of ensembles
  res = R |>
    summarise_by_date_ens(CI = prm.R$CI) |>  
    # flag which points to trust or not
    # beginning of estimate takes a bit to converge
    # use one max generation interval as rule of thumb
    dplyr::mutate(use = (date >= min(date, na.rm = TRUE) + lubridate::days(dist.gi$max))) |>
    dplyr::mutate(foo = 'old')

  resnew = Rnew |> 
    dplyr::group_by(date) |>
    dplyr::summarise(
      mean = mean(postsample),
      lwr  = quantile(postsample, probs = 0.5 - prm.R$CI / 2),
      upr  = quantile(postsample, probs = 0.5 + prm.R$CI / 2)
    ) |>
    dplyr::mutate(use = (date >= min(date, na.rm = TRUE) + lubridate::days(dist.gi$max))) |>
    dplyr::mutate(foo = 'new')
  
  
  foofoo = rbind(res,resnew)
  return(foofoo)
  
  return(res)
}
