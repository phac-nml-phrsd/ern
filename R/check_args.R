#' Check parameters for Rt calculation
#'
#' @param x List of parameters for Rt calculation
#'
#' @return NULL
check_prm.R <- function(x){

  # Check config.EpiEstim
  if(!is.null(x$config.EpiEstim)){
    message("-----
You are passing your own config for EpiEstim::estimate_R().
Please note that ern always uses method = 'si_from_sample',
and thus any method specified in your config will be ignored.
Also, any config parameters that are specific to
method = 'si_from_sample' (like n2) cannot be modified and
will also be ignored.")
  }

  return()
}

#' Check distributions
#'
#' @param x
#'
#' @return NULL
check_dist <- function(x){
  if(x$dist == "gamma"){
    if(!("sd" %in% names(x) | "shape" %in% names(x))){
      stop(paste0("Gamma distributions must be specified with a mean and one of
a standard deviation (sd) or a shape parameter (shape).
Neither sd nor shape found: ", print(x)))
    }
    if("sd" %in% names(x) & "shape" %in% names(x)){
      stop(paste0("Gamma distributions must be specified with a mean and either
one of a standard deviation (sd) or a shape parameter (shape).
Both sd and shape found: ", print(x)))
    }

  }

  return()
}

#' Check that deconvolution inputs are compatible
#'
#' @param obs numeric vector. signal (_e.g._, case reports)
#' @param dist numeric vector. deconvolution kernel (_e.g._, reporting delay distribution)
#'
#' @return NULL
check_for_deconv <- function(obs, dist){
  if(length(dist) > length(obs)) stop(paste0("For deconvolutions, length of distribution vector cannot exceed number of observations.
  - length of distribution vector: ", length(dist), "
  - number of observations: ", length(obs)))
  return()
}
