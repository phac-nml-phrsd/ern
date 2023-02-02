#' Check parameters for Rt calculation
#'
#' @param x List of parameters for Rt calculation
#'
#' @return NULL
check_prm.R <- function(x){

  # Check config.EpiEstim
  if(!is.null(x$config.EpiEstim)){
    message("-----
You are passing your own config for EpiEstim::estimate_R.
Please note that ern always uses method = 'si_from_sample',
and thus any method specified in your config will be ignored.
Also, any config parameters that are specific to
method = 'si_from_sample' (like n2) cannot be modified and
will also be ignored.")
  }

}

#' Check distributions
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
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
}
