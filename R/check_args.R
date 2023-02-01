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
