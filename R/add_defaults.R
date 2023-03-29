#' Add defaults for `prm.daily`
#'
#' @param x List. `prm.daily`.
#'
#' @return List. Updated version of `prm.daily`.
#'
#' @seealso estimate_R_ww estimate_R_cl
add_defaults_prm.daily <- function(x){
  (x
    %>% set_default("burn", 100)
    %>% set_default("iter", 500)
    %>% set_default("chains", 1)
   )
}

#' Add defaults for `prm.R`
#'
#' @param x List. `prm.R`.
#'
#' @return List. Updated version of `prm.R`.
#'
#' @seealso estimate_R_ww estimate_R_cl
add_defaults_prm.R <- function(x){
  (x
    %>% set_default("CI", 0.95)
    %>% set_default("window", 7)
  )
}

#' Set a default value in a list
#'
#' @param x List.
#' @param name String. Name of the list element.
#' @param value Default value.
#'
#' @return List.
#'
set_default <- function(x, name, value){
  assertthat::assert_that(assertthat::is.string(name))

  if(!(name %in% names(x))){
    x[name] <- value
  }

  return(x)
}
