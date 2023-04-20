#' Format clinical data for estimate_R_cl_rep()
#'
#' @template param-cl.input
format_cl.daily <- function(cl.input) {
  (cl.input
   %>% attach_t()
   %>% dplyr::transmute(
     id = as.integer(1),
     date,
     t,
     value
   ))
}


# helpers -----------------------------------------------------------------

#' Attach time index column for daily input data
#'
#' @template param-cl.input
#'
#' @return Tibble.
attach_t <- function(cl.input) {
  (cl.input
   %>% dplyr::mutate(t = 1:nrow(.))
  )
}
