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
