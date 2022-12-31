#' Smooth realizations from estimating daily reports
#'
#' @param cl.daily Dataframe resulting from [`weekly_to_daily()`]
#' @param prm.smooth smoothing parameters
#'
#' @return Dataframe
#' @export
smooth_cl <- function(cl.daily, prm.smooth){

  (cl.daily
   %>% dplyr::group_by(id)
   %>% dplyr::mutate(
     window = dplyr::case_when(
       t < prm.smooth$window ~ t,
       T ~ as.integer(prm.smooth$window)
     )
   )
   %>% dplyr::mutate(
     value = data.table::frollmean(
       value, n = window,
       align = "right", adaptive = TRUE)
   )
   %>% dplyr::select(-window)
   %>% dplyr::ungroup()
  )

}
