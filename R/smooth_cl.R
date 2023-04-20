#' Smooth realizations from estimating daily reports
#'
#' @param cl.daily Data frame. Output of [`agg_to_daily()`].
#' @inheritParams estimate_R_cl
#'
#' @return Data frame
#' @export
smooth_cl <- function(cl.daily, prm.smooth){

  check_prm.smooth(prm.smooth)

  (cl.daily
     %>% dplyr::group_by(id)
     %>% dplyr::mutate(
       value = zoo::rollapply(
         value, width = prm.smooth$window,
         FUN = mean, align = "center", partial = TRUE)
     )
     %>% dplyr::ungroup()
  )
}
