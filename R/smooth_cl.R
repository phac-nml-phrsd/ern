#' Smooth realizations from estimating daily reports
#'
#' @param cl.daily Dataframe resulting from [`agg_to_daily()`]
#' @param prm.smooth smoothing parameters
#'
#' @importFrom rlang .data
#'
#' @return Dataframe
#' @export
smooth_cl <- function(cl.daily, prm.smooth){

  if(is.null(prm.smooth)) res = cl.daily

  if(!is.null(prm.smooth)){
    res =   (cl.daily
             %>% dplyr::group_by(.data$id)
             %>% dplyr::mutate(
               window = dplyr::case_when(
                 t < prm.smooth$window ~ t,
                 T ~ as.integer(prm.smooth$window)
               )
             )
             %>% dplyr::mutate(
               value = data.table::frollmean(
                 .data$value, n = .data$window,
                 align = "right", adaptive = TRUE)
             )
             %>% dplyr::select(-.data$window)
             %>% dplyr::ungroup()
    )
  }
  return(res)
}
