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

  if(is.null(prm.smooth)) return(cl.daily)

  if(!is.null(prm.smooth)){
    check_prm.smooth(prm.smooth)
    df <- (cl.daily
       %>% dplyr::group_by(.data$id)
       %>% dplyr::mutate(
         value = zoo::rollapply(
           .data$value, width = prm.smooth$window,
           FUN = mean, align = "center", partial = TRUE)
       )
       %>% dplyr::ungroup()
    )
  }
}
