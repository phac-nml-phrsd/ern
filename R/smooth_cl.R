#' Smooth realizations from estimating daily reports
#'
#' @param cl.daily Data frame. Output of [`agg_to_daily()`].
#' @inheritParams estimate_R_cl
#'
#' @importFrom rlang .data
#'
#' @return Data frame
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
