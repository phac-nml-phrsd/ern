#' @title Helper function that summarises 
#' posterior samples of Rt from EpiEstim
#'
#' @param x dataframe 
#' @param prm.R list
#'
#' @return summarised dataframe
#' @keywords internal
#'
summary_postsamples <- function(x, prm.R) {
  
  res = x |>  
    dplyr::group_by(date) |>
    dplyr::summarise(
      mean = mean(postsample),
      lwr  = stats::quantile(postsample, probs = 0.5 - prm.R$CI / 2),
      upr  = stats::quantile(postsample, probs = 0.5 + prm.R$CI / 2)
    )
  return(res)
}


