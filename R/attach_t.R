#' @title Attach internal time index column
#'
#' @param df Data frame. Must have column `date`.
#' @keywords internal
#' @return Tibble.
attach_t <- function(df) {
  (df
   |> dplyr::mutate(t = as.integer(date - min(date, na.rm = TRUE)+1)))
}
