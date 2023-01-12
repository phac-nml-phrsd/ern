#' @title Smoothing ww data
#' @description Function takes cleaned ww data and smooths data
#'
#' @param df ww dataframe
#' @param prm.smooth list of smoothing parameters
#'
#' @importFrom rlang .data
#'
#' @return dataframe with smoothed ww data
#'
#' @export
smooth_ww <- function(df, prm.smooth){

  if(prm.smooth$method == 'rollmean'){
    d = df %>%
      tidyr::complete(date = seq.Date(dplyr::first(date),
                                      dplyr::last(date), by = "day")) %>%
      dplyr::mutate(val_smooth =
                      zoo::rollmean(x = .data$val,
                                    k = prm.smooth$window,
                                    align = prm.smooth$align,
                                    fill  = NA,
                                    na.rm = TRUE)) %>%
      tidyr::drop_na(.data$val_smooth) %>%
      dplyr::mutate(t = as.numeric(date - dplyr::first(date)),
                    obs = .data$val_smooth)
  }

  if(prm.smooth$method == 'loess'){
    t = df %>%
      dplyr::mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

    z = stats::loess(formula = 'val ~ x', data = t, span = prm.smooth$span)
    x = z$x[,1]
    v = z$fitted

    d = stats::approx(x=x, y = v, xout = 1:max(x)) %>%
      as.data.frame() %>%
      dplyr::rename(t = x, obs = .data$y)

    d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]
  }

  return(d)
}
