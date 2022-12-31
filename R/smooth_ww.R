#' @title Smoothing ww data
#' @description Function takes cleaned ww data and smooths data
#'
#' @param df ww dataframe
#' @param date.start desired start date
#' @param prm.smooth list of smoothing parameters
#'
#' @return dataframe with smoothed ww data
#'
#' @export
smooth_ww <- function(df, date.start, prm.smooth){

  # Selection of the time period requested:
  date.start = lubridate::ymd(date.start)
  date.min = min(df$date)

  if(date.min > date.start) {
    warning('Start date before first data.')
    date.start = date.min
  }

  tmp = df %>%
    dplyr::filter(date >= date.start)

  if(prm.smooth$method == 'rollmean'){
    d = tmp %>%
      tidyr::complete(date = seq.Date(dplyr::first(date),
                                      dplyr::last(date), by = "day")) %>%
      dplyr::mutate(val_smooth =
                      zoo::rollmean(x = val,
                                    k = prm.smooth$window,
                                    align = prm.smooth$align,
                                    fill  = NA,
                                    na.rm = TRUE)) %>%
      tidyr::drop_na(val_smooth) %>%
      dplyr::mutate(t = as.numeric(date - first(date)),
                    obs = val_smooth)
  }

  if(prm.smooth$method == 'loess'){
    t = tmp %>%
      dplyr::mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

    z = stats::loess(formula = 'val ~ x', data = t, span = prm.smooth$span)
    x = z$x[,1]
    v = z$fitted

    d = stats::approx(x=x, y = v, xout = 1:max(x)) %>%
      as.data.frame() %>%
      dplyr::rename(t = x, obs = y)

    d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]
  }

  return(d)
}
