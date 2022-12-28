


#' @title Smoothing ww data
#' @description Function takes cleaned ww data and smooths data
#' @param df ww dataframe
#' @param start.date desired start date
#' @param prm.smooth list of smoothing parameters
#' @return dataframe with smoothed ww data
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
    filter(date >= date.start)

  if(prm.smooth$method == 'rollmean'){
    d = tmp %>%
      complete(date = seq.Date(first(date), last(date), by = "day")) %>%
      mutate(val_smooth = rollmean(x = val,
                                   k = prm.smooth$window,
                                   align = prm.smooth$align,
                                   fill  = NA,
                                   na.rm = TRUE)) %>%
      drop_na(val_smooth) %>%
      mutate(t = as.numeric(date - first(date)),
             obs = val_smooth)
  }

  if(prm.smooth$method == 'loess'){
    t = tmp %>%
      mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

    z = loess(formula = 'val ~ x', data = t, span = prm.smooth$span)
    x = z$x[,1]
    v = z$fitted

    d = approx(x=x, y = v, xout = 1:max(x)) %>%
      as.data.frame() %>%
      rename(t = x, obs = y)

    d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]
  }

  return(d)
}
