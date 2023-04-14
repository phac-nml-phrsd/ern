#' @title Smooth wastewater data
#' @description Function takes pre-processed wastewater data and smooths it
#'
#' @inheritParams estimate_R_ww
#'
#' @return Data frame
#'
#' @export
smooth_ww <- function(ww.conc, prm.smooth, silent = FALSE){
  browser()
  # check args
  check_prm.smooth(prm.smooth)

  # rollmean
  if(prm.smooth$method == 'rollmean'){
    d = ww.conc %>%
      tidyr::complete(date = seq.Date(dplyr::first(date),
                                      dplyr::last(date), by = "day")) %>%
      smooth_with_rollmean(prm.smooth = prm.smooth) %>%
      dplyr::mutate(
        t = as.numeric(date - dplyr::first(date)),
        obs = value
        )
  }

  # loess
  if(prm.smooth$method == 'loess'){
    d2 = smooth_with_loess(ww.conc, prm.smooth)
  }

  if(!silent){
    message(paste("Wastewater data smoothed using", prm.smooth$method,
                  "method"))
  }

  return(d)
}


# helpers -----------------------------------------------------------------

smooth_with_rollmean <- function(df, prm.smooth){
  d <- (df %>%
     dplyr::mutate(value_smooth =
                     zoo::rollmean(x = value,
                                   k = prm.smooth$window,
                                   align = prm.smooth$align,
                                   fill  = NA,
                                   na.rm = TRUE)) %>%
     tidyr::drop_na(value_smooth))

  return(d)
}

smooth_with_loess <- function(df, prm.smooth) {
  t = df %>%
    dplyr::mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

  z = stats::loess(formula = 'value ~ x', data = t, span = prm.smooth$span)
  x = z$x[,1]
  v = z$fitted

  d = stats::approx(x=x, y = v, xout = 1:max(x)) %>%
    as.data.frame() %>%
    ## TODO: EXTRACT THIS?
    dplyr::rename(t = x, obs = y)

  d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]

  return(d)
}
