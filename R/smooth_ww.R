#' @title Smooth wastewater data
#' @description Function takes pre-processed wastewater data and smooths it
#'
#' @inheritParams estimate_R_ww
#'
#' @return Data frame
#'
#' @export
smooth_ww <- function(ww.conc, prm.smooth, silent = FALSE){
  # check args
  check_prm.smooth(prm.smooth)

  # loess
  if(prm.smooth$method == 'loess'){
    d = smooth_with_loess(ww.conc, prm.smooth)
  }

  # rollmean
  if(prm.smooth$method == 'rollmean'){
    d = (ww.conc
     %>% tidyr::complete(date =
                           seq.Date(dplyr::first(date),
                                    dplyr::last(date),
                                    by = "day"))
     %>% smooth_with_rollmean(prm.smooth = prm.smooth)
    )
  }

  if(!silent){
    message(paste("Wastewater data smoothed using", prm.smooth$method,
                  "method"))
  }

  # format output
  d <- (d
    %>% as.data.frame()
    # re-attach time-index in case of interpolation
    %>% attach_t()
    # change output to suit ww methods
    %>% dplyr::transmute(
      t,
      obs = value_smooth,
      date
    )
  )

  return(d)
}


# helpers -----------------------------------------------------------------

smooth_with_rollmean <- function(df, prm.smooth){
  d <- (df
    %>% dplyr::mutate(
      value_smooth = zoo::rollmean(x = value,
                                   k = prm.smooth$window,
                                   align = prm.smooth$align,
                                   fill  = NA,
                                   na.rm = TRUE))
    %>% tidyr::drop_na(value_smooth)
    # standardize output
    %>% attach_t()
    %>% dplyr::transmute(
      date,
      value_smooth,
      t
    ))

  return(d)
}

smooth_with_loess <- function(df, prm.smooth) {

  # add time index t = 1, ..., nrow(df)
  df = attach_t(df)

  # fit LOESS model
  z = stats::loess(formula = 'value ~ t', data = df, span = prm.smooth$span)

  # extract time index and fitted values
  t = z$x[,"t"]
  v = z$fitted

  # interpolate in case of missing values
  d = (stats::approx(x = t, y = v, xout = 1:max(t))
    %>% as.data.frame()
    # standardize output
    %>% dplyr::transmute(
      t = x,
      value_smooth = y,
      date = lubridate::ymd(min(df$date)) + t
    )
    %>% dplyr::select(
      date, value_smooth, t
    )
    )

  return(d)
}
