#' @title Smooth wastewater data (using pre-processed wastewater)
#'
#' @inheritParams estimate_R_ww
#'
#' @keywords internal
#' @return Data frame
#'
smooth_ww <- function(ww.conc, prm.smooth, silent = FALSE){
  # check args
  check_prm.smooth(prm.smooth)

  # loess
  if(prm.smooth$method == 'loess'){
    d = smooth_with_loess(df = ww.conc, prm.smooth)
  }

  # rollmean
  if(prm.smooth$method == 'rollmean'){
    d = (ww.conc
     |> tidyr::complete(date =
                           seq.Date(dplyr::first(date),
                                    dplyr::last(date),
                                    by = "day"))
     |> smooth_with_rollmean(prm.smooth = prm.smooth)
    )
  }

  if(!silent){
    message(paste("Wastewater data smoothed using", prm.smooth$method,
                  "method"))
  }

  # format output
  d <- (d
    |> as.data.frame()
    # re-attach time-index in case of interpolation
    |> attach_t()
    # change output to suit ww methods
    |> dplyr::transmute(
      t,
      obs = value_smooth,
      date
    )
  )

  return(d)
}

#' @title Smooth realizations from estimating daily reports
#'
#' @param cl.daily Data frame. Output of [`agg_to_daily()`].
#' @inheritParams estimate_R_cl
#'
#' @keywords internal
#' @return Data frame
smooth_cl <- function(cl.daily, prm.smooth){

  check_prm.smooth(prm.smooth)

  if(prm.smooth$method == 'rollmean') smooth_fun <- smooth_with_rollmean
  if(prm.smooth$method == 'loess') smooth_fun <- smooth_with_loess

  (cl.daily
    |> dplyr::group_by(id)
    # perform smoothing
    |> smooth_fun(prm.smooth = prm.smooth)
    |> dplyr::ungroup()
    # standardize output
    |> dplyr::rename(value = value_smooth)
  )
}


# smoothing methods -----------------------------------------------------------------

smooth_with_rollmean <- function(df, prm.smooth){
  d <- (df
    |> dplyr::mutate(
      value_smooth = zoo::rollapply(
        value, width = prm.smooth$window,
        FUN = mean, na.rm = TRUE, align = prm.smooth$align,
        partial = TRUE
    ))
    # standardize output
    |> attach_t()
    |> dplyr::transmute(
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
  
  # determine if concentration floor specified
  floor = ifelse(!is.null(prm.smooth$floor), TRUE, FALSE)

  # interpolate in case of missing values
  d = (stats::approx(x = t, y = v, xout = 1:max(t))
    |> as.data.frame()
    # standardize output
    |> dplyr::transmute(
      t = x,
      value_smooth = 
        if(isTRUE(floor))
          dplyr::case_when(
            y < prm.smooth$floor ~ prm.smooth$floor,
            y >= prm.smooth$floor ~ y)
        else y,
      date = lubridate::ymd(min(df$date)) + t
    )
    |> dplyr::select(
      date, value_smooth, t
    )
    )

  return(d)
}
