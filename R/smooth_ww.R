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

  # rollmean
  if(prm.smooth$method == 'rollmean'){
    d = ww.conc %>%
      tidyr::complete(date = seq.Date(dplyr::first(date),
                                      dplyr::last(date), by = "day")) %>%
      dplyr::mutate(value_smooth =
                      zoo::rollmean(x = value,
                                    k = prm.smooth$window,
                                    align = prm.smooth$align,
                                    fill  = NA,
                                    na.rm = TRUE)) %>%
      tidyr::drop_na(value_smooth) %>%
      dplyr::mutate(t = as.numeric(date - dplyr::first(date)),
                    obs = value_smooth)
  }

  # loess
  if(prm.smooth$method == 'loess'){
    t = ww.conc %>%
      dplyr::mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

    z = stats::loess(formula = 'value ~ x', data = t, span = prm.smooth$span)
    x = z$x[,1]
    v = z$fitted

    d = stats::approx(x=x, y = v, xout = 1:max(x)) %>%
      as.data.frame() %>%
      dplyr::rename(t = x, obs = y)

    d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]
  }

  if(!silent){
    message(paste("Wastewater data smoothed using", prm.smooth$method,
                  "method"))
  }

  return(d)
}
