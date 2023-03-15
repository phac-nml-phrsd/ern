#' @title Smoothing ww data
#' @description Function takes cleaned ww data and smooths data
#'
#' @inheritParams estimate_R_ww
#'
#' @importFrom rlang .data
#'
#' @return dataframe with smoothed ww data
#'
#' @export
smooth_ww <- function(ww.conc, prm.smooth, silent = FALSE){

  # Checking if prm.smooth contains smoothing method
  if(is.null(prm.smooth$method)){
    stop("Method is not specified in prm.smooth. Please specify a smoothing
         method. Aborting!")
  }
  else if(!isTRUE(prm.smooth$method %in% c('rollmean', 'loess'))){
    stop("Invalid smoothing method. Please specify either rollmean or loess
         as smoothing in prm.smooth. Aborting!")
  }

  if(prm.smooth$method == 'rollmean'){
    if(is.null(prm.smooth$window) | prm.smooth$window <= 0){
      stop("Missing or invalid rollmean window. Please specify a rollmean value
           that is greater than 0 in prm.smooth. Aborting!")
    }
    if(is.null(prm.smooth$align) |
       !isTRUE(prm.smooth$align %in% c('center', 'left', 'right'))){
      stop("Missing or invalid rollmean alignment. Please specify a valid
           alignment in prm.smooth. Aborting!")
    }
    d = ww.conc %>%
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
    if(is.null(prm.smooth$span) | prm.smooth$span <= 0){
      stop("Missing or invalid loess span. Please specify a loess span that is
           greater than 0 in prm.smooth. Aborting!")
    }
    t = ww.conc %>%
      dplyr::mutate(x = as.numeric(date - min(date, na.rm = TRUE)+1))

    z = stats::loess(formula = 'val ~ x', data = t, span = prm.smooth$span)
    x = z$x[,1]
    v = z$fitted

    d = stats::approx(x=x, y = v, xout = 1:max(x)) %>%
      as.data.frame() %>%
      dplyr::rename(t = x, obs = .data$y)

    d[["date"]] = lubridate::ymd(min(t$date)) + d[["t"]]
  }

  if(!silent){
    message(paste("Wastewater data smoothed using", prm.smooth$method,
                  "method"))
  }

  return(d)
}
