format_ww.smooth <- function(ww.conc){
  ww.smooth <- (ww.conc
    %>% dplyr::transmute(
      t = as.numeric(difftime(date, dplyr::first(date), units = "days")) + 1,
      obs = val,
      date)
  )

  return(ww.smooth)
}
