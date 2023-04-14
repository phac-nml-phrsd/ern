format_ww.smooth <- function(ww.conc){
  ww.smooth <- (ww.conc
    %>% dplyr::transmute(
      t = 1:nrow(.),
      obs = val,
      date)
  )

  return(ww.smooth)
}
