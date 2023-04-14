format_ww.smooth <- function(ww.conc){
  ww.smooth <- (ww.conc
    %>% dplyr::transmute(
      t = 1:nrow(.),
      obs = value,
      date)
  )

  return(ww.smooth)
}
