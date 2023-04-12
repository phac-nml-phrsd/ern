## set up a cl.agg for tests
cl.agg <- (ern::cl.agg
   %>% dplyr::filter(pt == "on")
   %>% dplyr::slice(1:6)
   %>% dplyr::mutate(t = seq(7, 42, by = 7))
)
