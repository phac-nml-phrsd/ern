## set up a cl.data for tests
rows <- 7:14
cl.data <- (ern::cl.data
   |> dplyr::filter(pt == "on")
   |> dplyr::slice(rows)
   |> dplyr::mutate(t = seq(7, 7*length(rows), by = 7))
)
