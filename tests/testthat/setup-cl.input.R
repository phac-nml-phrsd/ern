## set up a cl.input for tests
rows <- 7:14
cl.input <- (ern::cl.input
   |> dplyr::filter(pt == "on")
   |> dplyr::slice(rows)
   |> dplyr::mutate(t = seq(7, 7*length(rows), by = 7))
)
