## set up a cl.input for tests
cl.input <- (ern::cl.input
   |> dplyr::filter(pt == "on")
   |> dplyr::slice(1:6)
   |> dplyr::mutate(t = seq(7, 42, by = 7))
)
