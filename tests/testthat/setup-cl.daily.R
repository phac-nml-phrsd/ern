n <- 70
start_date <- as.Date("2023-04-11")
date <- seq(start_date, start_date + lubridate::days(n-1), by = 1)
value <- (1:n*10)^2
cl.daily <- tibble::tibble(
  id = as.integer(1),
  date = date,
  t = 1:n,
  value = value
)
