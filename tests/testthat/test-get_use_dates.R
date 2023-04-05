test_that("get_use_dates() works", {
  # set up inputs
  start_date <- as.Date("2023-04-05")
  n <- 40
  agg.window <- 10 # must divide n
  date.vec <- seq(start_date, start_date + (n-1), by = 1)
  t.vec <- 1:n
  cl.daily <- bind_rows(
    tibble(
      id = as.integer(1),
      date = date.vec,
      t = t.vec,
      value = rpois(n, lambda = 1)+1 # avoid zeroes
    ),
    tibble(
      id = as.integer(2),
      date = date.vec,
      t = t.vec,
      value = rpois(n, lambda = 1)+2
    ))

  cl.agg <- (cl.daily
    %>% mutate(group = rep(1:(n/agg.window), each = agg.window),
               rownum = 1:nrow(.))
    %>% group_by(group)
    %>% mutate(count = sum(value))
    %>% ungroup()
    %>% filter(rownum %% agg.window == 0)
    %>% select(date, count)
  )

  prm.daily.check <- list(
    agg.reldiff.tol = 10
  )

  get_use_dates(
    cl.daily,
    cl.agg,
    prm.daily.check
  )
})
