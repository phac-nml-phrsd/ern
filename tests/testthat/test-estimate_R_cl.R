popsize = 14.7e6

test_that("estiamte_R_cl() returns a message when prm.daily.check is not NULL and silent mode is off", {
  expect_message(estimate_R_cl(
    cl.agg %>% dplyr::filter(pt == "on"),
    dist.repdelay,
    dist.repfrac,
    dist.incub,
    dist.gi,
    popsize = popsize,
    prm.daily = prm.daily,
    prm.daily.check = prm.daily.check,
    prm.smooth = prm.smooth,
    prm.R = prm.R,
    silent = FALSE
  ))
})

test_that("estiamte_R_cl() returns output of the expected type", {
  res <- estimate_R_cl(
    cl.agg %>% dplyr::filter(pt == "on"),
    dist.repdelay,
    dist.repfrac,
    dist.incub,
    dist.gi,
    popsize = popsize,
    prm.daily = prm.daily,
    prm.daily.check = prm.daily.check,
    prm.smooth = prm.smooth,
    prm.R = prm.R,
    silent = TRUE
  )

  print(res)

  expect_equal(
    class(res),
    "list"
  )

  expect_equal(
    names(res),
    c("cl.agg", "cl.daily", "inferred.agg", "R")
  )

  test_output_tibble(
    res$inferred.agg,
    col_name = c("date", "obs", "mean.agg", "lwr.agg", "upr.agg"),
    col_class = c("Date", rep("numeric", 4))
  )

  test_output_tibble(
    res$R,
    col_name = c("date", "mean", "lwr", "upr", "use"),
    col_class = c("Date", rep("numeric", 3), "logical")
  )
})
