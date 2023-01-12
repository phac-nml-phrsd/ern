test_that("weekly to daily report inference runs", {

  x <- weekly_to_daily(
    cl.weekly,
    dist.gi,
    popsize,
    prm.daily
  )
})
