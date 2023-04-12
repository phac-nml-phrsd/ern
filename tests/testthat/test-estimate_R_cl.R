popsize = 14.7e6


# outputs --------------------------------------------------

test_that("estimate_R_cl() returns output of the expected type", {
res <- estimate_R_cl(
  cl.input %>% dplyr::filter(pt == "on"),
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

expect_equal(
  class(res),
  "list"
)

expect_equal(
  names(res),
  c("cl.input", "cl.daily", "inferred.agg", "R")
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

test_that("estiamte_R_cl() returns a message when prm.daily.check is not NULL, input data is not daily, and silent mode is off", {
  expect_message(estimate_R_cl(
    cl.input %>% dplyr::filter(pt == "on"),
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
  ),
  regexp = "Aggregating inferred daily reports back using the original
reporting schedule")
})


# daily input data --------------------------------------------------------

cl.daily.test <- (cl.daily
%>% dplyr::select(date, value)
%>% dplyr::rename(count = value)
)

test_that("estimate_R_cl() skips JAGS step and smoothing if input data is already daily and prm.smooth is NULL", {

    # check that the input data is actually daily
    expect_true(
      check_cl.input_daily(
        cl.daily.test,
        silent = TRUE
      )
    )

    res <- estimate_R_cl(
      cl.daily.test,
      dist.repdelay,
      dist.repfrac,
      dist.incub,
      dist.gi,
      popsize = popsize,
      prm.daily = prm.daily,
      prm.daily.check = prm.daily.check,
      prm.smooth = NULL, # turn off smoothing
      prm.R = prm.R,
      silent = TRUE
    )

    expect_identical(
      res$cl.input,
      res$cl.daily %>% dplyr::transmute(date, count = value)
    )

    # verify this is still OK if popsize, prm.daily and prm.daily.check are NULL
    res2 <- estimate_R_cl(
      cl.daily.test,
      dist.repdelay,
      dist.repfrac,
      dist.incub,
      dist.gi,
      popsize = NULL,
      prm.daily = NULL,
      prm.daily.check = NULL,
      prm.smooth = NULL, # turn off smoothing
      prm.R = prm.R,
      silent = TRUE
    )

    expect_identical(
      res2$cl.input,
      res2$cl.daily %>% dplyr::transmute(date, count = value)
    )
})



# smoothing ---------------------------------------------------------------

test_that("estimate_R_cl() smooths daily input data (but skips JAGS step) with smoothing is turned on", {

  res <- estimate_R_cl(
    cl.daily.test,
    dist.repdelay,
    dist.repfrac,
    dist.incub,
    dist.gi,
    popsize = NULL,
    prm.daily = NULL,
    prm.daily.check = NULL,
    prm.smooth = prm.smooth,
    prm.R = prm.R,
    silent = FALSE
  )

  # calculate expected
  # (do manual smoothing of input data)
  input.count.smoothed <- zoo::rollapply(
    cl.daily.test$count,
    width = prm.smooth$window,
    FUN = mean,
    align = "center",
    partial = TRUE)

  expect_identical(
    res$cl.daily$value, # output
    input.count.smoothed, # expected
  )

})





