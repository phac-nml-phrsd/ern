test_that("estimate_R_cl_rep() returns a message if using default EpiEstim config", {
  expect_message(estimate_R_cl_rep(
    cl.daily = cl.daily,
    dist.repfrac = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub = dist.incub,
    dist.gi = dist.gi,
    prm.R = prm.R,
    silent = FALSE
  ))
})

test_that("estimate_R_cl_rep() returns output in the expected format", {
  res <- estimate_R_cl_rep(
    cl.daily = cl.daily,
    dist.repfrac = dist.repfrac,
    dist.repdelay = dist.repdelay,
    dist.incub = dist.incub,
    dist.gi = dist.gi,
    prm.R = prm.R,
    silent = FALSE
  )

  test_output_tibble(
    res,
    col_name = c("date", "mean", "lwr", "upr", "use"),
    col_class = c("Date", "numeric", "numeric", "numeric", "logical")
  )
})
