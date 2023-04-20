# set up inputs
start_date <- as.Date("2023-04-05")
n <- 4
agg.window <- 2 # must divide n
date.vec <- seq(start_date, start_date + (n-1), by = 1)
t.vec <- 1:n
cl.daily.test <- tibble::tibble(
  id = as.integer(1),
  date = date.vec,
  t = t.vec,
  value = n # avoid zeroes
)

cl.input.test <- (cl.daily.test
  %>% dplyr::mutate(group = rep(1:(n/agg.window), each = agg.window),
            rownum = 1:nrow(.))
  %>% dplyr::group_by(group)
  %>% dplyr::mutate(value = sum(value))
  %>% dplyr::ungroup()
  %>% dplyr::filter(rownum %% agg.window == 0)
  %>% dplyr::select(date, value, t)
)

test_that("get_use_dates() works when dates.only = TRUE", {
  dates.new <- get_use_dates(
    cl.daily.test,
    cl.input.test,
    prm.daily.check,
    dates.only = TRUE
  )

  # don't drop dates since results are exactly matching
  expect_equal(length(dates.new), nrow(cl.daily.test))

  expect_s3_class(dates.new, "Date")
})

test_that("get_use_dates() works when dates.only = FALSE", {
  df.new <- get_use_dates(
    cl.daily.test,
    cl.input.test,
    prm.daily.check,
    dates.only = FALSE
  )
  df.expected <- tibble::tibble(
    date = cl.daily.test$date,
    mean = n,
    lwr = n,
    upr = n,
    obs = rep(c(NA, n*agg.window), times = agg.window),
    date.report = rep(cl.input.test$date, each = agg.window),
    mean.agg = n*agg.window,
    lwr.agg = n*agg.window,
    upr.agg = n*agg.window,
    mean.agg.reldiff = rep(c(NA, 0), times = agg.window),
    lwr.agg.reldiff = rep(c(NA, 0), times = agg.window),
    upr.agg.reldiff = rep(c(NA, 0), times = agg.window),
    use = TRUE
  )
  names(df.expected$lwr) <- rep("2.5%", n)
  names(df.expected$upr) <- rep("97.5%", n)

  expect_identical(df.new, df.expected)
})
