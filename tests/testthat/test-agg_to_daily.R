# agg_to_daily() ------------------------------------------------------------

# set up inputs

test_that("agg_to_daily() returns a message when silent = FALSE", {
  expect_message(
    agg_to_daily(
      cl.data = cl.data,
      dist.gi = dist.gi,
      prm.daily = prm.daily,
      silent = FALSE
    )
  )
})

test_that("agg_to_daily() returns a data frame with the right format", {
  res <- agg_to_daily(
    cl.data = cl.data,
    dist.gi = dist.gi,
    prm.daily = prm.daily,
    silent = TRUE
  )
  
  # check output
  test_output_tibble(
    res[['df']],
    col_name = c("id", "date", "t", "value"),
    col_class = c("integer", "Date", "integer", "numeric")
  )
})


# fit_jags_aggreg() ---------------------------------------------------------
# uses inputs set up for agg_to_daily() above

test_that("fit_jags_aggreg() returns errors when initial incidence is invalid", {
  df <- cl.data
  
  df[1, "value"] <- -10
  expect_error(
    agg_to_daily(
      cl.data = df,
      dist.gi = dist.gi,
      prm.daily = prm.daily,
      silent = TRUE
    )
  )
  
  df[1, "value"] <- prm.daily$popsize*1e4
  expect_error(
    agg_to_daily(
      cl.data = df,
      dist.gi = dist.gi,
      prm.daily = prm.daily,
      silent = TRUE
    )
  )
})


# attach_t_agg() ----------------------------------------------------------

# generate df of dates for testing
df = tibble::tibble(
  date = seq.Date(as.Date("2022-10-04"), as.Date("2023-02-03"),
                  by = 1)
)

test_that("internal time index is correctly specified", {
  
  # evenly spaced dates
  window = 8 # date spacing
  
  check <- all((df
                |> dplyr::slice(which(dplyr::row_number() %% window == 1))
                |> attach_t_agg(silent = TRUE)
                |> dplyr::mutate(t.diff.check = as.numeric(date - dplyr::lag(date)))
                |> tidyr::drop_na()
                |> dplyr::mutate(check = t.diff.check == window)
  )$check
  )
  
  expect_true(check)
  
  # randomly spaced dates
  check <- all(
    suppressMessages(df
                     |> dplyr::slice_sample(n = 20)
                     |> dplyr::arrange(date)
                     |> dplyr::mutate(window = as.numeric(date - lag(date)))
                     |> attach_t_agg()
                     |> dplyr::mutate(
                       window.t = t - lag(t),
                       check = window == window.t
                     )
                     |> tidyr::drop_na()
    )$check
  )
  
  expect_true(check)
})

test_that("message is returned if first aggregation period not specified for aggregate clinical data", {
  expect_message(attach_t_agg(df))
})

test_that("message is returned when first aggregation period is specified explicitly", {
  expect_message(attach_t_agg(df, prm.daily = list(first.agg.period = 1)))
})



test_that("warning message is returned when deducing times from dates",{
  
  d =lubridate::ymd('2022-01-01')+ seq(0,40,by=7)
  df  = data.frame(date = d,
                  value = rpois(n=length(d), lambda = 100))
  prm.daily$burn = 2
  prm.daily$iter = 2
  
  expect_warning(
    agg_to_daily(
      cl.data = df,
      dist.gi = dist.gi,
      prm.daily = prm.daily,
      silent = TRUE
    ) )
})

