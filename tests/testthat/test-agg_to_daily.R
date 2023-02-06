test_that("internal time index is correctly specified", {
  library(tibble)
  library(dplyr)
  library(tidyr)

  # generate df to sample
  df = tibble(
    date = seq.Date(as.Date("2022-10-04"), as.Date("2023-02-03"),
                    by = 1)
  )

  # evenly spaced dates
  window = 8 # date spacing

  check <- all(
  suppressMessages(df
      %>% slice(which(row_number() %% window == 1))
      %>% attach_t_agg()
      %>% mutate(t.diff.check = as.numeric(date - lag(date)))
      %>% drop_na()
      %>% mutate(check = t.diff.check == window)
    )$check
  )

  expect_true(check)

  # randomly spaced dates
  check <- all(
  suppressMessages(df
      %>% slice_sample(n = 20)
      %>% arrange(date)
      %>% mutate(window = as.numeric(date - lag(date)))
      %>% attach_t_agg()
      %>% mutate(
        window.t = t - lag(t),
        check = window == window.t
      )
      %>% drop_na()
    )$check
  )

  expect_true(check)
})
