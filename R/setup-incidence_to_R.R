# dummy incidence df for testing

incidence = data.frame(
  date = seq(lubridate::today(), lubridate::today() + 10, by = "days"),
  t = 1:11,
  I = seq(from=1, to=100, length.out = 11)
)
