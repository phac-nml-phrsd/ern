# Get weekly public COVID reports (`cl.weekly`)

# list of pts to retrieve
pt.list <- c("bc", "ab", "sk", "mb", "on", "qc")

# date horizon of data
date.lim <- c(lubridate::today() - months(4),
              lubridate::today())

# lookup table for provice names
pt.lookup <- tibble::tribble(
  ~pt,  ~PT,                ~province_en,
  "ab", "AB",                   "Alberta",
  "bc", "BC",          "British Columbia",
  "ca", "CA",                    "Canada",
  "mb", "MB",                  "Manitoba",
  "nb", "NB",             "New Brunswick",
  "nl", "NL", "Newfoundland and Labrador",
  "ns", "NS",               "Nova Scotia",
  "nt", "NT",     "Northwest Territories",
  "nu", "NU",                   "Nunavut",
  "on", "ON",                   "Ontario",
  "pe", "PE",      "Prince Edward Island",
  "qc", "QC",                    "Quebec",
  "sk", "SK",              "Saskatchewan",
  "yt", "YT",                     "Yukon"
)

# raw data
cl.weekly <- (
   readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv",
                   show_col_types = FALSE)
   %>% dplyr::transmute(
     province_en = .data$prname,
     .data$date,
     count_cumm = .data$totalcases
   )
   %>% dplyr::left_join(pt.lookup, by = "province_en")
   %>% dplyr::filter(pt %in% pt.list)
   %>% dplyr::group_by(pt)
   %>% dplyr::mutate(count = dplyr::case_when(
     .data$date == min(.data$date) ~ .data$count_cumm,
     T ~ .data$count_cumm - dplyr::lag(count_cumm)
   ))
   %>% dplyr::select(pt, date, count)
   %>% tidyr::drop_na()
   %>% dplyr::filter(dplyr::between(.data$date, date.lim[1], date.lim[2]))
   %>% dplyr::arrange(.data$date)
   %>% dplyr::ungroup()
)

usethis::use_data(cl.weekly, overwrite = TRUE)
