library(lubridate)

# interval ----------------------------------------------------------------

# Last update date: November 27, 2021
interval_merger <- ymd("1970-04-01") %--% ymd("2021-11-27")

usethis::use_data(interval_merger,
                  internal = TRUE,
                  overwrite = TRUE)
