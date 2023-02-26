tz_jst <- "Asia/Tokyo"

assert_city <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city(city)) {
    cli::cli_abort("{.arg {name}} must inherit from {.cls city}.")
  }
}

intersect_interval <- function(interval) {
  if (vec_is_empty(interval)) {
    lubridate::interval(tzone = tz_jst)
  } else {
    start <- max(lubridate::int_start(interval),
                 na.rm = TRUE)
    end <- min(lubridate::int_end(interval),
               na.rm = TRUE)
    if (start <= end) {
      start %--% end
    } else {
      lubridate::NA_Date_ %--% lubridate::NA_Date_
    }
  }
}
