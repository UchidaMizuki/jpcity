tz_jst <- "Asia/Tokyo"

assert_city <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city(city)) {
    cli::cli_abort("{.arg {name}} must inherit from {.cls city}.")
  }
}

intersect_interval <- function(interval) {
  size_interval <- vec_size(interval)
  if (size_interval == 0L) {
    vec_init(interval)
  } else if (size_interval == 1L) {
    interval
  } else {
    start <- max(lubridate::int_start(interval),
                 na.rm = TRUE)
    end <- min(lubridate::int_end(interval),
               na.rm = TRUE)

    if (is.finite(start) && is.finite(end) && start <= end) {
      start %--% end
    } else {
      lubridate::NA_Date_ %--% lubridate::NA_Date_
    }
  }
}
