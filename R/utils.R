tz_jst <- "Asia/Tokyo"
city_empty <- city(list(city_code = character(),
                        pref_name = character(),
                        city_desig_name = character(),
                        city_desig_name_kana = character(),
                        city_name = character(),
                        city_name_kana = character()),
                   interval = lubridate::interval(tzone = tz_jst))

assert_city <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city(city)) {
    cli::cli_abort("{.arg {name}} must inherit from {.cls city}.")
  }
}

assert_city_or_pref <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city(city) && !is_pref(city)) {
    cli::cli_abort("{.arg {name}} must inherit from {.cls jpcity_city} or {.cls jpcity_pref}.")
  }
}

intersect_interval <- function(interval) {
  size_interval <- vec_size(interval)
  if (size_interval == 0L) {
    vec_init(interval)
  } else if (size_interval == 1L) {
    interval
  } else if (all(is.na(interval))) {
    lubridate::NA_Date_ %--% lubridate::NA_Date_
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

parse_ymd <- function(when) {
  if (is.null(when)) {
    cli::cli_abort("{.arg when} must not be NULL.")
  } else if (is.character(when)) {
    when <- lubridate::ymd(when,
                           tz = tz_jst)
  }
  if (!when %within% interval_city) {
    cli::cli_abort("{.arg when} must be within {.val {interval_city}}")
  }
  when
}

extract_pref_name <- function(string) {
  string |>
    stringr::str_extract("[^[\\u90fd\\u9053\\u5e9c\\u770c]$]+")
}

quiet_as_integer <- function(x) {
  purrr::quietly(as.integer)(x) |>
    purrr::chuck("result")
}
