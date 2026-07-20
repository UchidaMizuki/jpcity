tz_jst_legacy <- "Asia/Tokyo"
city_empty_legacy <- city_legacy(
  list(
    city_code = character(),
    pref_name = character(),
    city_desig_name = character(),
    city_desig_name_kana = character(),
    city_name = character(),
    city_name_kana = character()
  ),
  interval = lubridate::interval(tzone = tz_jst_legacy)
)

assert_city_legacy <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city_legacy(city)) {
    cli::cli_abort("{.arg {name}} must inherit from {.cls city}.")
  }
}

intersect_interval_legacy <- function(interval, when = FALSE) {
  size_interval <- vec_size(interval)
  out <- if (size_interval == 0L) {
    vec_init(interval)
  } else if (size_interval == 1L) {
    interval
  } else if (all(is.na(interval))) {
    lubridate::NA_Date_ %--% lubridate::NA_Date_
  } else {
    start <- max(lubridate::int_start(interval), na.rm = TRUE)
    end <- min(lubridate::int_end(interval), na.rm = TRUE)

    if (is.finite(start) && start <= end) {
      start %--% end
    } else {
      lubridate::NA_Date_ %--% lubridate::NA_Date_
    }
  }
  if (when && is.infinite(lubridate::int_end(out))) {
    lubridate::int_end(out) <- lubridate::int_end(graph_city$interval_city)
  }
  out
}

parse_ymd_legacy <- function(when) {
  if (is.null(when)) {
    cli::cli_abort("{.arg when} must not be NULL.")
  } else if (is.character(when)) {
    when <- lubridate::ymd(when, tz = tz_jst_legacy)
  }
  if (!when %within% graph_city$interval_city) {
    cli::cli_abort(
      "{.arg when} must be within {.val {graph_city$interval_city}}"
    )
  }
  when
}
