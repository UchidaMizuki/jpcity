#' @export
parse_city <- function(city_code,
                       when = NULL) {
  if (is.null(when)) {
    interval <- interval_city_code |>
      dplyr::filter(.data$city_code %in% .env$city_code)
    interval <- check_city_interval(city_code = interval$city_code,
                                    interval = interval$interval)
  } else {
    if (is.character(when)) {
      when <- lubridate::ymd(interval,
                             tz = tz_jst)
      interval <- when %--% when
    } else if (lubridate::is.interval(when)) {
      interval <- when
    } else {
      interval <- lubridate::interval(interval, interval)
    }
  }

  data <- nodes_city |>
    dplyr::filter(.data$city_code %in% .env$city_code,
                  .env$interval %within% .data$interval)

  interval <- check_city_interval(data$city_code, data$interval)

  if (is.null(when)) {
    cli::cli_inform(c("Guessing the interval to be {.val {interval}}.",
                      "i" = "You can override using the {.arg when} argument."))
  }
  city(data = data,
       interval = interval)
}
