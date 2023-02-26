#' @export
parse_city <- function(city_code,
                       when = NULL) {
  if (is.null(when)) {
    interval <- intersect_interval_city_code(city_code)
  } else {
    if (lubridate::is.interval(when)) {
      interval <- when
    } else {
      if (is.character(when)) {
        when <- lubridate::ymd(when,
                               tz = tz_jst)
      }
      interval <- when %--% when
    }
  }

  nodes_city <- nodes_city |>
    dplyr::filter(.env$interval %within% .data$interval)
  nodes_city <- vec_slice(nodes_city,
                          i = vec_match(city_code, nodes_city$city_code))

  new_city(city_code = nodes_city$city_code,
           interval = interval)
}
