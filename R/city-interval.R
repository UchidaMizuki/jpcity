check_city_interval <- function(city_code, interval) {
  out <- intersect_interval(interval)

  if (is.na(out)) {
    data <- data_frame(city_code = city_code,
                       interval = interval) |>
      dplyr::mutate(date_start = lubridate::date(lubridate::int_start(interval)),
                    date_end = lubridate::date(lubridate::int_end(interval)))
    oldest <- data |>
      dplyr::slice_min(.data$date_end,
                       n = 1L)
    newest <- data |>
      dplyr::slice_max(.data$date_start,
                       n = 1L)
    cli::cli_abort(c("Intervals of {.arg city_code} do not overlap.",
                     "i" = "Oldest: {.val {oldest$city_code}} ({oldest$date_start}--{.strong {oldest$date_end}})",
                     "i" = "Newest: {.val {newest$city_code}} ({.strong {newest$date_start}}--{newest$date_end})"))
  }
  out
}

add_city_interval <- function(data) {
  data <- as.data.frame(data)
  vec_slice(nodes_city,
            vec_match(data, nodes_city[names2(data)]))
}
