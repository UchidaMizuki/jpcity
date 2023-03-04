#' @export
city_convert <- function(city, from, to) {
  if (is.character(from)) {
    from <- lubridate::ymd(from,
                           tz = tz_jst)
  }
  if (is.character(to)) {
    to <- lubridate::ymd(to,
                           tz = tz_jst)
  }

  if (!from %within% city_interval(city, intersect = TRUE)) {
    cli::cli_abort("{.arg from} must be within the interval of {.arg city}.")
  }

  data <- city |>
    city_data() |>
    add_city_data()

  if (from < to) {
    relatives <- descendants_city |>
      dplyr::filter(node == node_relatives | from <= lubridate::int_start(interval),
                    to %within% .data$interval)
  } else {
    relatives <- ancestors_city |>
      dplyr::filter(node == node_relatives | from >= lubridate::int_end(interval),
                    to %within% .data$interval)
  }

  relatives <- relatives |>
    dplyr::select(!"interval") |>
    dplyr::filter(.data$node %in% data$node) |>
    dplyr::left_join(nodes_city,
                     by = dplyr::join_by("node_relatives" == "node")) |>
    dplyr::select(!"node_relatives")
  relatives <- vec_split(relatives, relatives$node) |>
    dplyr::mutate(val = .data$val |>
                    purrr::modify(\(val) {
                      city(val,
                           interval = intersect_interval(val$interval))
                    }))
  vec_slice(relatives,
            vec_match(data$node, relatives$key)) |>
    dplyr::pull("val")
}
