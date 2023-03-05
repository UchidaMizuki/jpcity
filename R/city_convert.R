#' Convert to cities at different points in time
#'
#' @param city A `jpcity_city` object.
#' @param from A `character` (year, month, and day components) or date-time
#' object of the starting date.
#' @param to A `character` (year, month, and day components) or date-time
#' object of the ending date.
#'
#' @return A list of a `jpcity_city` object.
#'
#' @examples
#' city <- parse_city(c("01201", "01202"))
#' city_convert(city,
#'              from = "1970-04-01",
#'              to = "2020-01-01")
#'
#' @export
city_convert <- function(city, from, to) {
  from <- parse_ymd(from)
  to <- parse_ymd(to)

  if (!from %within% city_interval(city, intersect = TRUE)) {
    cli::cli_abort("{.arg from} must be within the interval of {.arg city}.")
  }

  data <- city |>
    city_data() |>
    add_city_data()

  if (from < to) {
    relatives <- descendants_city |>
      dplyr::filter(.data$node == .data$node_relatives | from <= lubridate::int_start(.data$interval),
                    to %within% .data$interval)
  } else {
    relatives <- ancestors_city |>
      dplyr::filter(.data$node == .data$node_relatives | from >= lubridate::int_end(.data$interval),
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
