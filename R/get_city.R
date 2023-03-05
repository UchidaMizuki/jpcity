#' Get cities at a specific point in time
#'
#' @param when A `character` (year, month, and day components) or date-time
#' object.
#'
#' @return A `jpcity_city` object.
#'
#' @examples
#' get_city("2020-01-01")
#'
#' @export
get_city <- function(when) {
  when <- parse_ymd(when)

  out <- nodes_city |>
    dplyr::filter(!.data$city_code %in% city_desig_code$city_desig_code,
                  when %within% .data$interval) |>
    dplyr::arrange(.data$city_code)
  city(out,
       interval = check_city_interval(out$city_code, out$interval,
                                      message_when = TRUE))
}
