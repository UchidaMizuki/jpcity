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
