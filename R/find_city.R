#' @export
find_city <- function(patterns,
                      when = NULL) {
  out <- nodes_city |>
    dplyr::mutate(string_city = stringr::str_glue("{pref_name}{city_desig_name}{city_name}{city_desig_name_kana}{city_name_kana}",
                                                  .na = ""))
  for (pattern in patterns) {
    out <- out |>
      dplyr::filter(stringr::str_detect(.data$string_city, pattern))
  }
  if (!is.null(when)) {
    when <- parse_ymd(when)
    out <- out |>
      dplyr::filter(when %within% .data$interval)
  }
  out <- out |>
    dplyr::arrange(.data$city_code)
  city(out,
       interval = check_city_interval(out$city_code, out$interval,
                                      message_when = TRUE))
}
