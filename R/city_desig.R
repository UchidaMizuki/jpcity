#' @export
city_desig_merge <- function(city,
                             merge_tokyo = FALSE) {
  city_code <- city_code(city)

  city_desig_code <- city_desig_code |>
    dplyr::filter(merge_tokyo | city_desig_code != "13100")
  city_desig_code <- vec_slice(city_desig_code, vec_match(city_code, city_desig_code$city_code)) |>
    dplyr::mutate(city_desig_code = .data$city_desig_code |>
                    dplyr::coalesce(.env$city_code))

  when <- intersect_interval_city_code(city_desig_code$city_desig_code)
  city_desig_code |>
    dplyr::mutate(city = parse_city(.data$city_desig_code,
                                    when = when)) |>
    dplyr::pull("city")
}

#' @export
city_desig_split <- function(city,
                             split_tokyo = TRUE) {
  city_code <- city_code(city)

  city_desig_code <- city_desig_code |>
    dplyr::filter(split_tokyo | city_desig_code != "13100",
                  .data$city_code != .data$city_desig_code,
                  .data$city_desig_code %in% .env$city_code)

  when <- intersect_interval_city_code(city_desig_code$city_code)
  city_desig_code <- city_desig_code |>
    dplyr::mutate(city = parse_city(.data$city_code,
                                    when = when),
                  .keep = "unused")
  city_desig_code <- vec_split(city_desig_code$city, city_desig_code$city_desig_code)
  city_desig_code <- vec_slice(city_desig_code$val, vec_match(city_code, city_desig_code$key))
  purrr::modify2(city_desig_code, city,
                 \(city_desig_code, city) {
                   labelled::drop_unused_value_labels(city_desig_code %||% city)
                 })
}
