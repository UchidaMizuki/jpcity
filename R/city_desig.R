#' Merge designated city wards
#'
#' @param city A `jpcity_city` object.
#' @param merge_tokyo Whether to merge Tokyo special wards?
#'
#' @return A `jpcity_city` object.
#'
#' @examples
#' city <- parse_city(c("01101", "13101"))
#' city_desig_merge(city)
#' city_desig_merge(city,
#'                  merge_tokyo = TRUE)
#'
#' @export
city_desig_merge <- function(city,
                             merge_tokyo = FALSE) {
  city_code <- city_code(city)

  city_desig_code <- city_desig_code |>
    dplyr::filter(merge_tokyo | city_desig_code != "13100") |>
    dplyr::select(!"interval")

  data <- city |>
    city_data() |>
    add_city_data() |>
    dplyr::left_join(city_desig_code,
                     by = "city_code") |>
    dplyr::mutate(city_code = .data$city_desig_code |>
                    dplyr::coalesce(.data$city_code),
                  interval = .data$interval_desig |>
                    dplyr::coalesce(.data$interval),
                  .keep = "unused")

  parse_city(city_code = data$city_code,
             when = intersect_interval(data$interval))
}

#' Split designated cities into wards
#'
#' @param city A `jpcity_city` object.
#' @param split_tokyo Whether to split into Tokyo special wards?
#'
#' @return A list of a `jpcity_city` object.
#'
#' @examples
#' city <- parse_city(c("01100", "13100"))
#' city_desig_split(city)
#' city_desig_split(city,
#'                  split_tokyo = FALSE)
#'
#' @export
city_desig_split <- function(city,
                             split_tokyo = TRUE) {
  city_code <- city_code(city)

  city_desig_code <- city_desig_code |>
    dplyr::select(!"interval_desig") |>
    dplyr::filter(split_tokyo | city_desig_code != "13100",
                  .data$city_code != .data$city_desig_code,
                  .data$city_desig_code %in% .env$city_code,
                  city_interval(city, intersect = TRUE) %within% .data$interval)
  city_desig_code <- vec_split(city_desig_code,
                               city_desig_code$city_desig_code) |>
    dplyr::mutate(val = .data$val |>
                    purrr::modify(\(val) {
                      parse_city(city_code = val$city_code,
                                 when = intersect_interval(val$interval))
                    }))
  data_frame(key = city_code,
             val = vec_chop(city)) |>
    dplyr::rows_update(city_desig_code,
                       by = "key") |>
    dplyr::pull("val")
}
