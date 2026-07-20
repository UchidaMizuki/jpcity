#' Merge designated city wards
#'
#' @param city A `jpcity_city` object.
#' @param merge_tokyo Whether to merge Tokyo special wards?
#'
#' @return A `jpcity_city` object.
#'
#' @examples
#' city <- parse_city_legacy(c("01101", "13101"))
#' city_desig_merge_legacy(city)
#' city_desig_merge_legacy(city,
#'                  merge_tokyo = TRUE)
#'
#' @export
city_desig_merge_legacy <- function(city, merge_tokyo = FALSE) {
  city_code <- city_code_legacy(city)

  city_desig_code <- city_desig_code_legacy |>
    dplyr::filter(merge_tokyo | city_desig_code != "13100") |>
    dplyr::select(!"interval")

  data <- city |>
    city_data_legacy() |>
    add_city_data_legacy() |>
    dplyr::left_join(city_desig_code, by = "city_code") |>
    dplyr::mutate(
      city_code = .data$city_desig_code |>
        dplyr::coalesce(.data$city_code),
      interval = .data$interval_desig |>
        dplyr::coalesce(.data$interval),
      .keep = "unused"
    )

  parse_city_legacy(
    data$city_code,
    when = intersect_interval_legacy(data$interval, when = TRUE)
  )
}

#' Split designated cities into wards
#'
#' @param city A `jpcity_city` object.
#' @param split_tokyo Whether to split into Tokyo special wards?
#'
#' @return A list of a `jpcity_city` object.
#'
#' @examples
#' city <- parse_city_legacy(c("01100", "13100"))
#' city_desig_split_legacy(city)
#' city_desig_split_legacy(city,
#'                  split_tokyo = FALSE)
#'
#' @export
city_desig_split_legacy <- function(city, split_tokyo = TRUE) {
  city_code <- city_code_legacy(city)

  city_desig_code <- city_desig_code_legacy |>
    dplyr::select(!"interval_desig") |>
    dplyr::filter(
      split_tokyo | city_desig_code != "13100",
      .data$city_code != .data$city_desig_code,
      .data$city_desig_code %in% .env$city_code,
      city_interval_legacy(city, intersect = TRUE) %within% .data$interval
    )
  city_desig_code <- vec_split(
    city_desig_code,
    city_desig_code$city_desig_code
  ) |>
    dplyr::mutate(
      val = .data$val |>
        purrr::map(\(val) {
          parse_city_legacy(
            val$city_code,
            when = intersect_interval_legacy(val$interval, when = TRUE)
          )
        })
    )
  data_frame(key = city_code, val = vec_chop(city)) |>
    dplyr::rows_update(city_desig_code, by = "key") |>
    dplyr::pull("val")
}

#' Check if a city is a designated city or a ward of a designated city
#'
#' @param x A `jpcity_city` object.
#' @param type A character vector of city types, `"city"` or `"ward"`.
#' By default, `"city"`.
#'
#' @return A logical vector.
#'
#' @export
is_city_desig_legacy <- function(x, type = "city") {
  type <- arg_match(type, c("city", "ward"), multiple = TRUE)

  city_code <- NULL
  if ("city" %in% type) {
    city_code <- c(city_code, city_desig_code_legacy$city_desig_code)
  }
  if ("ward" %in% type) {
    city_code <- c(
      city_code,
      setdiff(
        city_desig_code_legacy$city_code,
        city_desig_code_legacy$city_desig_code
      )
    )
  }

  city_code_legacy(x) %in% city_code
}
