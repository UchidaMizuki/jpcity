#' Get city codes
#'
#' @param city A `jpcity_city` object.
#'
#' @return A `character` vector of city codes.
#'
#' @export
city_code_legacy <- function(city) {
  assert_city_legacy(city)
  field(city, "city_code")
}

#' Get city names
#'
#' @param city A `jpcity_city` object.
#' @param type Types of city names. By default, returns both designated city
#' names (`"city_desig"`) and city names (`"city"`).
#' @param sep Separator for city names.
#' @param kana Whether to use hiragana or not?
#'
#' @return A `character` vector of city names.
#'
#' @export
city_name_legacy <- function(
  city,
  type = c("city_desig", "city"),
  sep = "",
  kana = FALSE
) {
  assert_city_legacy(city)
  type <- arg_match(type, c("city_desig", "city"), multiple = TRUE)
  if (kana) {
    city_desig_name <- "city_desig_name_kana"
    city_name <- "city_name_kana"
  } else {
    city_desig_name <- "city_desig_name"
    city_name <- "city_name"
  }
  if ("city_desig" %in% type) {
    city_desig_name <- field(city, city_desig_name)
  }
  if ("city" %in% type) {
    city_name <- field(city, city_name)
  }

  if (setequal(type, c("city_desig", "city"))) {
    stringr::str_glue(
      "{city_desig_name}",
      "{city_name}",
      .na = "",
      .sep = sep
    ) |>
      as.character()
  } else if (setequal(type, "city_desig")) {
    city_desig_name
  } else if (setequal(type, "city")) {
    city_name
  }
}

#' Get city duration
#'
#' @param city A `jpcity_city` object.
#' @param intersect Whether to get the common part of the duration of cities.
#'
#' @return A `interval` vector of the duration of cities.
#'
#' @export
city_interval_legacy <- function(city, intersect = FALSE) {
  assert_city_legacy(city)
  if (intersect) {
    attr(city, "interval")
  } else {
    city_data_legacy(city) |>
      dplyr::left_join(
        graph_city_legacy$nodes_city,
        by = dplyr::join_by(
          "city_code",
          "pref_name",
          "city_desig_name",
          "city_desig_name_kana",
          "city_name",
          "city_name_kana"
        )
      ) |>
      dplyr::pull("interval")
  }
}

#' Convert city to prefecture
#'
#' @param city A `jpcity_city` object.
#'
#' @return A `jpcity_pref` object.
#'
#' @export
city_to_pref_legacy <- function(city) {
  assert_city_legacy(city)
  parse_pref_legacy(pref_code_legacy(city))
}
