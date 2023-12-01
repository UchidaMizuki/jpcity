#' Get city codes
#'
#' @param city A `jpcity_city` object.
#'
#' @return A `character` vector of city codes.
#'
#' @export
city_code <- function(city) {
  assert_city(city)
  field(city, "city_code")
}

#' Get prefecture codes
#'
#' @param city A `jpcity_city` or `jpcity_pref` object.
#'
#' @return A integer vector of prefecture codes.
#'
#' @export
pref_code <- function(city) {
  assert_city_or_pref(city)
  if (is_city(city)) {
    city_code(city) |>
      stringr::str_extract("^\\d{2}") |>
      as.integer()
  } else if (is_pref(city)) {
    field(city, "pref_code")
  }
}

#' Get prefecture names
#'
#' @param city A `jpcity_city` object.
#'
#' @return A `character` vector of prefecture names.
#'
#' @export
pref_name <- function(city) {
  assert_city_or_pref(city)
  field(city, "pref_name")
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
city_name <- function(city,
                      type = c("city_desig", "city"),
                      sep = "",
                      kana = FALSE) {
  assert_city(city)
  type <- arg_match(type, c("city_desig", "city"),
                    multiple = TRUE)
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
    stringr::str_glue("{city_desig_name}", "{city_name}",
                      .na = "",
                      .sep = sep) |>
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
city_interval <- function(city,
                          intersect = FALSE) {
  assert_city(city)
  if (intersect) {
    attr(city, "interval")
  } else {
    city_data(city) |>
      dplyr::left_join(nodes_city,
                       by = dplyr::join_by("city_code", "pref_name",
                                           "city_desig_name", "city_desig_name_kana",
                                           "city_name", "city_name_kana")) |>
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
city_to_pref <- function(city) {
  assert_city(city)
  parse_pref(pref_code(city))
}
