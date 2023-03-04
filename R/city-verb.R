#' @export
city_code <- function(city) {
  assert_city(city)
  field(city, "city_code")
}

#' @export
pref_code <- function(city) {
  assert_city(city)
  city_code(city) |>
    stringr::str_extract("^\\d{2}") |>
    as.integer()
}

#' @export
pref_name <- function(city) {
  assert_city(city)
  field(city, "pref_name")
}

#' @export
city_name <- function(city,
                      type = c("city_desig", "city"),
                      sep = "") {
  assert_city(city)
  type <- arg_match(type, c("city_desig", "city"),
                    multiple = TRUE)

  if ("city_desig" %in% type) {
    city_desig_name <- field(city, "city_desig_name")
  }
  if ("city" %in% type) {
    city_name <- field(city, "city_name")
  }

  if (setequal(type, c("city_desig", "city"))) {
    stringr::str_glue("{city_desig_name}", "{city_name}",
                      .na = "",
                      .sep = sep)
  } else if (setequal(type, "city_desig")) {
    city_desig_name
  } else if (setequal(type, "city")) {
    city_name
  }
}

#' @export
city_name_kana <- function(city,
                           type = c("city_desig", "city"),
                           sep = "") {
  assert_city(city)
  type <- arg_match(type, c("city_desig", "city"),
                    multiple = TRUE)

  if ("city_desig" %in% type) {
    city_desig_name_kana <- field(city, "city_desig_name_kana")
  }
  if ("city" %in% type) {
    city_name_kana <- field(city, "city_name_kana")
  }

  if (setequal(type, c("city_desig", "city"))) {
    stringr::str_glue("{city_desig_name_kana}", "{city_name_kana}",
                      .na = "",
                      .sep = sep)
  } else if (setequal(type, "city_desig")) {
    city_desig_name_kana
  } else if (setequal(type, "city")) {
    city_name_kana
  }
}

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
