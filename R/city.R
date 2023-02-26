city_data <- function(city_code,
                      interval = NULL) {
  if (is_city(city_code)) {
    interval <- city_interval(city_code)
    city_code <- city_code(city_code)
  }

  nodes_city <- nodes_city |>
    dplyr::filter(.data$city_code %in% .env$city_code,
                  .env$interval %within% .data$interval)
  if (vec_duplicate_any(nodes_city$city_code)) {
    cli::cli_abort("{.arg city_code} must not be duplicated in {.arg interval}.")
  }

  vec_slice(nodes_city,
            i = vec_match(city_code, nodes_city$city_code))
}

new_city <- function(city_code, interval) {
  labels <- city_data(city_code = city_code,
                      interval = interval) |>
    dplyr::distinct(.data$city_code, .data$pref_name, .data$city_desig_name, .data$city_name)
  labels <- labels$city_code |>
    set_names(labels |>
                stringr::str_glue_data("{pref_name}{city_desig_name}{city_name}",
                                       .na = ""))

  out <- labelled::labelled(city_code, labels,
                            label = as.character(interval))
  structure(out,
            interval = interval,
            class = c("jpcity_city", class(out)))
}

#' @export
vec_restore.jpcity_city <- function(x, to, ...) {
  new_city(x,
           interval = intersect_interval_city_code(x))
}

#' @export
is_city <- function(x) {
  inherits_any(x, "jpcity_city")
}

#' @export
`==.jpcity_city` <- function(e1, e2) {
  interval <- lubridate::intersect(city_interval(e1), city_interval(e2))
  if (is.na(interval)) {
    cli::cli_abort("Intervals of {.arg e1} and {.arg e2} must overlap.")
  }
  vec_equal(city_code(e1), city_code(e2))
}

#' @export
c.jpcity_city <- function(...) {
  city_code <- list2(...) |>
    purrr::modify(city_code) |>
    purrr::list_c()
  new_city(city_code,
           interval = intersect_interval_city_code(city_code))
}

#' @export
city_code <- function(city) {
  assert_city(city)
  as.character(city)
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
  city_data <- city_data(city)
  city_data$pref_name
}

#' @export
city_name <- function(city,
                      type = c("city_desig", "city"),
                      sep = "") {
  assert_city(city)
  type <- arg_match(type, c("city_desig", "city"),
                    multiple = TRUE)

  city_data <- city_data(city)
  if ("city_desig" %in% type) {
    city_desig_name <- city_data$city_desig_name
    vec_slice(city_desig_name, is.na(city_desig_name)) <- ""
  }
  if ("city" %in% type) {
    city_name <- city_data$city_name
    vec_slice(city_name, is.na(city_name)) <- ""
  }

  if (setequal(type, c("city_desig", "city"))) {
    stringr::str_c(city_desig_name, city_name,
                   sep = sep)
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

  city_data <- city_data(city)
  if ("city_desig" %in% type) {
    city_desig_name_kana <- city_data$city_desig_name_kana
    vec_slice(city_desig_name_kana, is.na(city_desig_name_kana)) <- ""
  }
  if ("city" %in% type) {
    city_name_kana <- city_data$city_name_kana
    vec_slice(city_name_kana, is.na(city_name_kana)) <- ""
  }

  if (setequal(type, c("city_desig", "city"))) {
    stringr::str_c(city_desig_name_kana, city_name_kana,
                   sep = sep)
  } else if (setequal(type, "city_desig")) {
    city_desig_name_kana
  } else if (setequal(type, "city")) {
    city_name_kana
  }
}

#' @export
city_interval <- function(city) {
  assert_city(city)
  attr(city, "interval")
}

#' @export
vec_ptype_full.jpcity_city <- function(x, ...) {
  "city"
}

#' @export
vec_ptype_abbr.jpcity_city <- function(x, ...) {
  "city"
}
