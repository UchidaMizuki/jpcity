pref <- function(data) {
  new_rcrd(data[c("pref_code", "pref_name")], class = "jpcity_pref")
}

#' Get pref data
#'
#' @param pref A `jpcity_pref` object.
#'
#' @return A data frame.
#'
#' @export
pref_data <- function(pref) {
  data_frame(
    pref_code = field(pref, "pref_code"),
    pref_name = field(pref, "pref_name")
  )
}

#' Test if the object is a jpcity_pref object
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `jpcity_pref` class.
#'
#' @export
is_pref <- function(x) {
  inherits_any(x, "jpcity_pref")
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
  if (is_city_legacy(city)) {
    city_code_legacy(city) |>
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
