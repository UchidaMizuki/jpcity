pref_legacy <- function(data) {
  new_rcrd(data[c("pref_code", "pref_name")], class = "jpcity_pref")
}

#' Get pref data
#'
#' @param pref A `jpcity_pref` object.
#'
#' @return A data frame.
#'
#' @export
pref_data_legacy <- function(pref) {
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
is_pref_legacy <- function(x) {
  inherits_any(x, "jpcity_pref")
}

#' Get prefecture codes
#'
#' @param city A `jpcity_city` or `jpcity_pref` object.
#'
#' @return A integer vector of prefecture codes.
#'
#' @export
pref_code_legacy <- function(city) {
  assert_city_or_pref_legacy(city)
  if (is_city_legacy(city)) {
    city_code_legacy(city) |>
      stringr::str_extract("^\\d{2}") |>
      as.integer()
  } else if (is_pref_legacy(city)) {
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
pref_name_legacy <- function(city) {
  assert_city_or_pref_legacy(city)
  field(city, "pref_name")
}
