pref <- function(data) {
  new_rcrd(data[c("pref_code", "pref_name")],
           class = "jpcity_pref")
}

pref_data <- function(pref) {
  data_frame(pref_code = field(pref, "pref_code"),
             pref_name = field(pref, "pref_name"))
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

#' Coerce an object to a jpcity_pref object
#'
#' @param x A `integer` vector, a `jpcity_city` object or a `jpcity_pref` object.
#'
#' @return A `jpcity_pref` object.
#'
#' @export
as_pref <- function(x) {
  if (!is_integerish(x)) {
    assert_city_or_pref(x)
    x <- pref_code(x)
  }
  data <- vec_slice(string_pref_name,
                    vec_match(x, string_pref_name$pref_code))
  pref(data)
}

#' Parse prefecture names
#'
#' @param pref_name A `character` vector of prefecture names.
#'
#' @return A `jpcity_pref` object.
#'
#' @export
parse_pref <- function(pref_name) {
  pref_code <- dplyr::case_when(stringr::str_detect(pref_name, "^\\d+$") | is_integerish(pref_name) ~ purrr::quietly(as.integer)(pref_name)$result,
                                is.character(pref_name) ~ vec_slice(string_pref_name$pref_code,
                                                                    vec_match(extract_pref_name(pref_name), string_pref_name$string_pref_name)))
  as_pref(pref_code)
}
