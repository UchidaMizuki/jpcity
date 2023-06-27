#' Coerce an object to a jpcity_pref object
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param x A `integer` vector, a `jpcity_city` object or a `jpcity_pref` object.
#'
#' @return A `jpcity_pref` object.
#'
#' @export
as_pref <- function(x) {
  lifecycle::deprecate_warn("0.2.0", "as_pref()", "parse_pref()")

  if (!is_integerish(x)) {
    assert_city_or_pref(x)
    x <- pref_code(x)
  }
  data <- vec_slice(string_pref_name,
                    vec_match(x, string_pref_name$pref_code))
  pref(data)
}
