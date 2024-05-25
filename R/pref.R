pref <- function(data) {
  new_rcrd(data[c("pref_code", "pref_name")],
           class = "jpcity_pref")
}

#' Get pref data
#'
#' @param pref A `jpcity_pref` object.
#'
#' @return A data frame.
#'
#' @export
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
