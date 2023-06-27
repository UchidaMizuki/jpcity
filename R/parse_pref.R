#' Parse prefecture codes or names
#'
#' @param x A `character` vector of prefecture codes or names.
#' @param strict A scalar logical. Whether to require the code to have 1 or 2
#' digits. By default, `TRUE`.
#' @param pref_name (Depricated) A `character` vector of prefecture codes or names.
#'
#' @return A `jpcity_pref` object.
#'
#' @export
parse_pref <- function(x,
                       strict = TRUE,
                       pref_name = deprecated()) {
  if (lifecycle::is_present(pref_name)) {
    lifecycle::deprecate_warn("0.2.0", "parse_pref(pref_name = )", "parse_pref(x = )")
    x <- pref_name
  }

  if (!is_scalar_logical(strict)) {
    cli::cli_abort("{.arg strict} must be a scalar logical.")
  }

  pref_code <- dplyr::case_when(stringr::str_detect(x, "^\\d{1,2}$") ~ quiet_as_integer(x),
                                !strict & stringr::str_detect(x, "^\\d+$") ~ x |>
                                  stringr::str_sub(1, 2) |>
                                  quiet_as_integer(),
                                TRUE ~ vec_slice(string_pref_name$pref_code,
                                                 vec_match(extract_pref_name(x), string_pref_name$string_pref_name)))
  data <- vec_slice(string_pref_name,
                    vec_match(pref_code, string_pref_name$pref_code))
  pref(data)
}
