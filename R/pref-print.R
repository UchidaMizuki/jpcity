#' @export
format.jpcity_pref <- function(x, ...) {
  field(x, "pref_code")
}

#' @export
obj_print_footer.jpcity_pref <- function(x, ...) {
  cli::cat_line(c("", "Prefectures:"))
  pref_data(x) |>
    vec_unique() |>
    print()
  invisible(x)
}

#' @export
pillar_shaft.jpcity_pref <- function(x, ...) {
  pref_code <- field(x, "pref_code")
  pref_name <- field(x, "pref_name")

  pref_name <- stringr::str_glue("[{pref_name}]",
                                 .na = "")
  formatted <- stringr::str_c(stringr::str_pad(pref_code, 2L), pillar::style_subtle(pref_name),
                              sep = " ")
  pillar::new_pillar_shaft_simple(formatted)
}

#' @export
vec_ptype_full.jpcity_pref <- function(x, ...) {
  "pref"
}

#' @export
vec_ptype_abbr.jpcity_pref <- function(x, ...) {
  "pref"
}
