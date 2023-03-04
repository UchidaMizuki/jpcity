#' @export
format.jpcity_city <- function(x, ...) {
  field(x, "city_code")
}

#' @export
obj_print_header.jpcity_city <- function(x, ...) {
  interval <- attr(x, "interval")
  interval <- stringr::str_c(lubridate::int_start(interval), lubridate::int_end(interval),
                             sep = "--")
  cli::cat_line("<", vec_ptype_full(x), "[", vec_size(x), "]> Interval: ", interval)
  invisible(x)
}

#' @export
obj_print_footer.jpcity_city <- function(x, ...) {
  cli::cat_line(c("", "Cities:"))
  city_data(x) |>
    vec_unique() |>
    print()
  invisible(x)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.jpcity_city <- function(x, ...) {
  city_code <- field(x, "city_code")
  pref_name <- field(x, "pref_name")
  city_desig_name <- field(x, "city_desig_name")
  city_name <- field(x, "city_name")

  city_name <- stringr::str_glue("[{pref_name}{city_desig_name}{city_name}]",
                                 .na = "")
  formatted <- stringr::str_c(city_code, pillar::style_subtle(city_name),
                              sep = " ")
  pillar::new_pillar_shaft_simple(formatted)
}

#' @export
vec_ptype_full.jpcity_city <- function(x, ...) {
  "city"
}

#' @export
vec_ptype_abbr.jpcity_city <- function(x, ...) {
  "city"
}
