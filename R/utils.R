assert_city_or_pref <- function(city) {
  name <- as_name(enquo(city))

  if (!is_city_legacy(city) && !is_pref(city)) {
    cli::cli_abort(
      "{.arg {name}} must inherit from {.cls jpcity_city} or {.cls jpcity_pref}."
    )
  }
}

extract_pref_name <- function(string) {
  string |>
    stringr::str_extract("[^[\\u90fd\\u9053\\u5e9c\\u770c]$]+")
}

quiet_as_integer <- function(x) {
  purrr::quietly(as.integer)(x) |>
    purrr::chuck("result")
}
