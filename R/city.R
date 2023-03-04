city <- function(data, interval) {
  new_rcrd(data[c("city_code", "pref_name", "city_desig_name", "city_desig_name_kana", "city_name", "city_name_kana")],
           interval = interval,
           class = "jpcity_city")
}

city_data <- function(city) {
  data_frame(city_code = field(city, "city_code"),
             pref_name = field(city, "pref_name"),
             city_desig_name = field(city, "city_desig_name"),
             city_desig_name_kana = field(city, "city_desig_name_kana"),
             city_name = field(city, "city_name"),
             city_name_kana = field(city, "city_name_kana"))
}

#' @export
is_city <- function(x) {
  inherits_any(x, "jpcity_city")
}

#' @export
vec_restore.jpcity_city <- function(x, to, ...) {
  out <- add_city_interval(x)
  city(out,
       interval = check_city_interval(out$city_code, out$interval))
}

#' @export
c.jpcity_city <- function(...) {
  out <- list2(...) |>
    purrr::modify(city_data) |>
    purrr::list_c() |>
    add_city_interval()
  city(out,
       interval = check_city_interval(out$city_code, out$interval))
}
