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

check_city_interval <- function(city_code, interval,
                                message_when = FALSE) {
  out <- intersect_interval(interval)

  if (!all(is.na(interval)) && is.na(out)) {
    data <- data_frame(city_code = city_code,
                       interval = interval) |>
      dplyr::mutate(date_start = lubridate::date(lubridate::int_start(interval)),
                    date_end = lubridate::date(lubridate::int_end(interval)))
    oldest <- data |>
      dplyr::slice_min(.data$date_end,
                       n = 1L)
    newest <- data |>
      dplyr::slice_max(.data$date_start,
                       n = 1L)
    message <- c("Intervals of {.arg city_code} must overlap.",
                 "*" = "Oldest: {.val {oldest$city_code}} ({oldest$date_start}--{.strong {oldest$date_end}})",
                 "*" = "Newest: {.val {newest$city_code}} ({.strong {newest$date_start}}--{newest$date_end})")
    if (message_when) {
      message <- c(message,
                   "i" = "The {.arg when} argument must be given.")
    }
    cli::cli_abort(message)
  }
  out
}

add_city_data <- function(data) {
  data <- as.data.frame(data)
  vec_slice(nodes_city,
            vec_match(data, nodes_city[names2(data)]))
}

#' @export
is_city <- function(x) {
  inherits_any(x, "jpcity_city")
}

#' @export
vec_restore.jpcity_city <- function(x, to, ...) {
  out <- add_city_data(x)
  city(out,
       interval = check_city_interval(out$city_code, out$interval))
}

#' @export
c.jpcity_city <- function(...) {
  out <- list2(...) |>
    purrr::modify(city_data) |>
    purrr::list_c() |>
    add_city_data()
  city(out,
       interval = check_city_interval(out$city_code, out$interval))
}
