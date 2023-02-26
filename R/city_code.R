check_city_code <- function(city_code) {
  # https://www.soumu.go.jp/main_content/000137948.pdf
  col_names_digit <- stringr::str_c("digit", 1:5,
                                    sep = "_")
  city_code <- data_frame(city_code = city_code) |>
    dplyr::mutate(check_digit_actual = city_code |>
                    stringr::str_extract("(?<=^\\d{5})\\d$"),
                  city_code = city_code |>
                    stringr::str_extract("^\\d{5}(?=\\d?$)"),
                  city_code |>
                    stringr::str_match("^(\\d)(\\d)(\\d)(\\d)(\\d)$") |>
                    tibble::as_tibble(.name_repair = \(x) c("city_code", col_names_digit)) |>
                    dplyr::mutate(dplyr::across({{ col_names_digit }},
                                                as.integer)) |>
                    dplyr::mutate(check_digit_expected = .data$digit_1 * 6L +
                                    .data$digit_2 * 5L +
                                    .data$digit_3 * 4L +
                                    .data$digit_4 * 3L +
                                    .data$digit_5 * 2L,
                                  check_digit_expected = dplyr::if_else(.data$check_digit_expected < 11L,
                                                                        11L - .data$check_digit_expected,
                                                                        (11L - (.data$check_digit_expected %% 11L)) %% 10L),
                                  .keep = "unused"))

  error_city_code <- city_code |>
    dplyr::filter(!is.na(.data$check_digit_actual) & .data$check_digit_actual != .data$check_digit_expected) |>
    dplyr::mutate(actual = stringr::str_c(.data$city_code, .data$check_digit_actual),
                  expected = stringr::str_c(.data$city_code, .data$check_digit_expected),
                  .keep = "unused")
  size_error_city_code <- vec_size(error_city_code)
  if (size_error_city_code > 0L) {
    message <- cli::format_inline("{vec_size(error_city_code)} check digit{?s} {?is/are} incorrect.")
    rlang::abort(message,
                 footer = utils::capture.output(error_city_code))
  }
  city_code$city_code
}

intersect_interval_city_code <- function(city_code) {
  interval_city_code <- interval_city_code |>
    dplyr::filter(.data$city_code %in% .env$city_code)

  if (vec_is_empty(interval_city_code)) {
    NULL
  } else {
    check_interval_city_code(city_code = interval_city_code$city_code,
                             interval = interval_city_code$interval)
  }
}

check_interval_city_code <- function(city_code, interval) {
  interval_city_code <- data_frame(city_code = city_code,
                                   interval = interval)

  out <- interval_city_code |>
    dplyr::pull("interval") |>
    purrr::discard(is.na) |>
    intersect_interval()

  if (is.na(out)) {
    interval_city_code <- interval_city_code |>
      dplyr::mutate(date_start = lubridate::date(lubridate::int_start(interval)),
                    date_end = lubridate::date(lubridate::int_end(interval)))
    oldest <- interval_city_code |>
      dplyr::slice_min(.data$date_end,
                       n = 1L)
    newest <- interval_city_code |>
      dplyr::slice_max(.data$date_start,
                       n = 1L)
    cli::cli_abort(c("Intervals of {.arg city_code} do not overlap.",
                     "i" = "Oldest: {.val {oldest$city_code}} ({oldest$date_start}--{.strong {oldest$date_end}})",
                     "i" = "Newest: {.val {newest$city_code}} ({.strong {newest$date_start}}--{newest$date_end})"))
  } else {
    out
  }
}
