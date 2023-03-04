check_city_code <- function(city_code,
                            na = c("", "NA")) {
  loc <- stringr::str_detect(city_code, "^\\d{5,6}$") | city_code %in% na
  if (!all(loc, na.rm = TRUE)) {
    message <- cli::format_inline("{vec_size(loc)} parsing failure{?s}")
    footer <- data_frame(city_code = vec_slice(city_code, !loc))
    rlang::warn(message,
                footer = utils::capture.output(footer))
  }

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
