#' Parse city codes
#'
#' @param x A `character` vector of city codes.
#' @param when A `character` (year, month, and day components) or date-time
#' object.
#' @param na A `character` vector to be treated as missing values.
#' @param city_code (Deprecated) A `character` vector of city codes.
#'
#' @return A `jpcity_city` object.
#'
#' @export
parse_city <- function(x,
                       when = NULL,
                       na = c("", "NA"),
                       city_code = deprecated()) {
  if (lifecycle::is_present(city_code)) {
    lifecycle::deprecate_warn("0.2.0", "parse_city(city_code = )", "parse_city(x = )")
    x <- city_code
  }

  city_code <- check_city_code(x,
                               na = na)

  if (is.null(when)) {
    interval <- interval_city_code |>
      dplyr::filter(.data$city_code %in% .env$city_code)
    interval <- check_city_interval(city_code = interval$city_code,
                                    interval = interval$interval,
                                    message_when = TRUE)
  } else {
    when <- parse_ymd(when)

    if (lubridate::is.interval(when)) {
      interval <- when
    } else {
      interval <- when %--% when
    }
  }

  data <- nodes_city |>
    dplyr::filter(!is.na(lubridate::intersect(.data$interval, .env$interval)),
                  .data$city_code %in% .env$city_code)
  interval <- check_city_interval(data$city_code, data$interval)
  if (is.null(when)) {
    cli::cli_inform(c("Guessing the interval to be {.val {interval}}.",
                      "i" = "You can override using {.arg when} argument."))
  }

  data <- vec_slice(data,
                    vec_match(city_code, data$city_code))
  city(data = data,
       interval = interval)
}

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
