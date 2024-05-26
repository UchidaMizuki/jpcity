test_that("city_convert-01", {
  out <- parse_city("01104") |>
    city_convert("1989-11-04", "2020-01-01")
  expect_equal(city_code(out[[1]]), c("01104", "01108"))

  expect_error(
    parse_city(c("01104", "01108")) |>
      city_convert("1989-11-05", "2020-01-01")
  )

  out <- parse_city(c("01104", "01108")) |>
    city_convert("1989-11-06", "2020-01-01")
  expect_true(setequal(city_code(out[[1]]), c("01104", "01108")))
  expect_equal(city_code(out[[2]]), "01108")
})

test_that("city_convert-02", {
  city <- parse_city("04403")
  from <- "1970-04-01"

  out <- city_convert(city, from, "1971-10-31")[[1L]]
  expect_equal(city_code(out), "04403")

  out <- city_convert(city, from, "1971-11-01")[[1L]]
  expect_equal(city_code(out), "04210")

  out <- city_convert(city, from, "1988-03-01")[[1L]]
  expect_equal(city_code(out), "04201")

  out <- city_convert(city, from, "1989-04-01")[[1L]]
  expect_equal(city_code(out), "04100")

  city <- parse_city("04201")
  from <- "1989-03-31"

  out <- city_convert(city, from, "1988-03-01")[[1L]]
  expect_equal(city_code(out), "04201")

  out <- city_convert(city, from, "1988-02-28")[[1L]]
  expect_true(setequal(city_code(out), c("04201", "04382", "04210")))

  out <- city_convert(city, from, "1987-10-31")[[1L]]
  expect_true(setequal(city_code(out), c("04201", "04382", "04210", "04405")))

  out <- city_convert(city, from, "1971-10-31")[[1L]]
  expect_true(setequal(city_code(out), c("04201", "04382", "04403", "04405")))

  # Kushiro city
  city <- parse_city("01206")

  out <- city_convert(city, "2020-10-01", "2020-10-01")
  out <- out[[1L]]
  expect_length(out[[1L]], 1L)
  expect_equal(city_code(out[1L]), "01206")

  out <- city_convert(city, "2020-10-01", "2005-10-11")
  out <- out[[1L]]
  expect_length(out[[1L]], 1L)
  expect_equal(city_code(out[[1L]]), "01206")

  out <- city_convert(city, "2020-10-01", lubridate::int_end(graph_city$interval_city) - lubridate::days(1L))
  out <- out[[1L]]
  expect_length(out[[1L]], 1L)
  expect_equal(city_code(out[[1L]]), "01206")

  # Anamizu Town and Monzen Town
  city <- parse_city(c("17421", "17422"))
  out <- city_convert(city, "2005-02-28", "2005-03-01")
  expect_length(out[[1L]], 1L)
  expect_length(out[[2L]], 1L)

  # Uruma City
  city <- parse_city("47213",
                     when = "2020-01-01")
  cities <- city_convert(city, "2020-01-01", "1994-01-01")[[1L]]
  expect_true(setequal(city_code(cities), c("47202", "47203", "47322", "47323")))

  cities <- city_convert(city, "2020-01-01", "1972-05-15")[[1L]]
  expect_true(setequal(city_code(cities), c("47202", "47203", "47322", "47323")))

  cities <- city_convert(city, "2020-01-01", "1972-05-14")[[1L]]
  expect_length(cities, 0L)

  # Kamikuishiki Village
  city <- parse_city("19341",
                     when = "2000-01-01")
  cities <- city_convert(city, "2000-01-01", "2006-03-01")
  expect_equal(city_code(cities[[1]]), c("19201", "19430"))

  # Hamamatsu City
  city <- parse_city(c("22131", "22132", "22133", "22134"),
                     when = "2020-01-01")
  cities <- city_convert(city, "2020-01-01", "2024-01-01") |>
    vctrs::list_unchop() |>
    vctrs::vec_unique()
  expect_equal(city_code(cities), "22138")

  city <- parse_city("22135",
                     when = "2020-01-01")
  cities <- city_convert(city, "2020-01-01", "2024-01-01")
  expect_equal(city_code(cities[[1]]), c("22138", "22139"))
})
