test_that("parse_city", {
  expect_error(
    parse_city("01340")
  )
  expect_no_error(
    parse_city("01340",
               when = "1985-04-01")
  )

  city_code <- c("01340", "01108")
  out <- parse_city(city_code)
  expect_equal(city_code(out), city_code)

  out <- parse_city(city_code,
                    when = "1985-03-31")
  expect_equal(city_code(out), c(city_code[1], NA_character_))
})

test_that("vctrs::vec_detect_complete() works in jpcity_city (#5)", {
  city <- jpcity::parse_city("01100", "2020-10-01")
  expect_true(vctrs::vec_detect_complete(city))

  city <- jpcity::parse_city("01202", "2020-10-01")
  expect_true(vctrs::vec_detect_complete(city))
})
