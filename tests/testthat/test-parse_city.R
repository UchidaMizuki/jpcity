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
