test_that("parse_city_legacy", {
  expect_error(
    parse_city_legacy("01340")
  )
  expect_no_error(
    parse_city_legacy("01340", when = "1985-04-01")
  )

  city_code_legacy <- c("01340", "01108")
  out <- parse_city_legacy(city_code_legacy)
  expect_equal(city_code_legacy(out), city_code_legacy)

  out <- parse_city_legacy(city_code_legacy, when = "1985-03-31")
  expect_equal(city_code_legacy(out), c(city_code_legacy[1], NA_character_))
})

test_that("vctrs::vec_detect_complete() works in jpcity_city (#5)", {
  city <- jpcity::parse_city_legacy("01100", "2020-10-01")
  expect_true(vctrs::vec_detect_complete(city))

  city <- jpcity::parse_city_legacy("01202", "2020-10-01")
  expect_true(vctrs::vec_detect_complete(city))
})

test_that("jpcity::city_desig_merge_legacy() works (#7)", {
  city <- jpcity::parse_city_legacy("01101") |>
    jpcity::city_desig_merge_legacy()

  expect_equal(jpcity::city_code_legacy(city), "01100")

  city <- jpcity::parse_city_legacy("13101") |>
    jpcity::city_desig_merge_legacy()

  expect_equal(jpcity::city_code_legacy(city), "13101")

  city <- jpcity::parse_city_legacy("13101") |>
    jpcity::city_desig_merge_legacy(merge_tokyo = TRUE)

  expect_equal(jpcity::city_code_legacy(city), "13100")
})
