test_that("parse_pref_legacy", {
  expect_equal(
    pref_code_legacy(parse_pref_legacy(c("北海道", "沖縄県"))),
    c(1, 47)
  )
  expect_equal(
    pref_code_legacy(parse_pref_legacy(c("東京", "大阪"))),
    c(13, 27)
  )
  expect_equal(
    pref_code_legacy(parse_pref_legacy(c("1", "02", "47"))),
    c(1, 2, 47)
  )
  expect_equal(
    pref_code_legacy(parse_pref_legacy(c("01000", "02000", "47000"))),
    vec_rep(NA_integer_, 3)
  )
  expect_equal(
    pref_code_legacy(
      parse_pref_legacy(c("01000", "02000", "47000"), strict = FALSE)
    ),
    c(1, 2, 47)
  )
})
