test_that("parse_pref", {
  expect_equal(pref_code(parse_pref(c("北海道", "沖縄県"))), c(1, 47))
  expect_equal(pref_code(parse_pref(c("東京", "大阪"))), c(13, 27))
  expect_equal(pref_code(parse_pref(c("1", "02", "47"))), c(1, 2, 47))
  expect_equal(pref_code(parse_pref(c("01000", "02000", "47000"))), vec_rep(NA_integer_, 3))
  expect_equal(pref_code(parse_pref(c("01000", "02000", "47000"), strict = FALSE)), c(1, 2, 47))
})
