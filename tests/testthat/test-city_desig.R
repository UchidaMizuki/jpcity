test_that("city_desig_merge works", {
  city <- parse_city(c("01101", "13101"))

  expect_s3_class(city_desig_merge(city), "jpcity_city")
  expect_s3_class(city_desig_merge(city, merge_tokyo = TRUE), "jpcity_city")
})

test_that("city_desig_split works", {
  city <- parse_city(c("01100", "13100"))

  expect_true(is.list(city_desig_split(city)))
  expect_true(is.list(city_desig_split(city, split_tokyo = FALSE)))
})
