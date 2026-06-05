test_that("country_plastic_prop returns correct structure", {
  dat <- load_data()
  result <- country_plastic_prop(dat, country_name = "China")
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc"))
  expect_true(all(result >= 0 & result <= 1, na.rm = TRUE))
})

test_that("country_plastic_prop errors on bad country", {
  dat <- load_data()
  expect_error(country_plastic_prop(dat, country_name = "Fake Country XYZ"))
  expect_error(country_plastic_prop(dat, country_name = 123))
})
