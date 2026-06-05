test_that("plastic_lm returns a list with model and summary", {
  dat <- load_data()
  result <- plastic_lm(dat, country_name = "China")
  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("summary" %in% names(result))
})

test_that("plastic_lm model is an lm object", {
  dat <- load_data()
  result <- plastic_lm(dat, country_name = "China")
  expect_s3_class(result$model, "lm")
})

test_that("plastic_lm errors on country with insufficient data", {
  dat <- load_data()
  expect_error(plastic_lm(dat, country_name = "Fake Country XYZ"))
})
