test_that("plastic_year_change_test returns correct structure", {
  dat <- load_data()
  result <- plastic_year_change_test(dat)
  expect_type(result, "list")
  expect_true("test" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("country_changes" %in% names(result))
})

test_that("plastic_year_change_test errors on same year", {
  dat <- load_data()
  expect_error(plastic_year_change_test(dat, year_before = 2019, year_after = 2019))
})

test_that("plastic_year_change_test errors on non-numeric year", {
  dat <- load_data()
  expect_error(plastic_year_change_test(dat, year_before = "2019", year_after = 2020))
})
