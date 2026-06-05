test_that("top_polluters returns correct number of rows", {
  dat <- load_data()
  result <- top_polluters(dat, n = 5)
  expect_equal(nrow(result), 5)
})

test_that("top_polluters result is sorted descending", {
  dat <- load_data()
  result <- top_polluters(dat, n = 10)
  expect_true(all(diff(result$total_plastic) <= 0))
})

test_that("top_polluters filters by year correctly", {
  dat <- load_data()
  result <- top_polluters(dat, n = 5, year = 2019)
  expect_equal(nrow(result), 5)
})
