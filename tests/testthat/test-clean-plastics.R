test_that("clean_plastics returns a data frame", {
  dat <- load_data()
  result <- clean_plastics(dat)
  expect_s3_class(result, "data.frame")
})

test_that("clean_plastics removes EMPTY countries", {
  dat <- load_data()
  result <- clean_plastics(dat)
  expect_false("EMPTY" %in% result$country)
})

test_that("clean_plastics removes zero grand_total rows", {
  dat <- load_data()
  result <- clean_plastics(dat)
  expect_true(all(result$grand_total > 0))
})
