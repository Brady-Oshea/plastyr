test_that("load_data returns a tibble with correct columns", {
  dat <- load_data()
  expect_s3_class(dat, "tbl_df")
  expect_true("country" %in% names(dat))
  expect_true("grand_total" %in% names(dat))
  expect_true("pet" %in% names(dat))
})
