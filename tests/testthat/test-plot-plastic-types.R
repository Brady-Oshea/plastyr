test_that("plot_plastic_types returns a ggplot", {
  dat <- load_data()
  p <- plot_plastic_types(dat, country_name = "China")
  expect_s3_class(p, "ggplot")
})

test_that("plot_plastic_types works for different countries", {
  dat <- load_data()
  p <- plot_plastic_types(dat, country_name = "Nigeria")
  expect_s3_class(p, "ggplot")
})
