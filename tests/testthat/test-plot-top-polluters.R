test_that("plot_top_polluters returns a ggplot", {
  dat <- load_data()
  p <- plot_top_polluters(dat)
  expect_s3_class(p, "ggplot")
})

test_that("plot_top_polluters respects n argument", {
  dat <- load_data()
  p <- plot_top_polluters(dat, n = 5)
  expect_s3_class(p, "ggplot")
})
