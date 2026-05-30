test_that("plot_plastic_by_country returns a ggplot", {
  dat <- load_data()

  p_default <- plot_plastic_by_country(dat)
  p_top5    <- plot_plastic_by_country(dat, top_n = 5)

  expect_s3_class(p_default, "ggplot")
  expect_s3_class(p_top5, "ggplot")
})

test_that("plot_plastic_by_country errors on bad top_n", {
  dat <- load_data()
  expect_error(plot_plastic_by_country(dat, top_n = -1))
  expect_error(plot_plastic_by_country(dat, top_n = "ten"))
})
