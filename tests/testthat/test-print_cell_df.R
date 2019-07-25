test_that("print cell-df works", {
  x <- head(iris)
  cd <- x %>% as_cell_df()
  cdv <- cd %>% basic_classifier()
  g1 <- plot(cd, no_plot = TRUE)
  g2 <- plot(cdv, no_plot = TRUE)
  # following should run without error or warning
  wnow <- options("warn")[[1]]
  options(warn = 2)

  plot(cdv, no_fill = TRUE)
  plot(cdv, adaptive_txt_size = FALSE)
  plot(cdv, no_txt = TRUE)

  options(warn = wnow)

  expect_equal(rlang::as_name(g1$mapping$fill), "data_type")
  expect_equal(rlang::as_name(g2$mapping$fill), "type")
})
