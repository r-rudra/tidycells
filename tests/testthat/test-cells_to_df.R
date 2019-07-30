test_that("cell_df to DF (and vice versa) works", {
  d0 <- data.frame(x = 1:26, letters, stringsAsFactors = FALSE)
  d1 <- as_cell_df(d0)

  d2 <- as.data.frame(d1)
  d3 <- as_cell_df(d2)
  d4 <- as.data.frame(d3)

  expect_equal(dim(d1), c(52, 4))
  expect_equal(dim(d3), c(52, 4))
  expect_equal(dim(d2), dim(d0))
  expect_equal(dim(d4), dim(d0))
  expect_identical(d2, d4)
  expect_identical(d3, as_cell_df(d4))
})
