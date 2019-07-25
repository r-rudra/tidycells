
test_that("as_cell_df() works on tidyxl::xlsx_cells on single sheet", {
  skip_if_not_installed("tidyxl")
  cd <- system.file("extdata", "untidy.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()
  d0 <- as_cell_df(cd)

  cd <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()

  d1 <- cd %>%
    dplyr::filter(sheet == sheet[1]) %>%
    as_cell_df()

  expect_identical(d0, as_cell_df(d0))
  expect_true(inherits(d0, "cell_df"))
  expect_output(summary(d0), "88 x 25")
  expect_true(inherits(d1, "cell_df"))
  expect_output(summary(d1), "231 characters and 838 numbers")
})

test_that("as_cell_df() does not work on tidyxl::xlsx_cells on multiple sheets", {
  skip_if_not_installed("tidyxl")
  cd <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()

  expect_error(as_cell_df(cd))
})

test_that("as_cell_df() works on readr::melt_csv", {
  skip_if_not_installed("readr")
  skip_if_not_installed("datasets")
  tf <- tempfile()
  write.csv(datasets::iris, tf, row.names = F)
  d0 <- readr::melt_csv(tf) %>% as_cell_df()
  unlink(tf)

  expect_identical(d0, as_cell_df(d0))
  expect_true(inherits(d0, "cell_df"))
  expect_output(summary(d0), "151 x 5")
})

test_that("as_cell_df() works on unpivotr::as_cells", {
  d0 <- unpivotr::as_cells(data.frame(x = 1:26, letters)) %>% as_cell_df()

  expect_identical(d0, as_cell_df(d0))
  expect_true(inherits(d0, "cell_df"))
  expect_output(summary(d0), "26 characters and 26 numbers")
})

test_that("as_cell_df() works on data.frame", {
  d0 <- as_cell_df(data.frame(x = 1:26, y = letters))

  expect_identical(d0, as_cell_df(d0))
  expect_true(inherits(d0, "cell_df"))
  expect_output(summary(d0), "26 characters and 26 numbers")

  skip_if_not_installed("datasets")
  d1 <- as_cell_df(datasets::iris)
  expect_true(inherits(d1, "cell_df"))
})
