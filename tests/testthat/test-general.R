test_that("All Functions Work (Basic)", {

  # This should run without error

  sh1 <- read_cells(
    test_path("testdata","prototype_untidy.xlsx"),
    show_progress = FALSE,
    finalize_through_stages = FALSE)

  expect_s3_class(sh1, "sheets")

  expect_s3_class(sh1$Normal, "cells")

  expect_equal(package_version("1.2.1"),packageVersion("dplyr"))

  options(warn = 2L)
  # Whole processing
  capture_output(
    df <- read_cells(
      test_path("testdata","prototype_untidy.xlsx"),
      show_progress = TRUE,
      finalize_through_stages = TRUE)
  )

  # Check for df to be data frame with non zero rows
  expect_s3_class(df, "data.frame")
  expect_gt(NROW(df), 0)

  # Not real test (checks only for error)
  # plot(sh1$Normal)
  # print(sh1$Normal)
  # print(sh1)


})
