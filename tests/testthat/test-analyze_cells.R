test_that("analyze_cell works: base", {
  d <- structure(c(
    "block 1", "", "C", "D", "", "block 2", "", "C",
    "D", "", "A", "1", "2", "", "", "A", "10", "20", "", "B", "3",
    "4", "", "", "B", "30", "40"
  ), .Dim = c(9L, 3L))
  d <- as.data.frame(d)
  cd <- as_cell_df(d) %>% numeric_values_classifier()
  ca <- analyze_cells(cd)

  expect_output(print(ca), "Total blocks: 2")
  expect_equal(ca$sections %>%
    dplyr::select(-gid) %>%
    unlist() %>%
    unique() %>%
    sort(), c(1, 2, 3, 4, 6, 9))
  expect_identical(ca$cell_df, cd)
  expect_equal(names(ca$details), c(
    "attr_details", "data_details",
    "data_attr_map_raw", "definiteness_checks"
  ))
})


test_that("analyze_cell works: tidyxl", {
  skip_if_not_installed("tidyxl")

  cd <- system.file("extdata", "untidy.xlsx", package = "tidycells", mustWork = TRUE) %>%
    tidyxl::xlsx_cells() %>%
    as_cell_df()

  cd <- numeric_values_classifier(cd)

  ca <- analyze_cells(cd)

  dc <- compose_cells(ca, discard_raw_cols = TRUE)

  dc_sel <- dc %>%
    dplyr::select(-row, -col, -data_block, -value) %>%
    unique()

  expect_equal(ca$details$data_attr_map_raw$data_gid %>% unique() %>% length(), 9)
  expect_equal(
    dc_sel$minor_1 %>% unique() %>% sort(),
    c(
      "ABOVE LEFT", "ABOVE LEFT border", "BELOW RIGHT", "BELOW RIGHT border",
      "NNE WSW", "NNW WNW :: NNE WSW", "SSE ESE", "SSE ESE Disconnected",
      "SSW ENE"
    )
  )
})
