


test_that("collate_columns works", {
  d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>%
    readRDS()
  d <- numeric_values_classifier(d)
  da <- analyze_cells(d)

  expect_output(
    dc1 <- compose_cells(da, print_attribute_overview = TRUE),
    "Nakshatra Gayen, Titas Gupta, Ujjaini Gayen, Utsyo Roy"
  )
  dc2 <- compose_cells(da, post_process = FALSE)
  cc1 <- collate_columns(dc1)
  cc2 <- collate_columns(dc2)

  expect_equal(cc1, cc2)
  expect_identical(
    cc1 %>% unlist() %>% unique() %>% sort(),
    cc2 %>% unlist() %>% unique() %>% sort()
  )

  #  all School should appear in single column
  expect_equal(
    cc1 %>% purrr::map_lgl(~ any(stringr::str_detect(.x, "School"))) %>% sum(),
    1
  )
  # all Male/Female should appear in single column
  expect_equal(
    cc1 %>% purrr::map_lgl(~ any(stringr::str_detect(.x, "Male"))) %>% sum(),
    1
  )
  # strict test (may be dropped in future)
  expect_equal(
    cc1 %>% map(~ .x %>%
      unique() %>%
      length()) %>% unlist() %>% sort() %>% as.numeric(),
    c(2, 2, 3, 12, 12)
  )
})




test_that("collate_columns colname convention works", {
  expect_error(collate_columns(NULL), "Given composed_data has no known format")
  expect_error(collate_columns(tibble()), "Given composed_data has no known format")
  expect_error(collate_columns(tibble(col = 1, value = 1, data_block = 1, table_tag = 1, major_1 = 1)), "Given composed_data has no known format")
  expect_error(collate_columns(tibble(row = 1, value = 1, data_block = 1, table_tag = 1, major_1 = 1)), "Given composed_data has no known format")
  expect_error(collate_columns(tibble(row = 1, col = 1, data_block = 1, table_tag = 1, major_1 = 1)), "Given composed_data has no known format")
  expect_error(collate_columns(tibble(row = 1, col = 1, value = 1, table_tag = 1, major_1 = 1)), "Given composed_data has no known format")

  expect_equal(
    collate_columns(tibble(row = 1, col = 1, value = 1, data_block = 1, table_tag = 1, x = 1)),
    structure(list(value = 1, table_tag = 1, collated_1 = 1),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    collate_columns(tibble(row = 1, col = 1, value = 1, data_block = 1, table_tag = 1, major = 1, minor = 3)),
    structure(list(value = 1, table_tag = 1, collated_1 = 1, collated_2 = 3),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    collate_columns(tibble(row = 1, col = 2, value = 3, data_block = 4, table_tag = 5, x = 6), retain_cell_address = TRUE),
    structure(list(row = 1, col = 2, value = 3, data_block = 4, table_tag = 5, collated_1 = 6),
      row.names = c(NA, -1L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
})
