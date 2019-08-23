
test_that("read_cells on real data works I", {
  skip_if_not_installed("tidyxl")

  dcpi <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells", mustWork = TRUE) %>%
    read_cells(at_level = "compose")

  chk1 <- dcpi$major_1 %>%
    unique() %>%
    stringr::str_detect("[a-zA-Z]")
  chk1 <- all(!chk1)
  expect_equal(unique(dcpi$minor_2), "Year/Month")
  expect_true(chk1)
  expect_equal(
    dcpi$major_2 %>% unique() %>% tolower() %>% sort(),
    month.abb %>% tolower() %>% sort()
  )
})


test_that("sample analysis on real data works II", {
  dwss <- readRDS("testdata/WSS.rds")

  da <- analyze_cells(dwss)
  dcmp <- compose_cells(da, discard_raw_cols = TRUE)

  chk1 <- dcmp$major_1 %>%
    unique() %>%
    stringr::str_detect("[a-zA-Z]")
  chk1 <- all(!chk1)

  expect_true(chk1)
  expect_equal(
    dcmp$major_2 %>% unique() %>% sort() %>% .[[15]],
    "3 Rupee Securities (including Treasury Bills)"
  )
})


test_that("sample analysis on real data works III", {
  cd <- readRDS("testdata/enron_from_unpivotr_processed.rds")

  ca <- analyze_cells(cd)
  expect_warning(dcmp <- compose_cells_raw(ca, discard_raw_cols = TRUE, ask_user = FALSE), "failed to compose")


  expect_equal(
    dcmp$data_block %>% unique() %>% length(),
    7
  )
  expect_equal(
    dcmp$major_1 %>% unique() %>% sort() %>% .[[6]],
    "Cash :: to :: 2002-03-01"
  )
})
