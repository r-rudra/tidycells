
# I'll add few important masked data and expected outcome to make the logic robust
# ex1 added just after CRAN release. Thanks to the user for this contribution

test_that("use cases from users works - I", {
  cd <- readRDS("testusecases/ex1.rds")
  ca <- analyze_cells(cd)
  d0 <- ca$details$data_attr_map_raw %>% distinct(attr_gid, data_gid, direction, direction_group, attr_group)
  chk_str <- d0 %>%
    filter(attr_group == "major") %>%
    distinct(direction, direction_group) %>%
    arrange(direction, direction_group) %>%
    summarise(paste0(direction, ",", direction_group, collapse = ";")) %>%
    pull()
  expect_output(print(ca), "Total blocks: 1")
  expect_equal(nrow(ca$details$data_details$group_id_boundary), 1)
  expect_equal(chk_str, "E,WE;N,NS;W,WE")
})
