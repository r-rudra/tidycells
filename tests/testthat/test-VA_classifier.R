test_that("numeric_values_classifier works", {
  skip_if_not_installed("tidyxl")
  cd <- system.file("extdata", "untidy.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()
  cd <- as_cell_df(cd)

  cdn <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()
  cdn <- cdn %>%
    dplyr::filter(sheet == sheet[1]) %>%
    as_cell_df()

  cd1 <- numeric_values_classifier(cd)
  cd2 <- numeric_values_classifier()(cd)
  cd3 <- value_attribute_classify(cd, classifier = numeric_values_classifier())

  cdn1 <- numeric_values_classifier(cdn)

  expect_output(
    numeric_values_classifier(cd, verbose = T),
    "New cells detected as different type\n# A tibble: 579 x 6"
  )
  expect_output(
    numeric_values_classifier(cdn, verbose = T),
    "New cells detected as different type\n# A tibble: 50 x 6"
  )
  expect_output(
    summary(cdn1),
    "Types: 838 values, 181 attributes and 50 empty cells"
  )
  expect_output(
    print(cdn1),
    "With Value/Attribute Classification"
  )
  expect_identical(cd1, cd2)
  expect_identical(cd1, cd3)
  expect_equal(c(table(cd1$type)), c("attribute" = 244, "value" = 579))
})


test_that("sample_based_classifier works", {
  skip_if_not_installed("tidyxl")
  cdn <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") %>%
    tidyxl::xlsx_cells()
  cdn <- cdn %>%
    dplyr::filter(sheet == sheet[1]) %>%
    as_cell_df()

  cd1 <- sample_based_classifier(cdn, value_sample = "APR")
  cd2 <- sample_based_classifier(value_sample = "APR")(cdn)
  cd3 <- value_attribute_classify(cdn, classifier = sample_based_classifier(value_sample = "APR"))


  cdn0 <- cdn %>% numeric_values_classifier()
  cdn2 <- cdn0 %>% sample_based_classifier(value_sample = "APR")
  cdn3 <- cdn0 %>% sample_based_classifier(value_sample = "aug")

  cdn4 <- cdn0 %>%
    sample_based_classifier(attribute_sample = "APR") %>%
    # revese mapping
    sample_based_classifier(value_sample = "APR")

  expect_output(summary(cdn2), "929 values, 90 attributes and 50 empty cells")
  expect_output(summary(cdn3), "838 values, 181 attributes and 50 empty cells")
  expect_identical(summary(cdn3, no_print = T), summary(cdn0, no_print = T))
  expect_output(summary(cdn4), "929 values, 90 attributes and 50 empty cells")
  expect_identical(cd1, cd2)
  expect_identical(cd1, cd3)
  expect_equal(c(table(cd1$type)), c("attribute" = 137, "value" = 932))
})
