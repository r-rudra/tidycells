test_that("All Functions Work (Basic)", {

  # This should run without error

  sh1 <- read_cells(
    test_path("testdata","prototype_untidy.xlsx"),
    show_progress = FALSE,
    finalize_through_stages = FALSE)

  expect_s3_class(sh1, "sheets")

  expect_s3_class(sh1$Normal, "cells")

  # expect_equal(package_version("1.2.1"),packageVersion("dplyr"))

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
  pdf(file = tempfile(fileext = ".pdf"))
  expect_no_error(plot(sh1$Normal))
  dev.off()

  capture_output(
    {
      expect_no_error(print(sh1$Normal))
      expect_no_error(print(sh1))
    }
  )

})


# headline basic test

test_that("All Functions Work (Basic - II)",{
  d0 <- system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>%
    read_cells(finalize_through_stages = TRUE)

  d <- d0 |>
    drop_constants() |>
    rename_by_content(
      name_map = list(empty1 = c("student", "name"), sex = c("male","female")),
      name_map_regex = list(school = "School [A-Z]")) |>
    dplyr::rename(name = dplyr::starts_with("collated_"))

  d <- d |>
    dplyr::select(school, name, sex, marks = value) |>
    dplyr::mutate(marks = as.numeric(marks))

  expect_identical(
    structure(
      list(school = "School A", name = "Nakshatra Gayen",
           sex = "Male", marks = 99),
      row.names = c(NA, -1L),
      class = class(d)),
    d |> dplyr::filter(name  |> stringr::str_detect("Nakshatra"))
  )

  expect_identical(
    structure(
      list(school = "School A", name = "Titas Gupta",
           sex = "Female", marks = 89),
      row.names = c(NA, -1L),
      class = class(d)),
    d |> dplyr::filter(name  |> stringr::str_detect("Titas"))
  )

  expect_identical(
    structure(
      list(school = "School C", name = "I Roy",
           sex = "Male", marks = 50),
      row.names = c(NA, -1L),
      class = class(d)),
    d |> dplyr::filter(name  |> stringr::str_detect("I Roy"))
  )

  expect_identical(
    structure(
      list(school = "School B", name = "Sarmistha Senapati",
           sex = "Female", marks = 81),
      row.names = c(NA, -1L),
      class = class(d)),
    d |> dplyr::filter(name  |> stringr::str_detect("Sarmistha"))
  )


  # Another Test
  d0 <- test_path("testdata","untidy.xlsx") %>%
    read_cells(finalize_through_stages = TRUE)

  d <- d0 |>
    rename_by_content(name_map = list(group = "group", cat = "Category", class = "class", subcat = "Subcategory "))

  d <- d |>
    tidyr::unite(
      col = "info_comb",
      dplyr::starts_with("info_"),
      sep = " ",
      remove = TRUE,
      na.rm = TRUE
    )

  # Value by value check

  ####### Check 1
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("SSE ESE Disconnected"),
    class == "Class-II", group == "Group-I",
    cat == "Category M", subcat == "Subcategory - E") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 10439)


  ####### Check 2
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("BELOW RIGHT border"),
    class == "Class-II", group == "Group-II",
    cat == "Category F", subcat == "Subcategory - D") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 12603)


  ####### Check 3
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("BELOW RIGHT"),
    !(info_comb |> stringr::str_detect("BELOW RIGHT border")),
    class == "Class-III", group == "Group-I",
    cat == "Category F", subcat == "Subcategory - C") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 9859)


  ####### Check 4
  c0 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("SSW ENE")) |>
    dplyr::pull("value")

  expect_equal(sum(as.integer(c0)), 735098)

  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("SSW ENE"),
    class == "Class-IV", group == "Group-II",
    cat == "Category M", subcat == "Subcategory - D") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 10761)

  c2 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("SSW ENE"),
    class == "Class-I", group == "Group-II",
    cat == "Category M", subcat == "Subcategory - E") |>
    dplyr::pull("value")

  expect_true(length(c2)==0)

  ####### Check 5 (not done) (kept for reference)
  # c1 <- d |> dplyr::filter(
  #   # This is not picked up which is fine (as seen in traceback)
  #   info_comb |> stringr::str_detect("ABOVE LEFT"),
  #   class == "Class-III", group == "Group-II",
  #   cat == "Category F", subcat == "Subcategory - D") |>
  #   dplyr::pull("value")
  # c1 <- as.numeric(c1) |> round()

  ####### Check 5
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("ABOVE LEFT border"),
    class == "Class-II", group == "Group-II",
    cat == "Category M", subcat == "Subcategory - C") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 13819)


  ####### Check 6 (not done)
  # c1 <- d |> dplyr::filter(
  #   # the corner is not mapped to it properly which is ok as of now
  #   info_comb |> stringr::str_detect("NNE WSW"),
  #   class == "Class-III", group == "Group-I",
  #   cat == "Category F", subcat == "Subcategory - B") |>
  #   dplyr::pull("value")
  # c1 <- as.numeric(c1) |> round()

  ####### Check 6
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("SSE ESE"),
    !(info_comb |> stringr::str_detect("SSE ESE Disconnected")),
    class == "Class-III", group == "Group-II",
    cat == "Category M", subcat == "Subcategory - C") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  expect_equal(c1, 9559)

  ####### Check 7
  c1 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("NNW WNW"),
    class == "Class-I", group == "Group-I",
    cat == "Category F", subcat == "Subcategory - D") |>
    dplyr::pull("value")
  c1 <- as.numeric(c1) |> round()

  c2 <- d |> dplyr::filter(
    info_comb |> stringr::str_detect("NNW WNW")) |>
    dplyr::pull("value")


  expect_equal(c1, 9022)
  expect_equal(sum(as.integer(c2)), 717579)

})

# key tests
test_that("Specific tests for prototype_untidy.xlsx", {
  capture_output(
    df <- read_cells(
      test_path("testdata","prototype_untidy.xlsx"),
      show_progress = TRUE,
      finalize_through_stages = FALSE)
  )

  ca <- df$Normal |> value_attribute_classify() |> analyze_cells()
  expect_equal(length(unique(ca$data_blocks$data_gid)), 7)

  # Not real test (checks only for error)
  pdf(file = tempfile(fileext = ".pdf"))
  expect_no_error(plot(ca))
  dev.off()

  d0 <- collate_columns(ca)

  d <- d0 |> drop_constants()

  d <- d |>
    rename_by_content(
      name_map = list(group = "group", cat = "Category", class = "class", subcat = "Subcategory "),
      include_cols = colnames(d)[stringr::str_detect(colnames(d), "^collated_")],
      unite_other_cols = TRUE, exclude_cols = "value",
      united_others_col_name = "merged_info")

  d$value <- as.integer(d$value)

  # various checks at d level

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 1")]),
    708448
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 3") & abs(d$value)>100]),
    552370
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab5") & abs(d$value)>100]),
    471248
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 2") & abs(d$value)>100]),
    471248
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 6") & abs(d$value)>100]),
    708448
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 6") &
                  abs(d$value)>100 &
                  stringr::str_detect(d$cat, "Category F")]),
    353251
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 6") &
                  abs(d$value)>100 &
                  stringr::str_detect(d$cat, "Category M") &
                  d$group == "Group-I"]),
    167807
  )

  expect_equal(
    sum(d$value[stringr::str_detect(d$merged_info, "Tab 6") &
                  abs(d$value)>100 &
                  stringr::str_detect(d$cat, "Category F") &
                  d$group == "Group-I" &
                  d$subcat == "Subcategory - C"]),
    31363
  )

})
