# few misc test for further code coverage

test_that("etc works", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

  cd <- read_cells(fn, at_level = "make_cells")[[1]]
  expect_true(stringr::str_detect(cd %>% unlist() %>% paste0(collapse = "\n"), "Nakshatra"))
  expect_false(stringr::str_detect(mask_data(cd) %>% unlist() %>% paste0(collapse = "\n"), "Nakshatra"))

  cd <- numeric_values_classifier(cd)
  ca <- analyze_cells(cd)
  dcomp1 <- compose_cells(ca)
  expect_error(attach_trace_info(dc = dcomp1), "'ca' is not given")
  expect_error(attach_trace_info(dc = dcomp1), "not contain trace information")
  dc2 <- attach_trace_info(ca, dcomp1)
  dc3 <- attach_trace_info(ca, dcomp1 %>% dplyr::filter(row > 2))
  expect_equal(nrow(dc2), 4)
  expect_equal(nrow(dc3), 2)
  expect_false(identical(dc2, dcomp1))

  expect_message(get_col_representative(rnorm(3000), silent = FALSE), "set.seed")
  expect_equal(norm_this(0.6), 1)
  expect_equal(norm_this(0.1), 0)

  dc0 <- readRDS("testdata/enron_from_unpivotr_processed.rds") %>%
    analyze_cells()

  dc00 <- dc0 %>%
    compose_cells(post_process = FALSE)

  dc01 <- dc00 %>%
    collate_columns(combine_threshold = 0.1)
  dc02 <- dc00 %>%
    collate_columns(combine_threshold = 1)

  expect_false(identical(colnames(dc02), colnames(dc01)))

  # test as_cell_df_internal.default
  c1 <- dc2 %>%
    dplyr::select(row, col, value) %>%
    as_cell_df_internal.default()
  c2 <- dc2 %>%
    dplyr::select(row, col, omg = value) %>%
    as_cell_df_internal.default()
  expect_identical(c1, c2 %>% dplyr::select(-omg))

  # read date object
  if (rlang::is_installed("xlsx") & rlang::is_installed("readxl")) {
    dex <- "testdata/test.xls"
    m1 <- read_xls_from_xlsx(dex)[[1]] %>%
      as_cell_df() %>%
      as.matrix()
    m2 <- read_excel_whole_readxl(dex)[[1]] %>%
      as_cell_df() %>%
      as.matrix()

    expect_identical(m1[, -3], m2[, -3])
    expect_equal(m1[4, 2], "2019-06-15")
    expect_equal(m1[5, 2], "1245")

    cd0 <- read_xls_from_xlsx(dex)[[1]] %>%
      as_cell_df() %>%
      numeric_values_classifier()
    ca0 <- analyze_cells(cd0)

    expect_output(print(ca0), "Potential Issues:")

    # test few of the plot issues section
    g <- plot(ca0, plot_issues = TRUE)

    expect_true(inherits(g, "ggplot"))
  }

  lvls <- 1:5

  lvls %>%
    purrr::map(~ expect_output(print(read_cells(fn, at_level = .x, simplify = FALSE)), read_cell_task_orders[.x]))
  expect_output(print(read_cells(fn, at_level = 2, simplify = FALSE)), "A partial read_cell")
})

test_that("doc works", {
  #  too slow in local windows
  if (!isTRUE(as.logical(Sys.getenv("CI")))) {
    skip_on_os("windows")
  }

  u <- possible_to_support()
  fty <- u %>%
    dplyr::filter(support_possible) %>%
    dplyr::pull(file_type)

  if ("doc" %in% fty) {
    fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
    fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

    expect_message(dc <- read_cells("testdata/doc.doc"), "slow")
    expect_equal(dc, read_cells(fn))
  }
})

test_that("etc 2 works", {
  expect_equal(
    is_conforms_to_rcdf(tibble(x = 1, row = 1, col = c(NA, 2))) %>%
      attr("msg"),
    "NA present in cols"
  )
  expect_equal(
    is_conforms_to_rcdf(tibble(x = 1, col = 1, row = c(NA, 2))) %>%
      attr("msg"),
    "NA present in rows"
  )

  expect_equal(
    is_conforms_to_rcdf(tibble(x = 1, col = -1, row = 2)) %>%
      attr("msg"),
    "all cols are not positive"
  )
  expect_equal(
    is_conforms_to_rcdf(tibble(x = 1, col = 1, row = -2)) %>%
      attr("msg"),
    "all rows are not positive"
  )

  expect_message(
    expect_warning(
      tibble(row = 1, col = -1, x = 1, y = 1) %>%
        as_cell_df(),
      "all cols are not positive"
    )
  )

  expect_message(
    expect_warning(
      tibble(row = -1, col = 1, x = 1, y = 1) %>%
        as_cell_df(),
      "all rows are not positive"
    )
  )

  expect_message(
    expect_warning(
      tibble(row = 1, col = 2, x = 1, y = 1) %>%
        as_cell_df(),
      "column containing value is not detectable"
    )
  )

  # this should be ok [with no warn or error]
  tibble(row = 1, col = 2, value = 1, x = 1, y = 1) %>%
    as_cell_df()
  tibble(value = 1, x = 1, y = 1) %>%
    as_cell_df()
  tibble(x = 1, y = 1) %>%
    as_cell_df()

  expect_message(
    expect_warning(
      tibble(col = 2, x = 1, y = 1) %>%
        as_cell_df(),
      "row/col column not present"
    )
  )

  expect_message(
    expect_warning(
      tibble(row = 2, x = 1, y = 1) %>%
        as_cell_df(),
      "row/col column not present"
    )
  )

  expect_error(as_cell_df_internal.default(NULL))
  expect_error(as_cell_df_internal.default(data.frame()))
  expect_error(as_cell_df_internal.readr(NULL))
  expect_error(as_cell_df_internal.readr(data.frame()))
  expect_error(as_cell_df_internal.tidyxl(NULL))
  expect_error(as_cell_df_internal.tidyxl(data.frame()))
  expect_error(as_cell_df_internal.unpivotr(NULL))
  expect_error(as_cell_df_internal.unpivotr(data.frame()))
  expect_error(as_cell_df_internal.cell_df(NULL))
  expect_error(as_cell_df_internal.cell_df(data.frame()))

  expect_error(
    value_attribute_classify(NULL),
    "Cell DF Expected"
  )
  expect_error(
    tibble(x = 1, y = 1) %>%
      as_cell_df() %>%
      value_attribute_classify(NULL),
    "not a function"
  )
  expect_error(
    tibble(x = 1, y = 1) %>%
      as_cell_df() %>%
      value_attribute_classify(function(x) x %>% dplyr::mutate(type = "not_valid")),
    "content differs from value, attribute, empty"
  )

  # should be fine
  tibble(x = 1, y = 1) %>%
    as_cell_df() %>%
    value_attribute_classify()

  expect_identical(
    tibble(x = 1, y = 1) %>%
      as_cell_df(take_row_names = TRUE) %>%
      basic_classifier() %>%
      analyze_cells(),
    tibble(x = 1, y = 1) %>%
      as_cell_df(take_row_names = TRUE) %>%
      basic_classifier() %>%
      analyse_cells()
  )

  expect_error(tibble(x = 1, y = 1) %>%
    as_cell_df() %>%
    basic_classifier() %>%
    analyze_cells(), "attribute")

  expect_error(tibble(x = "a", y = "b") %>%
    as_cell_df() %>%
    basic_classifier() %>%
    analyze_cells(), "value")

  if (is_available("docxtractr")) {
    lo1 <- detect_LibreOffice(type = 1)
    options(path_to_libreoffice = NULL)
    lo2 <- detect_LibreOffice(type = 2)
    expect_equal(lo1, lo2)
  }


  not_available("tidyxl")
  expect_null(system.file("extdata", "untidy.xlsx", package = "tidycells", mustWork = TRUE) %>% read_cells())
  not_available()

  expect_error(
    fj(data.frame(x = 1, y = 4), data.frame(x = 1, y = 1)),
    "unexpected error while joining"
  )

  cd <- readRDS("testdata/badcells.rds")
  expect_true(length(intersect(mask_data(cd)$value, cd$value)) == 0)
  ca <- analyze_cells(cd)
  plot(ca, plot_issues = TRUE, direction_text_on_all = TRUE)
  cell_composition_traceback(ca, trace_row = 1)
  expect_output(print(ca), "Potential Issues")

  expect_warning(
    tibble(x = list(1)) %>%
      as_cell_df(),
    "not yet implemented"
  )
  expect_equal(as_cell_df_internal.default(cd), cd)
  expect_message(
    expect_warning(
      tibble(row = 1, col = 1, x = list(1)) %>%
        as_cell_df(),
      "not atomic"
    )
  )
  expect_message(
    expect_warning(
      tibble(row = 1, col = 1, value = list(1)) %>%
        as_cell_df(),
      "not atomic"
    )
  )
})
