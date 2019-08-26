

test_that("read_cells for NULL works", {
  expect_output(read_cells(), "Support present for following type of files")
  expect_output(read_cells(), "Support is enabled for content type")
})

test_that("read_cells for external packages works", {
  ext_pkgs <- c(
    "readr", "readxl", "xlsx", "tidyxl", "docxtractr",
    "tabulizer", "XML"
  )

  if (rlang::is_installed("cli")) {
    cli_tck <- cli::symbol$tick
    cli_crs <- cli::symbol$cross
  } else {
    cli_tck <- "V"
    cli_crs <- "X"
  }

  ext_pkgs %>% purrr::map(~ {
    if (!rlang::is_installed(.x)) {
      expect_output(read_cells(), "These packages are required")
      expect_output(read_cells(), .x)
      expect_output(read_cells(), paste0(.x, " +", cli_crs))
    } else {
      expect_output(read_cells(), paste0(.x, " +", cli_tck))
    }
  })
})


test_that("read_cells: csv works", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]
  dcomp <- read_cells(fn, at_level = "compose") %>%
    dplyr::select(value, major_1, major_2) %>%
    dplyr::arrange(value)


  dc0 <- read_cells(fn) %>%
    dplyr::arrange(value) %>%
    as.matrix() %>%
    as.character() %>%
    sort()


  # strict check
  expct_d <- structure(list(
    value = c("1.5", "12", "16", "6"),
    major_1 = c(
      "Nakshatra", "Nakshatra",
      "Titas", "Titas"
    ), major_2 = c(
      "Age", "Weight",
      "Weight", "Age"
    )
  ),
  row.names = c(NA, -4L), class = c(
    "tbl_df",
    "tbl", "data.frame"
  )
  )

  expct_d2 <- c(
    "1.5", "12", "16", "6", "Age", "Age", "Kid Name", "Kid Name",
    "Kid Name", "Kid Name", "Nakshatra", "Nakshatra", "Table_1",
    "Table_1", "Table_1", "Table_1", "Titas", "Titas", "Weight",
    "Weight"
  )


  expect_equal(dcomp, expct_d)
  expect_equal(dc0, expct_d2)
})


test_that("read_cells: (for csv) chains works", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

  lvls <- c(0, seq_along(read_cell_task_orders))
  lvl <- c(0, seq_len(length(read_cell_task_orders) + 1))

  lvlsd <- lvls %>% purrr::map(~ read_cells(fn, at_level = .x, simplify = TRUE))
  lvld <- lvl %>% purrr::map(~ read_cells(fn, at_level = .x, simplify = FALSE))

  lvlsdchk <- lvlsd[1:6] %>% purrr::map(read_cells)
  lvldchk <- lvld %>% purrr::map(read_cells)

  expect_identical(
    read_cells(lvlsd[[5]], from_level = 4),
    read_cells(lvlsd[[5]])
  )

  expect_error(read_cells(lvlsd[[7]]), "No 'read_cells_stage' attribute found!")
  expect_true(lvlsdchk %>% purrr::map_lgl(~ identical(.x, lvlsdchk[[1]])) %>% all())
  expect_true(lvldchk %>% purrr::map_lgl(~ identical(.x, lvldchk[[1]])) %>% all())
  expect_equal(lvld[[7]], lvld[[8]])
  expect_true(is.data.frame(lvld[[7]]$final))
  expect_equal(
    lvld %>% purrr::map_chr(~ .x$stage),
    c(
      "init", "detect_and_read", "make_cells", "va_classify", "analyze",
      "compose", "collate", "collate"
    )
  )
  expect_identical(
    lvlsd %>% purrr::map(~ attr(.x, "read_cells_stage")),
    list(
      "init", "detect_and_read", "make_cells", "va_classify",
      "analyze", "compose", NULL
    )
  )
})

# optional tests depending on package installed
test_that("read_cells: external packages works (except pdf)", {
  skip_if_not_installed("readr")
  skip_if_not_installed("readxl")
  skip_if_not_installed("xlsx")
  skip_if_not_installed("tidyxl")
  skip_if_not_installed("docxtractr")
  skip_if_not_installed("tabulizer")
  skip_if_not_installed("XML")


  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  dm <- tibble::tibble(fn = list.files(fold, full.names = TRUE))

  dm <- dm %>%
    dplyr::mutate(original = dm$fn %>%
      purrr::map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        purrr::map_chr(1)))

  # remove pdf
  # for known issue https://github.com/ropensci/tabulizer/issues/106
  dm <- dm %>% dplyr::filter(original != "pdf")

  dm <- dm %>%
    dplyr::group_by(original) %>%
    dplyr::sample_n(1) %>%
    dplyr::ungroup()

  dtypes <- dm$fn %>% purrr::map(~ read_cells(.x, 1, simplify = FALSE))



  dcomps0 <- dm$fn %>% purrr::map(read_cells)
  dcomps0_sel <- dcomps0 %>%
    purrr::map(~ .x %>%
      dplyr::select(value, collated_1, collated_2, collated_3) %>%
      dplyr::arrange(value))

  dcomps <- dm$fn %>% purrr::map(read_cells, at_level = 5)
  dcomps_sel <- dcomps %>%
    purrr::map(~ .x %>%
      dplyr::select(value, major_1, major_2) %>%
      dplyr::arrange(value))

  expect_equal(dtypes %>% purrr::map_chr(~ .x$info$type), dm$original)
  expect_false(identical(
    read_cells(dm$fn[dm$original == "csv"], at_level = 1),
    read_cells(dm$fn[dm$original == "csv"], at_level = 1, omit = "csv")
  ))
  expect_true(dcomps_sel %>% purrr::map_lgl(~ identical(.x, dcomps_sel[[1]])) %>% all())
  expect_true(dcomps0_sel %>% purrr::map_lgl(~ identical(.x, dcomps0_sel[[1]])) %>% all())
})

test_that("read_cells: external packages works (for pdf)", {

  # perform this only in windows and linux
  skip_on_os("mac")
  skip_on_os("solaris")

  skip_if_not_installed("tabulizer")

  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  dm <- tibble::tibble(fn = list.files(fold, full.names = TRUE))

  dm <- dm %>%
    dplyr::mutate(original = dm$fn %>%
      purrr::map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        purrr::map_chr(1)))

  # this tested on only windows and linux (Travis)
  # for known issue https://github.com/ropensci/tabulizer/issues/106
  dm <- dm %>% dplyr::filter(original %in% c("pdf", "csv"))

  dm <- dm %>%
    dplyr::group_by(original) %>%
    dplyr::sample_n(1) %>%
    dplyr::ungroup()

  dtypes <- dm$fn %>% purrr::map(~ read_cells(.x, 1, simplify = FALSE))

  dcomps <- dm$fn %>% purrr::map(read_cells)
  dcomps_sel <- dcomps %>%
    purrr::map(~ .x %>%
      dplyr::select(value, collated_1, collated_2, collated_3) %>%
      dplyr::arrange(value))

  expect_equal(dtypes %>% purrr::map_chr(~ .x$info$type), dm$original)
  expect_true(dcomps_sel %>% purrr::map_lgl(~ identical(.x, dcomps_sel[[1]])) %>% all())
})


test_that("Partial read_cells works after manipulation", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fcsv <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

  rc_part1 <- read_cells(fcsv, at_level = 1)
  rc_part1 <- list(as_cell_df(rc_part1))

  dexpt1 <- rc_part1[[1]] %>%
    numeric_values_classifier() %>%
    analyze_cells() %>%
    compose_cells() %>%
    collate_columns()

  d0 <- rc_part1 %>%
    read_cells(from_level = 2) %>%
    dplyr::select(-table_tag)


  rc_part <- read_cells(fcsv, at_level = 2)
  rc_part[[1]] <- rc_part[[1]] %>% sample_based_classifier(empty_sample = "6")

  dexpt2 <- rc_part[[1]] %>%
    sample_based_classifier(empty_sample = "6") %>%
    analyze_cells() %>%
    compose_cells() %>%
    collate_columns()

  d1 <- rc_part %>%
    read_cells(from_level = 3) %>%
    dplyr::select(-table_tag)

  rc_partr <- read_cells(fcsv, at_level = 2, simplify = FALSE)
  rc_partr$cell_analysis_list <- rc_partr$cell_list %>%
    purrr::map(~ .x %>%
      sample_based_classifier(empty_sample = "6") %>%
      analyze_cells())
  d2 <- rc_partr %>%
    read_cells(from_level = 4) %>%
    dplyr::select(-table_tag)


  rc_part2 <- read_cells(fcsv, at_level = 4)
  rc_part2_1 <- rc_part2[[1]] %>% compose_cells()
  rc_part2_2 <- rc_part2[[1]] %>% compose_cells(discard_raw_cols = TRUE)

  d31 <- read_cells(rc_part2_1, from_level = 5)
  d32 <- read_cells(rc_part2_2, from_level = 5)


  expect_error(
    rc_part %>% read_cells(from_level = 0),
    "have you passed correct `from_level`. It should be one of"
  )

  expect_error(
    rc_part %>% read_cells(from_level = 7),
    "have you passed correct `from_level`. It should be one of"
  )

  expect_error(
    rc_part %>% read_cells(from_level = 4),
    "the object does not have required type for"
  )

  expect_error(
    rc_part %>% read_cells(from_level = 5),
    "the object does not have required type for"
  )

  expect_error(
    rc_part %>% read_cells(from_level = 6),
    "the object does not have required type for"
  )

  expect_equal(rc_part %>% read_cells(from_level = 1), rc_part %>% read_cells(from_level = 2))

  expect_false(identical(rc_part %>% read_cells(from_level = 2), rc_part %>% read_cells(from_level = 3)))

  expect_equal(d0, dexpt1)
  expect_equal(d1, dexpt2)
  expect_equal(d2, dexpt2)

  expect_equal(
    d31 %>% unlist() %>% unique() %>% sort(),
    d32 %>% unlist() %>% unique() %>% sort()
  )
})
