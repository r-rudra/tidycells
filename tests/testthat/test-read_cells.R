

test_that("read_cells for NULL works", {
  expect_output(read_cells(), "Support present for following type of files")
  expect_output(read_cells(), "support is enabled for content type")
})

test_that("read_cells for external packages works", {
  ext_pkgs <- c(
    "readr", "readxl", "xlsx", "tidyxl", "docxtractr",
    "tabulizer", "XML"
  )

  if (rlang::is_installed("cli")) {
    cli_tick <- cli::symbol$tick
    cli_cross <- cli::symbol$cross
  } else {
    cli_tick <- "V"
    cli_cross <- "X"
  }

  ext_pkgs %>% purrr::map(~ {
    if (!rlang::is_installed(.x)) {
      expect_output(read_cells(), "These packages are required")
      expect_output(read_cells(), .x)
      expect_output(read_cells(), paste0(.x, " +", cli_cross))
    } else {
      expect_output(read_cells(), paste0(.x, " +", cli_tick))
    }
  })
})


test_that("read_cells: csv works", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]
  dcomp <- read_cells(fn) %>%
    dplyr::select(value, major_1, major_2) %>%
    dplyr::arrange(value)


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

  expect_equal(dcomp, expct_d)
})


test_that("read_cells: (for csv) chains works", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fn <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

  lvls <- 0:5
  lvl <- 0:6

  lvlsd <- lvls %>% purrr::map(~ read_cells(fn, at_level = .x, simplify = TRUE))
  lvld <- lvl %>% purrr::map(~ read_cells(fn, at_level = .x, simplify = FALSE))

  lvlsdchk <- lvlsd[1:5] %>% purrr::map(read_cells)
  lvldchk <- lvld %>% purrr::map(read_cells)

  expect_error(read_cells(lvlsd[[6]]), "No 'read_cells_stage' attribute found!")
  expect_true(lvlsdchk %>% purrr::map_lgl(~ identical(.x, lvlsdchk[[1]])) %>% all())
  expect_true(lvldchk %>% purrr::map_lgl(~ identical(.x, lvldchk[[1]])) %>% all())
  expect_equal(lvld[[6]], lvld[[7]])
  expect_true(is.data.frame(lvld[[6]]$final_composition))
  expect_equal(
    lvld %>% purrr::map_chr(~ .x$stage),
    c(
      "init", "detect_and_read", "make_cells", "va_classify", "analyze",
      "compose", "compose"
    )
  )
  expect_identical(
    lvlsd %>% purrr::map(~ attr(.x, "read_cells_stage")),
    list(
      "init", "detect_and_read", "make_cells", "va_classify",
      "analyze", NULL
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
  dm <- tibble::tibble(fn = list.files(fold, full.names = T))

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

  dcomps <- dm$fn %>% purrr::map(read_cells)
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
})

test_that("read_cells: external packages works (for pdf)", {

  # perform this only in windows
  skip_on_os("mac")
  skip_on_os("linux")
  skip_on_os("solaris")

  skip_if_not_installed("tabulizer")

  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  dm <- tibble::tibble(fn = list.files(fold, full.names = T))

  dm <- dm %>%
    dplyr::mutate(original = dm$fn %>%
      purrr::map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        purrr::map_chr(1)))

  # this tested on only windows
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
      dplyr::select(value, major_1, major_2) %>%
      dplyr::arrange(value))

  expect_equal(dtypes %>% purrr::map_chr(~ .x$info$type), dm$original)
  expect_true(dcomps_sel %>% purrr::map_lgl(~ identical(.x, dcomps_sel[[1]])) %>% all())
})


test_that("Partial read_cells works after manipulation", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  fcsv <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]
  rc_part <- read_cells(fcsv, at_level = 2)
  rc_part[[1]] <- rc_part[[1]] %>% sample_based_classifier(empty_sample = "6")
  # below should be similar to
  dexpt <- rc_part[[1]] %>%
    analyze_cells() %>%
    compose_cells(discard_raw_cols = TRUE)
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

  expect_error(
    rc_part %>% read_cells(from_level = 0),
    "read_cell_part object is not valid"
  )
  expect_error(
    rc_part %>% read_cells(from_level = 4),
    "the object does not have required type for"
  )
  expect_equal(d1, dexpt)
  expect_equal(d2, dexpt)
})
