test_that("optional_package dependency test", {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  dm <- tibble::tibble(fn = list.files(fold, full.names = TRUE))

  dm <- dm %>%
    dplyr::mutate(original = dm$fn %>%
      purrr::map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        purrr::map_chr(1)))

  # both option for csv must result in same output (on test data at least)
  if (is_available("readr")) {
    d1 <- read_cells(dm$fn[dm$original == "csv"])
    not_available("readr")
    d2 <- read_cells(dm$fn[dm$original == "csv"])
    not_available()
    expect_equal(d1, d2)
  }

  # both option for xls must result in same output (on test data at least)
  if (is_available("xlsx") & is_available("readxl")) {
    d1 <- read_cells(dm$fn[dm$original == "xls"])
    not_available("xlsx")
    expect_message(d2 <- read_cells(dm$fn[dm$original == "xls"]), "readxl")
    expect_equal(d1, d2)
    not_available(c("readxl", "docxtractr"))
    not_available("cli")
    expect_output(possible_to_support(), "readxl")
    expect_output(possible_to_support(), "docxtractr")
    expect_null(read_cells(dm$fn[dm$original == "xls"]))
    not_available()
  }
})
