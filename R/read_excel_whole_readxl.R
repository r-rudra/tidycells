# readxl
# possible_date_range is somehow inspired by LibreOffice
read_excel_whole_part_readxl <- function(fn, sheet = 1L, possible_date_range = c(as.Date("1930-01-01"), Sys.Date() + 3800)) {
  if (!rlang::is_installed("readxl")) {
    abort("'readxl' package is required")
  }

  suppressWarnings({
    d0 <- readxl::read_excel(fn,
      col_names = FALSE, col_types = "text", sheet = sheet, .name_repair = "minimal"
    )
    d1 <- readxl::read_excel(fn,
      col_names = FALSE, col_types = "date", sheet = sheet, .name_repair = "minimal"
    )
  })

  possible_date_range <- as.character(possible_date_range)

  d0f <- as.matrix(d0) %>% as.character()
  d1f <- as.matrix(d1) %>% as.character()
  d1f <- ifelse(d1f <= max(possible_date_range) & d1f >= min(possible_date_range), d1f, NA_character_)

  df <- ifelse(is.na(d1f), d0f, d1f)

  d <- matrix(df, nrow = nrow(d0))

  as.data.frame(d, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()
}

read_excel_whole_readxl <- function(fn) {
  if (!rlang::is_installed("readxl")) {
    abort("'readxl' package is required")
  }
  sheets <- readxl::excel_sheets(fn)
  lout <- sheets %>% map(~ read_excel_whole_part_readxl(fn, sheet = .x))
  names(lout) <- sheets
  lout
}
