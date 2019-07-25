

# function to detect if xlsx working
is_xlsx_ok <- function() {
  if (!rlang::is_installed("xlsx")) {
    return(FALSE)
  }

  sample_xls <- system.file("extdata", "messy", "xls.pdf", package = "tidycells", mustWork = TRUE)

  suppressMessages(
    suppressWarnings(
      dxt <- try(xlsx::read.xlsx(sample_xls, sheetIndex = 1), silent = TRUE)
    )
  )

  if (is.data.frame(dxt)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
