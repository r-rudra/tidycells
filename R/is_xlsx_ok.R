

# function to detect if xlsx working
is_xlsx_ok <- function(no_pkg_check = F) {
  if (!no_pkg_check) {
    # this is created for safe dependency checks
    if (!is_available("xlsx")) {
      return(FALSE)
    }
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
