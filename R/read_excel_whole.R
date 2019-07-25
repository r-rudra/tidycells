read_excel_whole <- function(fn) {
  if (rlang::is_installed("xlsx")) {
    # preferred
    read_xls_from_xlsx(fn)
  } else {
    message("Using readxl to read xls. Manually check date and numeric cells. (for better result install xlsx package)")
    read_excel_whole_readxl(fn)
  }
}
