
assign("safe_dependency_check", list(
  # this is for rJava deps
  xlsx = function() {
    chk <- F
    try(
      {
        if (is_xlsx_ok(no_pkg_check = T)) {
          sample_xls <- system.file("extdata", "messy", "xls.pdf", package = "tidycells", mustWork = TRUE)
          x <- read_xls_from_xlsx(sample_xls, no_pkg_check = T)
          if (is.data.frame(x[[1]])) {
            if (nrow(x[[1]]) > 0) {
              chk <- T
            }
          }
        }
      },
      silent = T
    )
    chk
  },
  # for tabulizer
  # mainly for https://github.com/ropensci/tabulizer/issues/106
  tabulizer = function() {
    chk <- F
    try(
      {
        if (requireNamespace("tabulizer", quietly = T)) {
          sample_pdf <- system.file("extdata", "messy", "pdf.docx", package = "tidycells", mustWork = TRUE)
          x <- read_pdf_from_tabulizer(sample_pdf, no_pkg_check = T)
          if (is.matrix(x[[1]])) {
            if (nrow(x[[1]]) > 0) {
              chk <- T
            }
          }
        }
      },
      silent = T
    )
    chk
  }
), envir = tidycells_pkg_env)
