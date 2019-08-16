

# sallow level detection of LibreOffice for support of doc
# check this https://github.com/hrbrmstr/docxtractr/issues/23
detect_LibreOffice <- function(type = 1) {
  out <- FALSE
  if (is_available("docxtractr")) {
    if (!identical(type, 2)) {
      get_lo <- function() {
        # an internal function of docxtractr
        # this will work even if they change `lo_find`
        # as then it will be operational by 2nd option below
        lo_find()
      }
      # this is to avoid docxtractr's internal lo_find
      environment(get_lo) <- environment(docxtractr::read_docx)
      test <- try(get_lo(), silent = TRUE)
      if (!inherits(test, "try-error")) {
        if (is.character(test)) {
          if (file.exists(test)) {
            out <- TRUE
          }
        }
      }
    }

    if (identical(type, 2) & !out) {
      # heuristic option (should work even when lo_find change)
      bad_doc <- "*?@:bad.doc"
      try(docxtractr::read_docx(bad_doc), silent = TRUE)
      test_op <- options("path_to_libreoffice")[[1]]
      if (!is.null(test_op)) {
        if (is.character(test_op)) {
          if (file.exists(test_op)) {
            out <- TRUE
          }
        }
      }
    }
  }
  out
}
