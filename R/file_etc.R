this_file_ext <- function(fn) {
  x <- stringr::str_split(fn, "\\.")

  x %>% map_chr(~ {
    if (length(.x) == 1) {
      ""
    } else {
      rev(.x)[1]
    }
  })
}

is_txt_file <- function(fn) {
  f <- file(fn, "rb", raw = TRUE)
  bytes <- readBin(f, "int", 1000, size = 1, signed = FALSE)
  close(f)
  return(max(bytes) <= 128)
}



common_file_error <- function(fn) {
  if (missing(fn)) {
    abort("No file name given")
  }
  if (length(fn) != 1) {
    abort("only one file name supported")
  }
  if (!file.exists(fn)) {
    abort("file does not exists (or possibly you do not have permission)")
  }
}

crude_format_from_signature <- function(fn) {
  common_file_error(fn)

  f_8 <- readBin(fn, n = 8, what = "raw")
  # as xls and doc both have same magic number "D0 CF 11 E0 A1 B1 1A E1"
  # many other have it though
  # ref : https://asecuritysite.com/forensics/magic
  xls_doc_magic <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
  # either xlsx or docx
  # as xlsx and docx both have same magic number "50 4B 03 04"
  xlsx_docx_magic <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
  pdf_magic <- as.raw(c(0x25, 0x50, 0x44, 0x46))

  if (identical(f_8, xls_doc_magic)) {
    return("xls_doc")
  }
  if (identical(f_8[1:4], xlsx_docx_magic)) {
    return("xlsx_docx")
  }
  if (identical(f_8[1:4], pdf_magic)) {
    return("pdf")
  }
  return("unknown")
}
