
file_op_validate_file_input <- function(fn) {
  if (missing(fn) || is.null(fn)) {
    rlang::abort("No file name given.", call = NULL)
  }
  if (length(fn) != 1) {
    rlang::abort("Only one file name is supported.", call = NULL)
  }
  if (!is.character(fn)) {
    rlang::abort("File name must be a character string.", call = NULL)
  }
  if (nchar(fn) == 0) {
    rlang::abort("File name must be a non-empty character string.", call = NULL)
  }
  if (!file.exists(fn)) {
    rlang::abort(
      paste0("File does not exist (or you may not have permission): '", fn, "'"),
      call = NULL
    )
  }
  invisible(NULL)
}

file_op_is_text_file <- function(fn) {
  file_op_validate_file_input(fn) # Ensures fn is valid before reading

  f <- NULL # Initialize f to NULL
  tryCatch({
    f <- file(fn, "rb", raw = TRUE)
    # Read up to 1000 bytes, or fewer if file is smaller
    bytes <- readBin(f, "int", n = 1000, size = 1, signed = FALSE)
    if (length(bytes) == 0) {
      return(TRUE) # Empty file can be considered text
    }
    return(max(bytes) <= 128)
  }, error = function(e) {
    return(FALSE) # Default to not text on error
  }, finally = {
    if (!is.null(f) && inherits(f, "file") && isOpen(f)) {
      close(f)
    }
  })
}

file_op_identify_format_by_signature <- function(fn) {
  txt_chk <- file_op_is_text_file(fn)

  if (txt_chk) {
    return("txt")
  }

  file_op_validate_file_input(fn) # Ensures fn is valid before reading

  f_conn <- NULL
  tryCatch({
    f_conn <- file(fn, "rb")
    f_8 <- readBin(f_conn, n = 8, what = "raw")
  }, error = function(e) {
    f_8 <- NULL
  }, finally = {
    if (!is.null(f_conn)) close(f_conn)
  })

  if (length(f_8) < 8) {
    return("unknown")
  } # File too short for full signature check

  # Magic numbers
  xls_doc_magic <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
  xlsx_docx_magic <- as.raw(c(0x50, 0x4b, 0x03, 0x04)) # Checks only first 4 bytes
  pdf_magic <- as.raw(c(0x25, 0x50, 0x44, 0x46)) # Checks only first 4 bytes

  if (identical(f_8, xls_doc_magic)) {
    return("xls_doc")
  }
  if (length(f_8) >= 4 && identical(f_8[1:4], xlsx_docx_magic)) {
    return("xlsx_docx")
  }
  if (length(f_8) >= 4 && identical(f_8[1:4], pdf_magic)) {
    return("pdf")
  }
  return("unknown")
}

file_op_with_temp_file_ext <- function(original_fn, target_ext, reader_function) {
  ext_match <- tolower(tools::file_ext(original_fn)) == tolower(target_ext)

  tf_new <- if (ext_match) {
    original_fn
  } else {
    tf <- tempfile(fileext = paste0(".", target_ext))
    file.copy(original_fn, tf, overwrite = TRUE)
    on.exit(unlink(tf, force = TRUE), add = TRUE)
    tf
  }

  tryCatch(
    reader_function(tf_new),
    error = function(e) NULL
  )
}
