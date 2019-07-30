
finalize_lo <- function(lo) {
  if (length(lo$type) == 0) {
    lo$type <- c("unknown", lo$ext) %>% unique()
  }
  lo
}

# detect and read file type (and potentially read) based on content type
detect_and_read <- function(fn, silent = FALSE, omit = NULL) {
  common_file_error(fn)

  ext <- this_file_ext(fn)
  lo <- list(type = NULL, content = NULL, ext = ext)
  if (is_txt_file(fn)) {
    # the file is flat file [possible csv or html]
    lo$type <- c("csv", "html", "csv{utils}")

    # try html first
    if (!("html" %in% omit)) {
      if (rlang::is_installed("XML")) {
        read_try <- try(XML::readHTMLTable(fn, header = FALSE), silent = TRUE)
        if (inherits(read_try, "try-error")) read_try <- NULL
        if (length(read_try) != 0) {
          lo$type <- "html"
          lo$content <- read_try
          # decision done
          return(finalize_lo(lo))
        } else {
          # not html
          lo$type <- setdiff(lo$type, "html")
        }
      }
    }



    # try csv (melt csv first)
    if (!("csv" %in% omit)) {
      if (rlang::is_installed("readr")) {
        read_try <- try(readr::melt_csv(fn), silent = TRUE)
        if (inherits(read_try, "try-error")) read_try <- NULL
        if (is.data.frame(read_try)) {
          lo$type <- "csv"
          lo$content <- read_try
          # decision done
          return(finalize_lo(lo))
        } else {
          # not csv type
          lo$type <- setdiff(lo$type, "csv")
        }
      }
    }


    # last option is to use base read.csv
    if (!("csv{utils}" %in% omit)) {
      read_try <- try(utils::read.csv(fn, header = FALSE), silent = TRUE)
      if (inherits(read_try, "try-error")) read_try <- NULL

      if (is.data.frame(read_try)) {
        lo$type <- "csv{utils}"
        lo$content <- read_try
        # decision done
        return(finalize_lo(lo))
      } else {
        # not readable by base read.csv [mostly will never happen]
        lo$type <- setdiff(lo$type, "csv{utils}")
      }
    }
  } else {
    # binary formats
    cft <- crude_format_from_signature(fn)

    if (cft == "xls_doc") {
      lo$type <- c("xls", "doc")

      # try xls
      if (!("xls" %in% omit) | !("xls{readxl}" %in% omit)) {
        if (rlang::is_installed("readxl") | rlang::is_installed("xlsx")) {
          read_full <- FALSE
          if (rlang::is_installed("xlsx")) {
            read_try <- suppressMessages(try(xlsx::loadWorkbook(fn), silent = TRUE))
            if (inherits(read_try, "jobjRef")) {
              read_full <- TRUE
            }
          } else {
            read_try <- suppressMessages(try(readxl::read_excel(fn, n_max = 10), silent = TRUE))
            if (is.data.frame(read_try)) {
              read_full <- TRUE
            }
          }

          if (inherits(read_try, "try-error")) read_try <- NULL
          if (read_full) {
            lo$type <- "xls"
            # re read full data
            lo$content <- read_excel_whole(fn)
            # decision done
            return(finalize_lo(lo))
          } else {
            lo$type <- setdiff(lo$type, "xls")
          }
        }
      }

      # try doc
      if (!("doc" %in% omit)) {
        if (rlang::is_installed("docxtractr")) {
          if (lo$ext != "doc") {
            # need to rename the file as docxtractr detects by ext name
            tf <- tempfile(fileext = ".doc")
            file.copy(fn, tf, overwrite = TRUE)
            remove_at_end <- TRUE
          } else {
            tf <- fn
            remove_at_end <- FALSE
          }

          if (!silent) {
            if (detect_LibreOffice()) {
              message(paste0(
                "LibreOffice is present ",
                "(please wait as it may take some time to read/detect tables from possible doc file).",
                "(If it is too slow try opening LibreOffice outside this R-Session and retry)",
                "\nNote: If you want you may disable doc detection by setting omit = \"doc\"."
              ))
            } else {
              message("LibreOffice may be required for possible doc files. Check docxtractr::read_docx documentation")
            }
          }

          read_try <- suppressWarnings(suppressMessages(try(docxtractr::read_docx(tf), silent = TRUE)))
          if (inherits(read_try, "try-error")) read_try <- NULL

          if (inherits(read_try, "docx")) {
            lo$type <- "doc"

            # read full data
            suppressWarnings(
              suppressMessages(
                ctl <- docxtractr::docx_extract_all_tbls(read_try, guess_header = FALSE)
              )
            )

            if (is.null(names(ctl))) {
              names(ctl) <- seq_along(ctl) %>% paste0("Table_", .)
            }


            lo$content <- ctl

            if (remove_at_end) {
              unlink(tf, recursive = TRUE)
            }

            # decision done
            return(finalize_lo(lo))
          } else {
            lo$type <- setdiff(lo$type, "doc")
          }

          if (remove_at_end) {
            unlink(tf, recursive = TRUE)
          }
        }
      }
    }

    if (cft == "xlsx_docx") {
      lo$type <- c("xlsx", "docx")

      # try xlsx
      if (!("xlsx" %in% omit)) {
        if (rlang::is_installed("tidyxl")) {
          read_try <- try(tidyxl::xlsx_cells(fn), silent = TRUE)
          if (inherits(read_try, "try-error")) read_try <- NULL

          if (is.data.frame(read_try)) {
            lo$type <- "xlsx"
            lo$content <- read_try %>% split(.$sheet)
            # decision done
            return(finalize_lo(lo))
          } else {
            lo$type <- setdiff(lo$type, "xlsx")
          }
        }
      }

      # try docx
      if (!("docx" %in% omit)) {
        if (rlang::is_installed("docxtractr")) {
          if (lo$ext != "docx") {
            # need to rename the file as docxtractr detects by ext name
            tf <- tempfile(fileext = ".docx")
            file.copy(fn, tf, overwrite = TRUE)
            remove_at_end <- TRUE
          } else {
            tf <- fn
            remove_at_end <- FALSE
          }

          read_try <- suppressWarnings(suppressMessages(try(docxtractr::read_docx(tf), silent = TRUE)))
          if (inherits(read_try, "try-error")) read_try <- NULL

          if (inherits(read_try, "docx")) {
            lo$type <- "docx"

            # read full data
            suppressWarnings(
              suppressMessages(
                ctl <- docxtractr::docx_extract_all_tbls(read_try, guess_header = FALSE)
              )
            )

            if (is.null(names(ctl))) {
              names(ctl) <- seq_along(ctl) %>% paste0("Table_", .)
            }


            lo$content <- ctl

            if (remove_at_end) {
              unlink(tf, recursive = TRUE)
            }

            # decision done
            return(finalize_lo(lo))
          } else {
            lo$type <- setdiff(lo$type, "docx")
          }

          if (remove_at_end) {
            unlink(tf, recursive = TRUE)
          }
        }
      }
    }

    if (cft == "pdf") {
      lo$type <- c("pdf")

      if (!("pdf" %in% omit)) {
        if (rlang::is_installed("tabulizer")) {
          suppressWarnings(
            suppressMessages(
              read_try <- try(read_pdf_from_tabulizer(fn), silent = TRUE)
            )
          )
          if (inherits(read_try, "try-error")) read_try <- NULL

          if (is.list(read_try)) {
            lo$type <- "pdf"

            if (length(read_try) > 0) {
              if (is.null(names(read_try))) {
                names(read_try) <- seq_along(read_try) %>% paste0("Table_", .)
              }
            }

            lo$content <- read_try

            # decision done
            return(finalize_lo(lo))
          } else {
            lo$type <- setdiff(lo$type, "pdf")
          }
        }
      }
    }
  }

  return(finalize_lo(lo))
}
