
# --- Assistant Functions ---
# Principle: `file_op_read_attempt` should be in "Main Reader Functions"
# Principle: `pkg_is_available` should be in "Assistant Functions"

file_op_read_attempt <- function(reader_fn, fn, silent = TRUE) {
  read_try <- tryCatch(
    {
      if (silent) {
        suppressMessages(suppressWarnings(reader_fn(fn)))
      } else {
        reader_fn(fn)
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (!util_convertible_to_processable(read_try)) {
    return(NULL)
  }

  return(read_try)
}

file_op_read_delim <- function(fn) {
  d_out <- NULL

  if (pkg_is_available("meltr")) {
    # Detect either csv or tsv based on number of columns
    d1 <- meltr::melt_csv(fn, n_max = 5)
    d2 <- meltr::melt_tsv(fn, n_max = 5)

    type <- "csv"

    if (NROW(d2) > NROW(d1)) {
      type <- "tsv"
    }

    if (type == "csv") {
      d_out <- meltr::melt_csv(fn)
    } else {
      d_out <- meltr::melt_tsv(fn)
    }
  } else {
    # This is less powerful reading csv/tsv files, but does not require <meltr>
    # detect either csv or tsv based on number of columns
    d1 <- utils::read.csv(fn, nrows = 5, stringsAsFactors = FALSE,
                   colClasses = "character", header = FALSE)
    d2 <- utils::read.delim(fn, nrows = 5, stringsAsFactors = FALSE,
                     colClasses = "character", header = FALSE)

    type <- "csv"

    if (sum(dim(d2)) > sum(dim(d1))) {
      type <- "tsv"
    }

    if (type == "csv") {
      d_out <- utils::read.csv(fn, stringsAsFactors = FALSE,
                        colClasses = "character", header = FALSE)
    } else {
      d_out <- utils::read.delim(fn, stringsAsFactors = FALSE,
                          colClasses = "character", header = FALSE)
    }
  }

  d_out
}

file_op_read_html_table <- function(fn) {
  if(pkg_is_available("rvest")) {
    # Use <rvest> to read HTML tables
    d_out <- rvest::html_table(rvest::read_html(fn))
    if (length(d_out) == 0) {
      return(NULL)
    }
    return(d_out)
  }

  return(NULL)
}

# this function may not be required as readxl package reads xls files cellwise
file_op_read_excel_from_xlsx <- function(fn) {

  # local functions
  to_time_or_date <- function(x) {
    if (identical(x, floor(x))) {
      as.Date(x - 25569, origin = "1970-01-01")
    } else {
      as.POSIXct((x - 25569) * 86400, tz = "UTC", origin = "1970-01-01")
    }
  }

  # Processes a single sheet to extract cell data.
  for_a_sheet <- function(sheet) {
    rows <- xlsx::getRows(sheet)
    if (length(rows) == 0) return(NULL)

    cells <- xlsx::getCells(rows)
    if (length(cells) == 0) return(NULL)

    res <- purrr::map(cells, xlsx::getCellValue)

    # Use the internal (as of now) xlsx function to guess cell types
    types <- util_utilize_namespace_object("xlsx", ".guess_cell_type", cells)

    dat <- purrr::map_dfr(seq_along(res), ~{
      coords <- as.integer(stringr::str_split(names(res)[.x], "\\.")[[1]])

      # Initialize a row of data
      d0 <- tibble::tibble(
        row = coords[1], col = coords[2],
        data_type = NA_character_, chr = NA_character_,
        date = as.Date(NA), dttm = as.POSIXct(NA), dbl = NA_real_
      )

      val <- res[[.x]]
      type <- types[.x]

      if(type == "POSIXct") {
        converted_val <- to_time_or_date(val)
        if(inherits(converted_val, "Date")){
          d0$data_type <- "date"
          d0$date <- converted_val
        } else {
          d0$data_type <- "dttm"
          d0$dttm <- converted_val
        }
      } else if(type == "numeric") {
        d0$data_type <- "dbl"
        d0$dbl <- val
      } else if(type == "character") {
        d0$data_type <- "chr"
        d0$chr <- val
      }
      d0
    })

    # Final cleanup and ordering
    dat %>% dplyr::arrange(row, col)
  }

  # Main function body
  wb <- xlsx::loadWorkbook(fn)
  sheets <- xlsx::getSheets(wb)

  sheets %>% purrr::map(for_a_sheet)
}

file_op_read_excel_from_readxl <- function(fn) {
  # Processes a single sheet to extract cell data.

  process_sheet <- function(path, sheet) {

    df_list <- readxl::read_excel(
      path, sheet = sheet, col_names = FALSE,
      col_types = "list", .name_repair = "minimal"
    )

    # Return NULL for empty sheets
    if (nrow(df_list) == 0 && ncol(df_list) == 0) {
      return(NULL)
    }

    all_coords <- expand.grid(
      row = seq_len(nrow(df_list)),
      col = seq_len(ncol(df_list)),
      stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE
    )

    purrr::map_dfr(seq_len(NROW(all_coords)), ~{
      r <- all_coords$row[.x]
      c <- all_coords$col[.x]

      value <- df_list[r, c][[1]][[1]]

      # Skip empty cells efficiently
      if(is.null(value) || length(value) == 0 || is.na(value)) return(NULL)

      # Determine the data type of the cell
      data_type <- dplyr::case_when(
        inherits(value, "POSIXct") ~ "dttm",
        inherits(value, "Date")    ~ "date",
        is.logical(value)          ~ "lgl",
        is.numeric(value)          ~ "dbl",
        TRUE                       ~ "chr"
      )

      # Build the final row for this cell
      tibble::tibble(
        row = r,
        col = c,
        data_type = data_type,
        chr  = if (data_type == "chr") as.character(value) else NA_character_,
        lgl  = if (data_type == "lgl") as.logical(value) else as.logical(NA),
        date = if (data_type == "date") as.Date(value) else as.Date(NA),
        dttm = if (data_type == "dttm") as.POSIXct(value) else as.POSIXct(NA),
        dbl  = if (data_type == "dbl") as.numeric(value) else NA_real_
      )
    })
  }

  # Main function body
  sheets <- readxl::excel_sheets(fn)

  all_sheets_data <- purrr::map(sheets, ~process_sheet(fn, .x))
  names(all_sheets_data) <- sheets

  all_sheets_data
}

file_op_read_xlsx <- function(fn){
  d_out <- NULL

  if(pkg_is_available("tidyxl")) {
    # Use <tidyxl> to read xlsx files
    d_out <- tidyxl::xlsx_cells(fn)
  }else if(pkg_is_available("readxl")) {
    # Use <readxl> to read xlsx files
    d_out <- file_op_read_excel_from_readxl(fn)
  }

  return(d_out)
}

file_op_read_xls <- function(fn){

  d_out <- NULL

  if(pkg_is_available("readxl")) {
    d_out <- file_op_read_excel_from_readxl(fn)
  }

  d_out
}

file_op_read_docx_table <- function(fn){

  d_out <- NULL

  if(pkg_is_available("docxtractr")) {
    # Use <docxtractr> to read docx files
    doc <- docxtractr::read_docx(fn)
    d_out <- docxtractr::docx_extract_all_tbls(doc, guess_header = FALSE)
  }

  return(d_out)
}

file_op_read_doc_table <- function(fn){
  d_out <- NULL

  if(pkg_is_available("docxtractr")) {
    # Use <docxtractr> to read docx files as <docxtractr> requires proper
    # extension for doc with_ext is used (for docx it is not required). Also
    # note LibreOffice is required for this to work (the same will not be
    # checked)
    read_doc_tbls <- function(fn){
      doc <- docxtractr::read_docx(fn)
      docxtractr::docx_extract_all_tbls(doc, guess_header = FALSE)
    }
    d_out <- file_op_with_temp_file_ext(fn, "doc", read_doc_tbls)
  }

  return(d_out)
}

file_op_read_pdf_table_using_tabula <- function(fn) {

  if(!pkg_is_available("tabulapdf")){
    return(NULL)
  }

  # --- Helper function for pairwise table comparison ---
  choose_best_result <- function(lattice_tbl, stream_tbl) {
    # Heuristic 1: Check for "degenerate" tables (e.g., only 1 row or col).
    # A non-degenerate table is almost always a more accurate extraction.
    dim_l <- dim(lattice_tbl)
    dim_s <- dim(stream_tbl)
    degen_l <- any(dim_l < 2)
    degen_s <- any(dim_s < 2)

    if (degen_l && !degen_s) return(stream_tbl)
    if (degen_s && !degen_l) return(lattice_tbl)

    # Heuristic 2: Prefer the table with fewer NAs. A high NA count suggests the
    # extraction included empty space around the table.
    na_l <- sum(is.na(lattice_tbl))
    na_s <- sum(is.na(stream_tbl))

    if (na_l < na_s) return(lattice_tbl)
    if (na_s < na_l) return(stream_tbl)

    # Heuristic 3: Prefer the table with the smaller area (rows * cols).
    # This often indicates a more tightly bounded, accurate extraction.
    if (prod(dim_l) <= prod(dim_s)) {
      return(lattice_tbl)
    } else {
      return(stream_tbl)
    }
  }

  # --- Attempt Extraction with Both Methods ---
  pl <- tryCatch(
    tabulapdf::extract_tables(fn, method = "lattice", output = "matrix"),
    error = function(e) e
  )
  ps <- tryCatch(
    tabulapdf::extract_tables(fn, method = "stream", output = "matrix"),
    error = function(e) e
  )

  # --- Handle Top-Level Failures ---
  is_pl_error <- inherits(pl, "error")
  is_ps_error <- inherits(ps, "error")

  if (is_pl_error && is_ps_error) return(NULL)
  if (is_pl_error) return(ps)
  if (is_ps_error) return(pl)

  # --- Apply Heuristics if Both Methods Succeed ---

  # If the number of tables is different, prefer the result set with more *total
  # cells*, as this is a better proxy for finding all the data than just the
  # table count.
  if (length(pl) != length(ps)) {
    cells_l <- sum(purrr::map_dbl(pl, ~prod(dim(.x))))
    cells_s <- sum(purrr::map_dbl(ps, ~prod(dim(.x))))
    return(if (cells_l >= cells_s) pl else ps)
  }

  # If the number of tables is the same, compare them pair-wise. This is
  # essentially a mix of both methods.
  purrr::map2(pl, ps, choose_best_result)
}

# --- Main Reader Functions ---

file_op_read_txt <- function(fn, omit = NULL) {

  d_out <- NULL

  if(!("html" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_html_table, fn)
  }

  if(!util_convertible_to_processable(d_out) &&
     !("csv" %in% omit) && !("tsv" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_delim, fn)
  }

  d_out

}

file_op_read_xlsx_docx <- function(fn, omit = NULL){
  d_out <- NULL

  if(!("xlsx" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_xlsx, fn)
  }

  if(!util_convertible_to_processable(d_out) &&
     !("docx" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_docx_table, fn)
  }

  d_out

}

file_op_read_xls_doc <- function(fn, omit = NULL){
  d_out <- NULL

  if(!("xls" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_xls, fn)
  }

  if(!util_convertible_to_processable(d_out) &&
     !("doc" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_doc_table, fn)
  }

  d_out

}

file_op_read_pdf <- function(fn, omit = NULL){
  d_out <- NULL

  if(!("pdf" %in% omit)) {
    d_out <- file_op_read_attempt(file_op_read_pdf_table_using_tabula, fn)
  }

  d_out

}
