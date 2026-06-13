

cli_is_available <- function() {
  pkg_is_available("cli")
}

cli_bullet <- function() {
  if (cli_is_available()) {
    paste0(cli::symbol$menu, " ")
  } else {
    "* "
  }
}

cli_tick <- function() {
  if (cli_is_available()) {
    cli::symbol$tick
  } else {
    "V"
  }
}

cli_cross <- function() {
  if (cli_is_available()) {
    cli::symbol$cross
  } else {
    "X"
  }
}

cli_bold_blue <- function(x) {
  if (cli_is_available()) {
    cli::style_bold(cli::col_blue(x))
  } else {
    x
  }
}

cli_blue <- function(x) {
  if (cli_is_available()) {
    cli::col_blue(x)
  } else {
    x
  }
}

cli_red <- function(x) {
  if (cli_is_available()) {
    cli::col_red(x)
  } else {
    x
  }
}

cli_green <- function(x) {
  if (cli_is_available()) {
    cli::col_green(x)
  } else {
    x
  }
}

cli_bold_red <- function(x) {
  if (cli_is_available()) {
    cli::style_bold(cli::col_red(x))
  } else {
    x
  }
}

cli_print_excel_styled_matrix <- function(mat, col_width = 10, max_chars = 10, max_cols = 10, max_rows = 10) {

  if(!cli_is_available()){
    # Don't do anything if <cli> is not available
    return(invisible(0))
  }

  # check if mat is either matrix or data.frame
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    # Don't do anything
    return(invisible(0))
  }

  # Ensure max_chars does not exceed col_width
  max_chars <- min(max_chars, col_width - 1)

  mat <- mat[seq_len(min(nrow(mat), max_rows)), seq_len(min(ncol(mat), max_cols)), drop = FALSE]

  n_cols <- ncol(mat)
  n_rows <- nrow(mat)

  if(n_cols > 26){
    # Don't do anything if number of columns exceeds 26
    return(invisible(0))
  }

  col_letters <- LETTERS[1:n_cols]

  # ANSI styles using cli background helpers
  header_bg <- cli::combine_ansi_styles(cli::make_ansi_style("#000000"), cli::bg_cyan)
  row_even  <- cli::combine_ansi_styles(cli::make_ansi_style("#000000"), cli::bg_white)
  row_odd   <- cli::combine_ansi_styles(cli::make_ansi_style("#000000"), cli::bg_br_black)

  # Cell formatting
  format_cell <- function(x) {
    x <- ifelse(is.na(x), "", x)
    if (nchar(x) > max_chars) x <- paste0(substr(x, 1, max_chars - 1), "~")
    format(x, width = col_width, justify = "left")
  }

  # Header row
  col_labels <- paste0("\u2502", paste(format(col_letters, width = col_width), collapse = "\u2502"), "\u2502")
  cat("    ", header_bg(col_labels), "\n")

  # Data rows
  for (i in seq_len(n_rows)) {
    row_style <- if (i %% 2 == 0) row_even else row_odd
    cells <- vapply(mat[i, ], format_cell, character(1))
    row_text <- paste0("\u2502", paste(cells, collapse = "\u2502"), "\u2502")
    cat(row_style(sprintf(" %2d ", i)), row_style(row_text), "\n")
  }
}

