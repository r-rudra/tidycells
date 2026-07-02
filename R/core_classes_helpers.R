
# --- Methods linked to `cells` class ---

#' Internal function to convert data frames to cells format based on
#' intermediate structure.
#' @keywords internal
core_as_cells_internal <- function(df, ...) {
  UseMethod("core_as_cells_internal")
}


#' @noRd
#' @export
core_as_cells_internal.default <- function(
    df,
    include_colnames = TRUE,
    # include_rownames can be TRUE, FASLE and "auto".
    # If "auto", it will be TRUE if the data.frame has rownames which are non-numeric.
    include_rownames = "auto",
    translate_to_custom_type = TRUE,
    ...
) {

  # The [include_rownames = "auto"] case
  if (is.character(include_rownames) && include_rownames == "auto") {
    # \\D matches any non-digit character (including dot)
    include_rownames <- !is.null(rownames(df)) &&
      any(stringr::str_detect(rownames(df), "\\D"))
  }

  # Safe character conversion
  safe_char <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (is.raw(x)) return(vapply(x, rawToChar, character(1)))
    # this will cover Date and difftime (only POSIXlt is not atomic)
    if (is.atomic(x)) return(as.character(x))
    if (inherits(x, "POSIXt")) return(as.character(x))
    # otherwise, return a string indication of the object-type
    rep(paste0("<", class(x)[1], ">"), length.out = length(x))
  }

  # Type infer function
  if(translate_to_custom_type){
    infer_type <- function(x) {
      if (is.null(x)) return("NULL")
      if (is.raw(x)) return("character")
      if (is.logical(x)) return("logical")
      if (is.factor(x) ||is.ordered(x)) return("categorical")
      if (is.numeric(x) || inherits(x, "difftime")) return("numeric")
      if (inherits(x, "Date")) return("date")
      if (inherits(x, "POSIXt")) return("time")
      if (is.character(x)) return("character")
      paste0(typeof(x)) # Fallback for other types
    }
  }else{
    # use base type
    infer_type <- function(x) {
      # "logical", "integer", "double", "complex", "character", "raw", "list",
      # "NULL", "closure" (function), "special" and "builtin", "environment",
      # "S4" etc.
      typeof(x)
    }
  }

  nrows <- NROW(df)
  ncols <- NCOL(df)

  col_offset <- if (include_rownames) 1L else 0L
  row_offset <- if (include_colnames) 1L else 0L

  # convert to character each cols
  value_list <- lapply(df, safe_char)
  type_list  <- lapply(
    df,
    function(col) rep(infer_type(col), length.out = length(col))
  )

  rows <- rep(seq_len(nrows), times = ncols) + row_offset
  cols <- rep(seq_len(ncols), each = nrows) + col_offset

  val <- unlist(value_list, recursive = FALSE, use.names = FALSE)
  typ <- unlist(type_list, recursive = FALSE, use.names = FALSE)

  out <- tibble::tibble(
    row = rows,
    col = cols,
    value = val,
    data_type = typ
  )

  if (include_rownames) {
    r_part <- tibble::tibble(
      row = seq_len(nrows) + row_offset,
      col = 1L,
      value = rownames(df),
      data_type = "character"
    )

    out <- dplyr::bind_rows(r_part, out)
  }

  if (include_colnames) {

    c_part <- tibble::tibble(
      row = 1L,
      col = seq_len(ncols) + col_offset,
      value = colnames(df),
      data_type = "character"
    )

    out <- dplyr::bind_rows(c_part, out)
  }

  dplyr::arrange(out, row, col)
}

#' @noRd
#' @export
core_as_cells_internal.cells <- function(df, ...){
  # no change for `cells` class
  df
}

#' @noRd
#' @export
core_as_cells_internal.tidyxl <- function(df, dicard_extra_cols = TRUE, ...) {
  # Transformation for `tidyxl::xlsx_cells` output.
  dout <- df %>%
    dplyr::filter(!.data$is_blank, !is.na(.data$data_type)) %>%
    # as "data_type The type of a cell, referring to the following columns:
    # error, logical, numeric, date, character, blank."; No need error & blank
    tidyr::unite("value", dplyr::any_of(c("character", "numeric", "logical", "date")), na.rm = TRUE) %>%
    dplyr::mutate(
      data_type = dplyr::recode_values(
        .data$data_type,
        "numeric" ~ "numeric",
        "logical" ~ "logical",
        "date" ~ "date",
        default = "character"
      )
    )

  if(dicard_extra_cols) {
    dout <- dplyr::distinct(
      dout,
      .data$row, .data$col, .data$data_type, .data$value
    )
  }
  return(dout)
}

#' @noRd
#' @export
core_as_cells_internal.unpivotr <- function(df, dicard_extra_cols = TRUE, ...) {
  # Transformation for `unpivotr::as_cells` output.
  supported_types <- c("chr", "cpl", "cplx", "dbl", "fct", "int", "lgl", "ord", "date", "dttm")
  present_types <- intersect(supported_types, colnames(df))

  # Normalize factor-like fields
  if (utils::hasName(df, "fct")) {
    df$fct <- vapply(df$fct, function(x) if (length(x)) as.character(x)[1] else NA_character_, character(1))
  }
  if (utils::hasName(df, "ord")) {
    df$ord <- vapply(df$ord, function(x) if (length(x)) as.character(x)[1] else NA_character_, character(1))
  }

  dout <- if (length(present_types) > 0) {
    df %>%
      tidyr::unite("value", dplyr::any_of(present_types), na.rm = TRUE, remove = FALSE) %>%
      dplyr::mutate(
        data_type = dplyr::recode_values(
          .data$data_type,
          "date" ~ "date",
          "dttm" ~ "time",
          c("fct", "ord") ~ "categorical",
          c("dbl", "int", "cplx", "cpl") ~ "numeric",
          "lgl" ~ "logical",
          "chr" ~ "character",
          default = .data$data_type
        )
      )
  } else {
    dplyr::mutate(df, value = NA_character_)
  }


  if(dicard_extra_cols) {
    dout <- dplyr::distinct(
      dout,
      .data$row, .data$col, .data$data_type, .data$value
    )
  }

  return(dout)
}

#' @noRd
#' @export
core_as_cells_internal.meltr <- function(df, ...) {
  # Transformation for melted data from `meltr`.
  dplyr::mutate(
    df,
    data_type = dplyr::recode_values(
      .data$data_type,
      "double" ~ "numeric",
      "integer" ~ "numeric",
      "logical" ~ "logical",
      "date" ~ "date",
      "datetime" ~ "time",
      default = "character"
    )
  )
}

#' @noRd
core_as_cells_internal.this_format <- function(df, ...) {
  # No Transformation required for `this_format` class.
  df
}

core_detect_format_for_as_cells_internal <- function(df) {

  # Define known formats from different packages, specifying expected columns and data_type values
  chk <- tibble::tibble(
    # This is taken from core_cells_class_internal
    # (this_format : is the format of cells class)
    type = c("tidyxl", "unpivotr", "meltr", "this_format"),

    # Required columns typically found in each type of structure
    col_names = list(
      c("sheet", "address", "row", "col", "is_blank", "data_type", "error", "logical", "numeric", "date", "character"),
      c("row", "col", "data_type"),
      c("row", "col", "data_type", "value"),
      c("row", "col", "value", "data_type")
    ),

    # Expected values in the data_type column for each source
    data_types = list(
      c("error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cpl", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      c("integer", "character", "date"),
      # This is for cells class
      core_cell_recognized_format()
    ),

    # Optional columns often (but not always) found in the data
    optional_cols = list(
      c("sheet", "error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cpl", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      character(0),
      character(0)
    )
  )

  # Early return if data_type column is missing—can't proceed without it
  if (!utils::hasName(df, "data_type")) {
    return("unknown")
  }

  # For each known format, calculate:
  # - col_match: whether all required columns are present
  # - type_score: how many data_type values match known expected ones
  # - opt_score: how many optional columns are present
  scores <- chk %>%
    dplyr::mutate(
      col_match = purrr::map_lgl(.data$col_names, ~ all(.x %in% names(df))),
      extra_cols = purrr::map2_int(
        .data$col_names, .data$optional_cols,
        function(required, optional) {
          # Length of Extra cols in df from required + optional
          length(setdiff(names(df), c(required, optional)))
        }),
      type_score = purrr::map_int(.data$data_types, ~ sum(df$data_type %in% .x)),
      opt_score = purrr::map_int(.data$optional_cols, ~ sum(.x %in% names(df)))
    ) %>%
    dplyr::filter(.data$col_match)  # Only keep formats with all required columns

  # If no format matches required columns, return "unknown"
  if (nrow(scores) == 0) {
    return("unknown")
  }

  # Select the format with the best score: highest match on data_type and optional columns
  best_match <- scores %>%
    dplyr::filter(.data$extra_cols == min(.data$extra_cols)) %>%
    dplyr::filter(.data$type_score == max(.data$type_score)) %>%
    dplyr::filter(.data$opt_score == max(.data$opt_score)) %>%
    dplyr::pull(.data$type)

  # Return the matched type, or "unknown" if somehow none matched
  if (length(best_match) == 0) {
    return("unknown")
  }

  # Return only the first best match if there are multiple equally good ones
  best_match[1]
}

# This function bridges between public as_cells and internal as_cells
# (internal): core_as_cells_internal
core_do_as_cells_internal <- function(df, ..., silent = TRUE) {
  # Ensure the input is a data.frame
  if (!inherits(df, "data.frame")) {
    rlang::abort(
      "Input must be a data.frame or of equivalent type.",
      call = NULL)
  }
  # Detect the format of the input data
  internal_class <- core_detect_format_for_as_cells_internal(df)
  internal_class <- intersect(core_cells_class_internal, internal_class)

  if(length(internal_class) == 0) {
    internal_class <- "unknown"
  }

  class(df) <- c(internal_class, class(df))

  cells1 <- core_as_cells_internal(df, ...)

  required_cols <- c("row", "col", "data_type", "value")
  valid_cols <- c(
    "row", "col", "data_type", "value", "sheet","type", "PoA", "PoV"
  )
  this_cols <- intersect(valid_cols, colnames(cells1))

  # Check if all required columns are present
  if(!all(required_cols %in% this_cols)) {
    rlang::abort(
      paste0(
        "The input data have missing required columns: ",
        paste(setdiff(required_cols, this_cols), collapse = ", ")
      ),
      call = NULL
    )
  }

  extracols <- setdiff(colnames(cells1), valid_cols)

  df_1 <- cells1[this_cols]

  # other type present
  df_2 <- df_1 %>%
    # data_type should be one of core_cell_recognized_format() - rest discard
    dplyr::filter(.data$data_type %in% core_cell_recognized_format())

  if(!silent){
    message("Detected format: ", internal_class)
    if(length(extracols) > 0) {
      message("Extra columns detected: ", paste(extracols, collapse = ", "),
              " - These will be discarded!")
    }
    if(NROW(df_2) < NROW(df_1)) {
      message(
        "Some rows were filtered out due to unsupported data types:",
        paste0(setdiff(
          unique(df_1$data_type),
          core_cell_recognized_format()), collapse = ", "))
    }
  }

  core_ensure_cells_format(df_2)
}



# --- Methods linked to `sheets` class ---

# when each sheets are stacked into a single data frame, it has to be split by
# sheet to make it a valid sheets object
core_is_this_sheets_df <- function(dat) {
  issues_env <- new.env(parent = emptyenv())
  issues_env$log <- character(0)

  check <- function(cond, msg) {
    if (!isTRUE(cond)) {
      issues_env$log <- c(issues_env$log, msg)
    }
  }

  # 1. Must be a data.frame
  check(inherits(dat, "data.frame"), "Input must be a data.frame.")

  if (inherits(dat, "data.frame")) {
    # 2. Column names must be unique
    check(!anyDuplicated(names(dat)), "Column names must be unique.")

    # 3. Required sheet column must exist
    if(utils::hasName(dat, "sheet")) {
      check(is.atomic(dat$sheet) && (is.character(dat$sheet) || is.numeric(dat$sheet)),
            "'sheet' must be atomic and either character/numeric.")
      check(!anyNA(dat$sheet), "'sheet' contains NA values.")
      ld <- split(dat, dat$sheet, drop = TRUE)
      check(util_convertible_to_processable(ld), "Invalid format.")
    } else {
      check(FALSE, "'sheet' column is required.")
    }
  }

  result <- length(issues_env$log) == 0
  if (!result) attr(result, "msg") <- unique(issues_env$log)
  return(result)
}

#' Internal function to convert list or data.frames to sheets format based on
#' intermediate structure.
#' @keywords internal
core_as_sheets_internal <- function(lst, ...) {
  UseMethod("core_as_sheets_internal")
}

#' @noRd
#' @export
core_as_sheets_internal.list <- function(lst, ...) {
  # Ensure the input is a list of data.frames
  if(!util_convertible_to_processable(lst)){
    rlang::abort(
      "Input must be a list of data.frames/matrix (or of equivalent types).",
      call = NULL
    )
  }

  if(length(lst) == 0) {
    return(list())
  }

  # Check if list has unique names (if not, new names will be created)
  Names <- names(lst)
  Names0 <- paste0("sheet_", seq_along(lst))
  if(is.null(Names)) {
    Names <- Names0
  }
  # If names blank, take default names
  if(any(nzchar(Names))) {
    Names[which(!nzchar(Names))] <- Names0[which(!nzchar(Names))]
  }
  # Ensure names are unique
  if (any(duplicated(Names))) {
    Names <- util_make_unique_minimal(Names)
  }

  # Assign names to the list
  names(lst) <- Names

  # Convert each element of the list to cells format
  cells_list <- lapply(lst, as_cells, ...)

  return(cells_list)
}

#' @noRd
#' @export
core_as_sheets_internal.data.frame <- function(lst, ...) {

  if(NROW(lst) == 0) {
    return(list())
  }

  if(core_is_this_sheets_df(lst)) {
    # If the input is already a valid sheets data.frame, split it by 'sheet'
    lst$sheet <- as.character(lst$sheet)  # Ensure 'sheet' is character
    lst_2 <- split(lst, lst$sheet, drop = TRUE)
  } else {
    lst_2 <- list(sheet_default = lst)
  }

  # Convert each element of the list to cells format
  cells_list <- lapply(lst_2, as_cells, ...)

  return(cells_list)
}



# This function bridges between public as_sheets and internal as_sheets
# (internal): core_as_sheets_internal
core_do_as_sheets_internal <- function(lst, ...) {

  lst_2 <- core_as_sheets_internal(lst, ...)

  # Ensure the output is a list of cells objects
  core_ensure_sheets_format(lst_2)
}

