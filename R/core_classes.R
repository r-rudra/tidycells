
# The two primary structural classes in this system are `cells` and `sheets`.
#
# - `cells` represent a collection of cell-level data from a single sheet. Each cell is
#   identified by its (row, col) position in a 2D grid. The structure includes not just the
#   cell's value but also its source data type (`data_type`), its re-evaluated content
#   type (`type`), and optionally, probabilities of it being a value (`PoV`) or an
#   attribute (`PoA`).
#
# - `sheets` represent a collection of `cells` objects, each associated with a unique
#   sheet identifier. Internally, a `sheets` object is essentially a list of `cells` data
#   frames, where each list element corresponds to one sheet.
#
# Effectively, a `sheets` object or `list_of_cells` is a row-bound structure of multiple
# `cells`, organized by sheet.


#' The `cells` and `sheets` Class Structures
#'
#' @description The two primary data structures in this system are the `cells`
#'   and `sheets` classes. They provide a standardized, tidy format for
#'   representing tabular data. The fundamental building block is the `cells`
#'   object, which evolves through different processing stages.
#'
#' @section Recognized Content Formats: Throughout the system, cell content is
#'   classified into one of the following recognized formats: `"numeric"`,
#'   `"character"`, `"logical"`, `"categorical"`, `"date"`, `"time"` or
#'   `"blank"`.
#'
#' @section `cells` Structure and Value-Attribute Classification: A `cells`
#'   object is a `data.frame` representing data from a single sheet. Its
#'   structure evolves as it is processed.
#'
#'   \strong{1. Initial State (Post-Reading)}
#'
#'   After initial reading from a source, a `cells` object contains the
#'   following base columns:
#' \itemize{
#'   \item `row`, `col`: An integer pair uniquely identifying each cell's position.
#'   \item `data_type`: The type as read or inferred from the source (e.g., from Excel).
#'     Its value will be one of the Recognized Content Formats.
#'   \item `value`: The raw cell content, always stored as a character string.
#'   \item `sheet` (optional): If present, a single, non-NA value identifying the source sheet.
#' }
#'
#'   \strong{2. Processed State (Post-Value-Attribute Classification)}
#'
#'   The value-attribute classification process analyzes cell content and
#'   context to determine its role in the table. This augments the `cells`
#'   object with the following columns:
#' \itemize{
#'   \item `type`: The re-evaluated content type, based on a deeper examination
#'   of the cell's content. Like `data_type`, its value is one of the Recognized
#'   Content Formats.
#'   \item `PoA` (Probability of Attribute): A numeric value `>= 0` representing
#'   the probability of the cell being an attribute (e.g., a header).
#'   \item `PoV` (Probability of Value): A numeric value `>= 0` representing the
#'     probability of the cell being a data value.
#' }
#'   The constraint `PoA + PoV <= 1` must hold for each cell. The classification
#'   process uses the following heuristics regarding content types:
#' \itemize{
#'   \item \strong{Numeric}: These cells have a very high probability of being
#'   values (`PoV`). \item \strong{Categorical}: These cells can be either
#'   attributes or values, and their probabilities depend on context. `Logical`
#'   is considered a special, binary
#'     case of the `categorical` type.
#'   \item \strong{Date & Time}: Like categorical data, these cells often have a
#'     positive probability of being both attributes (e.g., a column header like
#'     "2023 Sales") and values.
#'   \item \strong{Character}: This is a general super-type. If content does not
#'   fit any other format, it is classified as character and typically has a
#'   higher probability of being an attribute (`PoA`).
#'   \item \strong{Blank}: These cells are empty or contain only whitespace or
#'   irrelevant information, and are classified as `"blank"` with both `PoA` and
#'   `PoV` equal to 0.
#' }
#'
#' @section `sheets` Structure:
#' A valid `sheets` object is a `list` with:
#' \itemize{
#'   \item Only valid `cells` objects as elements.
#'   \item Unique, non-empty names for each sheet.
#' }
#'
#' @docType class
#' @name cells-class
#' @aliases sheets-class
#' @seealso [as_cells()], [as_sheets()]
#' @exportClass cells
#' @exportClass sheets
NULL


core_cells_class <- c("cells", "sheet", "tbl_df", "tbl", "data.frame")
core_sheets_class <- c("sheets", "list")

# Class for analysis output of `cells` objects
core_cells_analysis_class <- c("cells_analysis", "sheet_analysis", "list")

# Class for analysis output of `cells` objects
core_cells_composition_class <- c("cells_composition", "sheet_composition", "list")


core_cells_class_internal <- c("tidyxl", "unpivotr", "meltr", "this_format", "unknown")

# Set the old class for S4 methods compatibility
methods::setOldClass(core_cells_class)
methods::setOldClass(core_sheets_class)
methods::setOldClass(core_cells_analysis_class)
methods::setOldClass(core_cells_composition_class)



# Recognized Content Formats for Cells
core_cell_recognized_format <- function() {
  c("numeric", "character", "logical", "date", "time", "categorical","blank")
}


# --- Methods on `cells` and `sheets` class ---


#' Validate a cells object
#'
#' @param dat An R object to validate against the `cells` definition.
#' @return A logical scalar. If `FALSE`, it includes a `msg` attribute detailing
#'   all reasons for validation failure.
#' @details This function performs a comprehensive check to ensure an object
#'   conforms to the `cells` structure. A valid `cells` object has the following
#'   properties:
#'   \itemize{
#'     \item It is a data.frame that conforms to the `rc_df` format.
#'     \item It has required character columns: `row`, `col`, `data_type`, `value`.
#'     \item The `data_type` column must only contain values from core_cell_recognized_format().
#'     \item The `value` column must always be a character string.
#'     \item Optional `type` column: If present, must also only contain values from core_cell_recognized_format().
#'     \item If `type` is present, `PoA` and `PoV` columns must also be present and valid.
#'     \item Optional `sheet` column: If present, must be atomic, non-NA, and contain only one unique value.
#'   }
#' @keywords internal
core_validate_cells <- function(dat) {
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

    # 3. Required columns must exist
    required_cols <- c("row", "col", "data_type", "value")
    missing_cols <- setdiff(required_cols, names(dat))
    check(
      length(missing_cols) == 0,
      paste("Missing required columns:", paste(missing_cols, collapse = ", ")))

    # 4. Validate rc_df structure
    rc_chk <- core_is_rcdf(dat)
    if (!isTRUE(rc_chk)) {
      check(FALSE, c("Not a valid rc_df structure.", attr(rc_chk, "msg")))
    }

    recognized_formats <- core_cell_recognized_format()

    # 5. Column type and value checks
    if (utils::hasName(dat, "data_type")) {
      check(is.character(dat$data_type), "'data_type' must be character.")
      unrecognized <- setdiff(unique(dat$data_type), recognized_formats)
      check(
        length(unrecognized) == 0,
        paste("Unrecognized 'data_type' formats found (some are):", paste(utils::head(unrecognized, 5), collapse = ", "))
      )
    }

    if (utils::hasName(dat, "value")) {
      check(is.character(dat$value), "'value' must be character.")
    }

    if (utils::hasName(dat, "type")) {
      check(is.character(dat$type), "'type' must be character.")
      unrecognized <- setdiff(unique(dat$type), recognized_formats)
      check(
        length(unrecognized) == 0,
        paste("Unrecognized 'type' formats found (some are):", paste(utils::head(unrecognized, 5), collapse = ", "))
      )

      # If 'type' exists, PoA and PoV must exist and be valid
      has_poa <- utils::hasName(dat, "PoA")
      has_pov <- utils::hasName(dat, "PoV")
      check(has_poa, "'PoA' column must be present when 'type' column exists.")
      check(has_pov, "'PoV' column must be present when 'type' column exists.")

      if (has_poa) {
        check(is.numeric(dat$PoA), "'PoA' must be numeric.")
        check(all(dat$PoA >= 0, na.rm = TRUE), "'PoA' values must be >= 0.")
      }
      if (has_pov) {
        check(is.numeric(dat$PoV), "'PoV' must be numeric.")
        check(all(dat$PoV >= 0, na.rm = TRUE), "'PoV' values must be >= 0.")
      }
      if (has_poa && has_pov && is.numeric(dat$PoA) && is.numeric(dat$PoV)) {
        check(all(dat$PoA + dat$PoV <= 1, na.rm = TRUE), "The sum of 'PoA' and 'PoV' must be <= 1 for each cell.")
      }
    }

    if (utils::hasName(dat, "sheet")) {
      check(is.atomic(dat$sheet) && (is.character(dat$sheet) || is.numeric(dat$sheet)),
            "'sheet' must be atomic and either character/numeric.")
      check(!anyNA(dat$sheet), "'sheet' contains NA values.")
      check(length(unique(dat$sheet)) <= 1, "'sheet' must contain only one unique value.")
    }
  }

  result <- length(issues_env$log) == 0
  if (!result) attr(result, "msg") <- unique(issues_env$log)
  return(result)
}

#' Validate a `cells` Object and Optionally Halt Execution
#'
#' This function checks if the provided `cells` object is valid according to the
#' `core_validate_cells()` function. If the validation fails, it prints error
#' messages and can halt execution if the `fail` argument is set to `TRUE`.
#' Ideally this function should be used everywhere apart from this file
#'
#' @param dat An R object to validate against the `cells` definition.
#' @return An logical value indicating whether the validation passed
#' @keywords internal
core_cells_validation_end_use <- function(dat) {

  chk <- core_validate_cells(dat)

  if(isFALSE(chk)){
    msg <- cli_bold_red(paste0("Malformed ",cli_blue("Cells")," Object!"))
    msg <- c(
      msg,
      paste0(cli_red(cli_bullet()),"Errors:"),
      paste0(cli_red(cli_cross()), " ", (attr(chk, "msg")))
    )
    cat(paste0(paste0(msg, collapse = "\n"), "\n"))
  }

  return(invisible(chk))

}

#' Internal Generic for Checking if an Object is a Valid `rc_df`
#' @keywords internal
core_is_rcdf <- function(d) {
  UseMethod("core_is_rcdf")
}

#' @noRd
#' @export
core_is_rcdf.data.frame <- function(d) {
  issues <- character(0)

  # Check 1: Column existence
  has_row <- utils::hasName(d, "row")
  has_col <- utils::hasName(d, "col")

  if (!has_row) issues <- c(issues, "Missing 'row' column")
  if (!has_col) issues <- c(issues, "Missing 'col' column")

  # Proceed with further checks only if both columns exist
  if (has_row && has_col) {

    # Suppress warnings from coercing non-numeric values to NA
    d$row <- suppressWarnings(as.integer(d$row))
    d$col <- suppressWarnings(as.integer(d$col))

    # Check 2: Coercion and data integrity issues
    if (anyNA(d$row)) issues <- c(issues, "NA values or non-integer values found in 'row'")
    if (anyNA(d$col)) issues <- c(issues, "NA values or non-integer values found in 'col'")

    # These checks are only meaningful if the columns are valid integers
    if (!anyNA(d$row)) {
      if (any(d$row <= 0)) issues <- c(issues, "'row' must contain positive values")
    }
    if (!anyNA(d$col)) {
      if (any(d$col <= 0)) issues <- c(issues, "'col' must contain positive values")
    }

    # Check 3: Uniqueness of (row, col) pairs
    if (anyDuplicated(d[, c("row", "col")])) {
      issues <- c(issues, "Duplicate (row, col) pairs found")
    }
  }

  # The final decision is based on whether any issues were found
  result <- length(issues) == 0
  if (!result) {
    attr(result, "msg") <- issues
  }

  return(result)
}

#' @noRd
#' @export
core_is_rcdf.matrix <- function(d) {
  core_is_rcdf.data.frame(as.data.frame(d))
}

#' Convert a `cells` Object to a `rc_df` Format
#'
#' This is basically tibble with only row and col columns.
#'
#' @keywords internal
core_as_rc_df <- function(x) {
  tibble::as_tibble(unclass(x))[, c("row", "col")] %>% dplyr::distinct()
}

#' Validate a `sheets` Object
#'
#' @description This internal helper function performs a comprehensive check to
#' ensure an object conforms to the `sheets` structure definition.
#'
#' @param lst An R object to validate against the `sheets` definition.
#'
#' @return A logical scalar (`TRUE` or `FALSE`). If `FALSE`, it includes a `msg`
#'   attribute containing a character vector that details all validation
#'   failures.
#'
#' @details A valid `sheets` object must satisfy all the following conditions:
#' \itemize{
#'   \item It **must** be a `list`.
#'   \item The list **must** have names.
#'   \item All names **must** be unique and non-empty.
#'   \item Each element within the list **must** be a valid `cells` object (i.e., it must pass `core_validate_cells()`).
#' }
#'
#' @keywords internal
core_validate_sheets <- function(lst) {
  issues_env <- new.env(parent = emptyenv())
  issues_env$log <- character(0)

  check <- function(cond, msg) {
    if (!isTRUE(cond)) {
      issues_env$log <- c(issues_env$log, msg)
    }
  }

  # 1. Must be a list
  check(is.list(lst), "Input must be a list.")

  if (is.list(lst)) {

    # 2. List should be named and all nodes must have unique names
    node_names <- names(lst)
    check(!is.null(node_names), "List must be named.")
    check(!anyDuplicated(node_names), "List node-names must be unique.")


    # 3. Each element must be a valid cells object

    purrr::iwalk(lst, function(cell_obj, idx) {

      label <- paste0("Node - ", idx)

      # 2. Each element must be a valid cells
      valid <- core_validate_cells(cell_obj)
      if (!isTRUE(valid)) {
        check(FALSE,
              paste0(label, " - failed validation: ",
                     paste0(attr(valid, "msg"), collapse = "; ")))
      }
    })
  }

  result <- length(issues_env$log) == 0
  if (!result) attr(result, "msg") <- unique(issues_env$log)
  return(result)
}

core_ensure_cells_format <- function(dat) {
  val <- core_validate_cells(dat)

  if (!val) {
    rlang::abort(paste0(attr(val, "msg"), collapse = "\n"), call = NULL)
  }

  if (!is.integer(dat$row)) {
    dat$row <- as.integer(dat$row)
  }

  if (!is.integer(dat$col)) {
    dat$col <- as.integer(dat$col)
  }

  # Remove any induced intermediate classes
  cls <- class(dat)
  cls <- setdiff(
    cls,
    c(core_cells_class_internal, core_cells_class, core_sheets_class)
  ) %>%
    unique()

  # Add core cells class
  class(dat) <- c(core_cells_class, cls)

  # Ensure the data is sorted by row and column
  dat <- dplyr::arrange(
    dat,
    .data$row,
    .data$col
  )

  dat
}

core_ensure_sheets_format <- function(lst){
  val <- core_validate_sheets(lst)

  if (!val) {
    rlang::abort(paste0(attr(val, "msg"), collapse = "\n"), call = NULL)
  }

  # Remove any induced intermediate classes
  cls <- class(lst)
  cls <- setdiff(
    cls,
    c(core_cells_class_internal, core_cells_class, core_sheets_class)
  ) %>%
    unique()

  # Add core sheets class
  class(lst) <- c(core_sheets_class, cls)

  lst
}

# --- Conversion Methods on `cells` and `sheets` class ---
# These are kept in core_classes_helpers.R


# Section: Cells/Sheet Analysis Class Methods ----

