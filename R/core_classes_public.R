
#' Coerce an Object into a `cells` Object
#'
#' @description
#' This S3 generic function standardizes various R objects into a `cells` object.
#'
#' A `cells` object is a "tidy" representation of a single sheet's data, where
#' each row describes one cell's content and metadata (its position, data type,
#' and value). This function acts as a robust adapter to convert common data
#' structures into this single, well-defined format.
#'
#' @details
#' `as_cells()` is designed to handle multiple data formats gracefully:
#' \itemize{
#'   \item **Rectangular Data:** Standard `data.frame` and `matrix` objects are
#'   "unpivoted" from their wide format into the long `cells` structure.
#'
#'   \item **Specialized Tidy Formats:** It can efficiently adapt outputs from
#'   other data-import and reshaping packages. The primary purpose is to create a
#'   common structure from objects created by:
#'   \itemize{
#'     \item \code{\link[tidyxl]{xlsx_cells}} from the \pkg{tidyxl} package.
#'     \item \code{\link[unpivotr]{as_cells}} from the \pkg{unpivotr} package.
#'     \item The melted `data.frame` output from the \pkg{meltr} package.
#'   }
#' }
#'
#' @param x The R object to transform (e.g., `data.frame`, `matrix`, or outputs
#'   from packages like `tidyxl`).
#' @param ... Additional arguments passed on to specific methods.
#'
#' @return An object of class `cells`.
#'
#' @seealso [as_sheets()] for handling multiple sheets.
#'   \code{\link[tidyxl]{xlsx_cells}}, \code{\link[unpivotr]{as_cells}}.
#'
#' @export
#' @examples
#' # Coerce a simple data frame into the 'cells' format
#' df <- data.frame(
#'   ID = c(101, 102),
#'   Score = c(95.5, 89.0),
#'   Grade = c("A", "B"),
#'   stringsAsFactors = FALSE
#' )
#'
#' cells_data <- as_cells(df)
as_cells <- function(x, ...){
  UseMethod("as_cells")
}

#' @export
as_cells.cells <- function(x, ...) {
  # If the object is already a 'cells' object, just re-check its format.
  core_ensure_cells_format(x)
}

#' @export
as_cells.data.frame <- function(x, ...) {
  # Main entry point for data frames.
  core_do_as_cells_internal(x, ...)
}

#' @export
as_cells.matrix <- function(x, ...) {
  # For matrices, first convert to a data frame, then process.
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  as_cells(x, ...)
}


#' Transform Data into a Standard 'sheets' Structure
#'
#' @description This S3 generic function transforms various R objects into a
#'   standardized `sheets` object. A `sheets` object is a named list of `cells`
#'   objects, each representing a single sheet (e.g., as in an Excel workbook).
#'
#'   This function handles three common input types:
#' \enumerate{
#'   \item A named or unnamed list where each element is a data.frame, matrix,
#'         or similar tabular object. Each element is treated as one sheet.
#'   \item A single "long" data.frame that contains a `sheet` column. The data is
#'         split by that column, and each subset becomes a separate sheet.
#'   \item A single data.frame, matrix, or `cells` object, which is wrapped into
#'         a one-element `sheets` list with default name `"sheet_default"`.
#' }
#'
#'   Each sheet (node) is internally passed through [as_cells()] to standardize
#'   it as a `cells` object. This ensures that the final result is a uniquely
#'   named list of valid `cells` objects — forming the `sheets` structure.
#'
#' @param x The R object to transform (list, data.frame, matrix, or `cells`).
#'
#' @return An object of class `sheets`, which is a uniquely named list of
#'   `cells` objects.
#'
#' @seealso [as_cells()]
#'
#' @export
#'
#' @examples
#' # Case 1: List of data.frames
#' sheets1 <- as_sheets(list(sheet1 = iris, sheet2 = mtcars))
#'
#' # Case 2: Long data.frame with a 'sheet' column
#' long_df <- data.frame(
#'   sheet = rep(c("A", "B"), each = 3),
#'   row = 1:6, col = 1,
#'   data_type = "character",
#'   value = letters[1:6]
#' )
#'
#' sheets2 <- as_sheets(long_df)
#'
#' # Case 3: Single data.frame as one sheet
#' sheets3 <- as_sheets(mtcars)
as_sheets <- function(x){
  UseMethod("as_sheets")
}

#' @export
as_sheets.cells <- function(x) {
  # If the object is already a 'cells' object, just re-check its format assuming
  # a single sheet (named "sheet_default")
  core_ensure_sheets_format(list(sheet_default = x))
}

#' @export
as_sheets.sheets <- function(x) {
  # If the object is already a 'sheets' object, just re-check its format.
  core_ensure_sheets_format(x)
}

#' @export
as_sheets.data.frame <- function(x) {
  core_do_as_sheets_internal(x)
}

#' @export
as_sheets.list <- function(x) {
  core_do_as_sheets_internal(x)
}
