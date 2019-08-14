
cell_df_class <- c("cell_df", "cells", "rc_df", "tbl_df", "tbl", "data.frame")

cell_df_class_internal <- c("cell_df", "cells", "rc_df", "tidyxl", "unpivotr", "readr", "unknown")

#' @importFrom methods setOldClass
#' @exportClass cell_df

setOldClass(cell_df_class)

#' `cell_df` class
#'
#' @description
#' The `cell_df` class is a subclass of [`tbl_df`][tibble::tibble()] and [`data.frame`][base::data.frame()],
#' created in order to store cell level information.
#'
#' @section Properties of `cell_df`:
#'
#' Objects of class `cell_df` have:
#' * A `class` attribute of `c("cell_df", "cells", "rc_df", "tbl_df", "tbl", "data.frame")`.
#' * Two column of `integer` type named `row` and `col`. All entries must be positive and not `NA`.
#'   This represents a cell address.
#' * A column of `character` type named `data_type`. Which can contain only `numeric` or `character` as entries.
#'   This represents the data type of the cell (classified to only two categories).
#' * A column of `character` type named `value`. This stores value of corresponding cells.
#' * Apart from these columns it can contain other columns.
#'   However, (`row`, `col`) together should identify the row of the `cell_df` uniquely.
#'
#' @section Applicable methods on `cell_df`:
#' * `print`: Prints identifier of the class and the content of the underlying tibble.
#' * `summary`: Calculates basic stats like number of rows and columns, number of characters and numeric fields, density etc.
#' * `plot`: Plots (using [`ggplot2`][ggplot2::ggplot()]) the cell information as ordinary table.
#' * `as.matrix`: This will transform the data back into tabular form and create a character matrix.
#' * `as.data.frame`: Similar to `as.matrix` this will also transform the data back into tabular form.
#'
#' @seealso
#'
#' The function [`validate_cells`][validate_cells()] which is used to validate `cell_df`.
#'
#' The `as.matrix` and `as.data.frame` method is similar to [`unpivotr::rectify`][unpivotr::rectify()].
#'
#' Object of this class is also compatible to most of the functions from \code{\link[unpivotr:unpivotr-package]{unpivotr}} package.
#'
#' @name cell_df-class
#' @keywords internal
NULL


new_cell_df <- function(dat) {
  val <- validate_cells(dat)

  if (!val) {
    abort(paste0(attr(val, "msg"), collapse = "\n"))
  }

  if (!is.integer(dat$row)) {
    dat <- dat %>% mutate(row = as.integer(row))
  }

  if (!is.integer(dat$col)) {
    dat <- dat %>% mutate(col = as.integer(col))
  }

  # if all above passes

  # unclass cell_df induces class
  dat <- unset_cell_df_class(dat)

  # re-attach cell_df class
  class(dat) <- class(dat) %>%
    c(cell_df_class, .) %>%
    unique()



  dat
}

unset_cell_df_class <- function(dat) {
  class(dat) <- class(dat) %>%
    setdiff(cell_df_class_internal) %>%
    unique()
  dat
}

is_cell_df <- function(d) {
  inherits(d, "cell_df")
}
