
#' Transform data into Cell-DF Structure
#'
#' @param d the data (either a matrix with column name or a data.frame)
#' @param take_row_names consider row names as separate cells
#' (applicable only for data with no (row, col) information). Default is \code{FALSE}.
#' @param take_col_names consider column names as separate cells
#' (applicable only for data with no (row, col) information). Default is \code{FALSE}.
#'
#' @return A object of class [`cell_df`][cell_df-class].
#' @export
#' @rdname as_cell_df
#'
#' @examples
#'
#' as_cell_df(iris)
#'
#' # consider column name as cell
#' as_cell_df(iris, take_col_names = TRUE)
#'
#' # if the data is already in similar format it will not further transform
#' unpivotr::as_cells(iris) %>% as_cell_df()
as_cell_df <- function(d, take_row_names = FALSE, take_col_names = FALSE) {
  UseMethod("as_cell_df")
}

#' @export
as_cell_df.data.frame <- function(d, ...) {
  d %>%
    attach_intermediate_class() %>%
    as_cell_df_internal(...)
}

#' @export
as_cell_df.matrix <- function(d, ...) {
  d %>%
    as.data.frame() %>%
    attach_intermediate_class() %>%
    as_cell_df_internal(...)
}
