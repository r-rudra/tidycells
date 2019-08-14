
## This file keeps entries for multiple names of same functions
##
## analyse_cells <- analyze_cells


#' Analyse Cells
#'
#' @description Note that this is an other name for `tidycells::analyze_cells`.
#' After [`Value Attribute Classification`][value_attribute_classify()] done on a [`cell_df`][cell_df-class] next task to do is
#' analyze it's contents for data block detection, attribute orientation identification etc. The function `analyze_cells` (and also `analyse_cells`)
#' does the same for you.
#'
#' **Note**:
#' \if{html}{If you are not sure about what package functions actually do or how they work together,
#' please start with \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.}
#' \if{latex}{If you are not sure about what package functions actually do or how they work together,
#' please start with \code{vignette("tidycells-intro")}.}
#'
#' @param d A [`cell_df`][cell_df-class] after [`Value Attribute Classification`][value_attribute_classify()] done
#' @param silent logical scalar indicating whether to raise a warning if heuristic detection fails. (Default TRUE).
#'
#' @details it returns detailed analysis of the data structure including data block detection, attribute orientation detection etc.
#' The argument `silent` is set to `TRUE` by default, as the warning will be given whenever the [`cell_analysis`][cell_analysis-class] is printed.
#'
#' After this step one may like to do :
#' * [`compose_cells`][compose_cells()]
#'
#' If in an interactive session, following additional functions can be helpful for interactive visualizations:
#' * [`visual_data_block_inspection`][visual_data_block_inspection()]
#' * [`visual_orientation_modification`][visual_orientation_modification()]
#' * [`visual_traceback`][visual_traceback()]
#'
#' @return Detailed analysis of the cell data structure.
#' Which will be a [`cell_analysis`][cell_analysis-class] class object.
#'
#' @seealso [`compose_cells`][compose_cells()], [`collate_columns`][collate_columns()], [`analyze_cells`][analyze_cells()]
#' @keywords internal
#' @export
analyse_cells <- function(d, silent = TRUE) {
  analyze_cells_raw(d = d, silent = silent)
}
