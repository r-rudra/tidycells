
#' `cell_analysis` class
#'
#' @description
#' The `cell_analysis` class is based on list,
#' created in order to store analysis of cell level information ([`cell_df`][cell_df-class]).
#'
#' @section Properties of `cell_analysis`:
#'
#' Objects of class `cell_analysis` have following named nodes:
#' * `cells` : Contains information about `cell_group_type` in terms of (data, minor and major attributes).(a tibble)
#' * `sections` : Contains boundaries of each data block. (a tibble)
#' * `details` : a list containing further information
#' * `cell_df` : The original cell_df which is passed for processing
#'
#' @section Applicable methods on `cell_analysis`:
#' * `print`: Prints identifier of the class and the number of blocks (and potential issues if any).
#' * `plot`: Plots (using [`ggplot2`][ggplot2::ggplot()]) the data-block information.
#'
#' @name cell_analysis-class
NULL

#' @exportClass cell_analysis
cell_df_analysis_class <- c("cell_analysis", "cell_df_analysis", "list")
setOldClass(cell_df_analysis_class)
