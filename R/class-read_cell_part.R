
#' `read_cell_part` class
#'
#' @description
#' The `read_cell_part` class is based on list,
#' created in order to store (possible) intermediate [`read_cells`][read_cells()] output in detailed format.
#'
#' **Note:** This class is designed mainly for _internal use_.
#' It is recommended to manipulate objects of this class only for cases where desired output is not coming.
#'
#' @section Properties of `read_cell_part`:
#'
#' Objects of class `read_cell_part` **may** have following named nodes (the nodes may change based on stage):
#' * `file_name` : file name which was given to process
#' * `stage` : stage at which it was last processed.
#' * `info` : a list containing further information on type and content of the file (provided the file got read)
#' * `is_empty` : whether the file contains no 'known tabular information'
#' * `cell_list` : list of [`cell_df`][cell_df-class]
#' (possibly after `Value Attribute Classification` done if stage is higher than `make_cells`)
#' * `cell_analysis_list` : list of [`cell_analysis`][cell_analysis-class]
#' * `final_composition` : final composition (a `tibble`) with only main columns
#' (or all column if `compose_main_cols_only = FALSE`)
#' * `final_composition_main` : only appear if `compose_main_cols_only = FALSE`.
#' This holds final composition (a `tibble`) with only main columns
#'
#' @section Applicable methods on `read_cell_part`:
#' * `print`: Prints identifier of the class and the stage at which it is last processed.
#'
#' @name read_cell_part-class
#' @keywords internal
NULL

#' @exportClass read_cell_part
read_cell_part_class <- c("read_cell_part", "list")
setOldClass(read_cell_part_class)
