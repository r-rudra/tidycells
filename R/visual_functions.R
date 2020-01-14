

#' Interactive operations and Visualizations
#'
#' @param d a cell_df.
#' @description These functions require \code{\link[shiny:shiny-package]{shiny}} and `miniUI`.
#' Each of these modules has option for interactive plots (using `plotly`).
#' However, for the same you need to have `plotly` installed.
#'
#' @details Each of these functions are designed for interactive sessions only.
#' Arguments to these functions are optional as it tries to pick those from global environment.
#' If there are multiple variables defined in global environment which are of same desired class then you will get option to choose
#' one of them. These are also available as \href{https://rstudio.github.io/rstudioaddins/}{RStudio Addin}
#' (if you are using RStudio IDE you'll get those addin in the RStudio addin panel).
#'
#' Following are the names used in RStudio Addin
#' * _Crop Cells_: `visual_crop`
#' * _Cell Analysis Inspection_: `visual_data_block_inspection`
#' * _Orientation Modification_: `visual_orientation_modification`
#' * _Traceback Composition_: `visual_traceback`
#' * _Classify Value/Attribute_: `visual_va_classify`
#'
#' For each of the modules it tries to load and unload `shiny` before and after the execution of the module.
#' You can disable the same by setting `options(AutoUnloadShiny = FALSE)`.
#'
#' @return `visual_crop` returns a cell_df after the interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
visual_crop <- function(d) {
  if (missing(d)) {
    d <- global_object_picker("cell_df")
  }

  shiny_check()
  dout <- shiny_app_crop(d)
  shiny_unload()

  return(invisible(dout))
}

#' @return `visual_va_classify` returns a cell_df after the interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
#' @seealso [`value_attribute_classify`][value_attribute_classify()]
visual_va_classify <- function(d) {
  if (missing(d)) {
    d <- global_object_picker("cell_df")
  }

  shiny_check()
  dout <- shiny_app_va_classify(d)
  shiny_unload()

  return(invisible(dout))
}

#' @param x a cell_analysis.
#' @export
#' @keywords internal
#' @rdname visual_functions
visual_data_block_inspection <- function(x) {
  if (missing(x)) {
    x <- global_object_picker("cell_analysis")
  }

  shiny_check()
  dout <- shiny_app_data_block(x)
  shiny_unload()

  return(invisible(dout))
}

#' @return `visual_orientation_modification` returns a cell_analysis after the interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
visual_orientation_modification <- function(x) {
  if (missing(x)) {
    x <- global_object_picker("cell_analysis")
  }

  shiny_check()
  this_out <- shiny_app_orientation_modification(x)
  shiny_unload()

  return(invisible(this_out))
}

#' @param dcomp a composition (a tibble rendered by [`compose_cells`][compose_cells()]).
#' @return `visual_traceback` returns a composed tidy data (as a tibble).
#' @details `visual_traceback` requires package `DT`.
#' If `DT` is not present you may use [`cell_composition_traceback`][cell_composition_traceback()]
#'
#' @export
#' @keywords internal
#' @rdname visual_functions
#' @seealso [`cell_composition_traceback`][cell_composition_traceback()]
visual_traceback <- function(x, dcomp) {
  if (!DT_present()) {
    abort("package `DT` is required")
  }

  if (missing(x)) {
    x <- global_object_picker("cell_analysis")
  }

  shiny_check()
  this_out <- shiny_app_traceback(x, dcomp)
  shiny_unload()

  return(invisible(this_out))
}
