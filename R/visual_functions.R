#' Interactive operations and Visualizations
#'
#' @param d a cells object.
#' @description These functions require \code{\link[shiny:shiny-package]{shiny}}
#'   and `miniUI`.
#'
#' @details Each of these functions are designed for interactive sessions only.
#'   Arguments to these functions are optional as it tries to pick those from
#'   global environment. If there are multiple variables defined in global
#'   environment which are of same desired class then you will get option to
#'   choose one of them. These are also available as
#'   \href{https://rstudio.github.io/rstudioaddins/}{RStudio Addin} (if you are
#'   using RStudio IDE you'll get those addin in the RStudio addin panel).
#'
#'   Following are the names used in RStudio Addin
#' * _Orientation Modification_: `visual_orientation_modification`
#' * _Traceback Composition_: `visual_traceback`
#' * _Classify Value/Attribute_: `visual_value_attribute_classify`
#'
#'   For each of the modules it tries to load and unload/detach `shiny` before
#'   and after the execution of the module (or rather it will honour the state
#'   in which theshiny was before and after the function. Means if shiny was
#'   atatched then it will remain attahced otherwise will be detached.
#'
#' @return `visual_value_attribute_classify` returns a cell_df after the
#'   interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
#' @seealso [`value_attribute_classify`][value_attribute_classify()]
visual_value_attribute_classify <- function(d) {
  # If argument not provided, pick from global environment interactively
  if (missing(d)) {
    d <- shiny_util_global_object_picker(what = "cells")
  }
  # Launch visual classification app
  shiny_app_va_classify(d)
}

#' @param x a cells-analysis object.
#' @return `visual_orientation_modification` returns a cell_analysis after the
#'   interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
visual_orientation_modification <- function(x) {
  # If argument not provided, pick from global environment interactively
  if (missing(x)) {
    x <- shiny_util_global_object_picker("cells_analysis")
  }
  # Launch orientation modification app
  shiny_app_orientation_modification(x)
}

#' @param x a cells-analysis object.
#' @return `visual_traceback` returns a composed object after interactive operations.
#' @export
#' @keywords internal
#' @rdname visual_functions
visual_traceback <- function(x) {
  # If argument not provided, pick from global environment interactively
  if (missing(x)) {
    x <- shiny_util_global_object_picker("cells_analysis")
  }
  shiny_app_traceback(x)
}
