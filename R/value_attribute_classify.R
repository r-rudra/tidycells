

#' Value/Attribute Classifier
#'
#' @description After [`as_cell_df`][as_cell_df()] (entry  point to `tidycells`) you may need to use this function
#' or individual _Value/Attribute Classifier_-functions as listed below in _"see also"_ - section.
#'
#' Here the idea is to classify all cells into either `value`, `attribute`, `empty` which will be
#' used by [`analyze_cells`][analyze_cells()] for further processing.
#'
#' @param d a Cell DF
#' @param classifier a classifier
#'
#' @return a Cell DF with Value/Attribute Classification. The underlying tibble will contain an extra column named `type`.
#' @export
#' @details In order to understand the data orientation and detect data-blocks Cell DF requires additional column named `type`.
#' This `type` column potentially contains either `value`, `attribute`, `empty`. The `value` are given corresponding to cells
#' with observations in it. The tag, `attribute` is for the identifier of these cells. Lastly, `empty` cells are useless cells or
#' cells with no meaningful information.
#'
#' For `classifier` following options are present:
#' * `basic_classifier` : naive classifier which recode `data_type`.
#' * `sample_based_classifier` : sample-based classifier.
#' * `numeric_values_classifier` : considers number like cells as values.
#'
#' Each of the above are available as individual functions. Those can also be directly applied on a `cell-df`.
#' However, it is recommended to use `value_attribute_classify` as it tests for integrity after classification.
#'
#' @examples
#'
#' iris %>%
#'   as_cell_df() %>%
#'   sample_based_classifier(value_sample = "setosa") %>%
#'   plot()
#'
#' iris %>%
#'   as_cell_df() %>%
#'   sample_based_classifier(value_sample = "setosa") %>%
#'   numeric_values_classifier() %>%
#'   plot()
#'
#' if (rlang::is_installed("tidyxl")) {
#'   cdn <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") %>%
#'     tidyxl::xlsx_cells()
#'   cdn <- cdn %>%
#'     dplyr::filter(sheet == sheet[1]) %>%
#'     as_cell_df()
#'
#'   # all of these are same except value_attribute_classify will perform validate_cells once again
#'   cd1 <- sample_based_classifier(cdn, value_sample = "APR")
#'   cd2 <- sample_based_classifier(value_sample = "APR")(cdn)
#'   cd3 <- value_attribute_classify(cdn,
#'     classifier = sample_based_classifier(value_sample = "APR")
#'   )
#'   # see it
#'   plot(cd3)
#' }
#' @seealso
#' Individual classifier functions:
#' * [basic_classifier][basic_classifier()]
#' * [sample_based_classifier][sample_based_classifier()]
#' * [numeric_values_classifier][numeric_values_classifier()],
#'
#' For interactive Value/Attribute Classification check [visual_va_classify][visual_va_classify()]
#'
value_attribute_classify <- function(d, classifier = basic_classifier()) {
  if (!is_cell_df(d)) {
    abort("Cell DF Expected")
  }

  classifier_try <- try(purrr::as_mapper(classifier), silent = TRUE)
  if (!inherits(classifier_try, "try-error")) {
    classifier <- classifier_try
  }

  if (!is.function(classifier)) {
    abort("classifier is not a function")
  }

  cd <- classifier(d)

  chk <- validate_cells(cd)

  if (!chk) {
    abort(paste0(attr(chk, "msg"), collapse = "\n"))
  }

  cd
}
