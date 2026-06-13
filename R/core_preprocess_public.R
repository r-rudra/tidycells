# Value and Attribute classification

#' Value and Attribute classification
#'
#' *Classify Cells as Attribute or Value in a `cells` Object:*
#'
#' This function assigns to each cell in a `cells` object a classification of
#' "attribute" (such as a header or label) or "value" (such as a data entry),
#' using configurable heuristics or probabilistic logic. It updates or adds the
#' columns `type`, `PoA` (probability of attribute), and `PoV` (probability of
#' value) to the input object. This supports semantic and tidy table analysis.
#'
#' @section Classification Logic: The classification is based on cell content
#'   (`value` column) and its original type (`data_type` column). The columns
#'   added or updated are:
#' \itemize{
#'   \item `type`: The detected or refined type of the cell, one of `"numeric"`,
#'   `"character"`, `"logical"`, `"categorical"`, `"date"`, `"time"`, or
#'   `"blank"`.
#'   \item `PoA`: Probability (between 0 and 1) that the cell is an attribute
#'   (header/label).
#'   \item `PoV`: Probability (between 0 and 1) that the cell is a value (data
#'   entry).
#' }
#'   It is always true that `PoA + PoV <= 1` for any cell.
#'
#' @param x A `cells` object to be classified.
#' @param method Character; one of `"auto"`, `"probabilistic"`,
#'   `"simple_heuristic"`, or `"manual"`. Controls which classification method
#'   is used:
#'   \describe{
#'     \item{`"auto"`}{Selects the method automatically based on the size of `x`
#'     and presence of manual tags. (default)}
#'     \item{`"probabilistic"`}{Uses probabilistic inference to determine
#'     attribute/value likelihoods, especially for ambiguous types. More
#'     accurate but slower for large data.}
#'     \item{`"simple_heuristic"`}{Uses fast, rule-based logic. Suitable for
#'     large tables, less nuanced.}
#'     \item{`"manual"`}{Uses human-provided tags in `av_class_tag` if present.}
#'   }
#'
#' @return The original `cells` object, with the columns `type`, `PoA`, and
#'   `PoV` added or updated.
#'
#' @details The function is essential for semantic table analysis and structure
#'   detection. Probabilities and type assignments can be further customized
#'   after classification. For more on the `cells` object format, see
#'   [cells-class].
#'
#' @seealso [cells-class]
#' @export
value_attribute_classify <- function(
    x,
    # Later visual method is to be added.
    method = c("auto","probabilistic", "simple_heuristic", "manual")) {

  # Check and halt if the input is not a valid cells object. Otherwise, proceed
  # with the classification
  if(isFALSE(core_cells_validation_end_use(x))){
    rlang::abort("Malformed Cells - Halted Execution! Please fix the issues.")
  }

  method <- match.arg(method)

  # If the method is "auto", use best available method based on the size and
  # whether av_class_tag is present then directly use manual
  if(method == "auto") {
    if(utils::hasName(x, "av_class_tag")) {
      # If av_class_tag is present, use manual method as it is the most accurate
      # as av_class_tag is supposed to be manually tagged and it is not supposed
      # to be used for probabilistic or heuristic methods.
      method <- "manual"
    } else if(NROW(x) < 30000) {
      # If the number of rows is less than 30,000, use probabilistic method as
      # it has TPS of 27k per sec (around 1 sec for 30k rows)
      method <- "probabilistic"
    } else {
      # Quite large, use simple heuristic as its really fast
      method <- "simple_heuristic"
    }
  }


  if(method == "probabilistic") {
    x <- core_prep_va_heuristic_probabilistic(x)
  } else if(method == "simple_heuristic") {
    x <- core_prep_va_simple_heuristic(x)
  } else if(method == "manual") {
    x <- core_prep_va_manual(x)
  }

  return(x)
}
