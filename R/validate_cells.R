#' Validate cell-DF
#'
#' @param dat An R object
#'
#' @return Logical scalar. If the value is `FALSE`, it will contain attribute named `msg`.
#' @details It checks for following facts:
#' * Whether `dat` is a `data.frame`
#' * Whether `dat` conforms to `rc_df` format
#' * Whether `dat` conforms to `cell_df` format
#' If all the checks are passed then it returns `TRUE` else it returns `FALSE` with `msg` attribute, indicating reason for validation failure.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' # returns TRUE
#' validate_cells(tibble::tibble(row = 1, col = 2, data_type = "numeric", value = "1"))
#'
#' # this is FALSE
#' chk <- validate_cells(tibble::tibble(row = 1, col = 2, data_type = "numeric"))
#'
#' # msg
#' attr(chk, "msg")
validate_cells <- function(dat) {
  msg <- character(0)

  if (!inherits(dat, "data.frame")) {
    msg <- msg %>% c("data.frame expected")
  }

  if (!is_conforms_to_rcdf(dat)) {
    msg <- msg %>% c("does not conforms to rc_df format")
  }

  if (!all(hasName(dat, c("row", "col", "data_type", "value")))) {
    msg <- msg %>% c("does not has required columns")
  }

  # optional test if the dat is VA classified
  if (hasName(dat, "type")) {
    dtyl <- dat$type %>%
      setdiff(c("value", "attribute", "empty")) %>%
      length()
    if (dtyl != 0) {
      msg <- msg %>% c("type column present but content differs from value, attribute, empty")
    }
  }

  if (hasName(dat, "data_type")) {
    dtl <- dat$data_type %>%
      setdiff(c("numeric", "character")) %>%
      length()

    if (dtl != 0) {
      msg <- msg %>% c("data_type has fields apart from numeric, character")
    }

    if (!rlang::is_atomic(dat$data_type)) {
      msg <- msg %>% c("data_type is not atomic")
    } else {
      if (!is.character(dat$data_type)) {
        msg <- msg %>% c("data_type must be character")
      }
    }
  }


  if (hasName(dat, "value")) {
    if (!rlang::is_atomic(dat$value)) {
      msg <- msg %>% c("value is not atomic")
    } else {
      if (!is.character(dat$value)) {
        msg <- msg %>% c("value must be character")
      }
    }
  }


  if (length(msg) == 0) {
    TRUE
  } else {
    res <- FALSE
    attr(res, "msg") <- msg
    res
  }
}
