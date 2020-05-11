
read_cell_task_orders <- c("detect_and_read", "make_cells", "va_classify", "analyze", "compose", "collate")



#' Read Cells from file
#'
#' @description This function is designed to read cell level information
#' (and the finally [analyze][analyze_cells()], [compose][compose_cells()] and [collate_columns][collate_columns()])
#' from many file types like xls, pdf, doc etc.
#' This is a wrapper function to functions from multiple packages. The support for a specific file is dependent on
#' the installed packages. To see the list of supported files and potentially required packages (if any) just
#' run `read_cells()` in the console. This function supports the file format based on content and not based on just the file
#' extension. That means if a file is saved as pdf and then the extension is removed (or extension modified to say `.xlsx`)
#' then also the `read_cells` will detect it as pdf and read its content.
#'
#' **Note** :
#'
#' * `read_cells` is supposed to work for any kind of data. However, if it fails in intermediate stage it will raise
#' a warning and give results till successfully processed stage.
#' * The heuristic-algorithm are not well-optimized (yet) so may be slow on large files.
#' * If the target table has numerical values as data and text as their attribute (identifier of the data elements),
#' straight forward method is sufficient in the majority of situations. Otherwise, you may need to utilize other functions.
#'
#' **A Word of Warning** :
#'
#' _The functions used inside `read_cells` are heuristic-algorithm based. Thus, outcomes may be unexpected.
#' It is recommend to try `read_cells` on the target file. If the outcome is expected., it is fine.
#' If not try again with `read_cells(file_name, at_level = "compose")`. If after that also the output is not as expected
#' then other functions are required to be used. At that time start again with `read_cells(file_name, at_level = "make_cells")`
#' and proceed to further functions._
#'
#'
#' @param x either a valid file path or a [`read_cell_part`][read_cell_part-class]
#' @param at_level till which level to process.
#' Should be one of `detect_and_read`, `make_cells`, `va_classify`, `analyze`, `compose`, `collate`.
#' Or simply a number (like 1 means `detect_and_read`, 5 means `compose`).
#' @param omit (Optional) the file-types to omit. A character vector.
#' @param simplify whether to simplify the output. (Default `TRUE`). If set to `FALSE` a [`read_cell_part`][read_cell_part-class]
#' will be returned.
#' @param compose_main_cols_only whether to compose main columns only. (Default `TRUE`).
#' @param from_level (Optional) override start level. (`read_cells` will process after `from_level`)
#' @param silent if `TRUE` no message will be displayed.(Default `TRUE`)
#' @param ... further arguments
#'
#' @return If `simplify=TRUE` then different kind of object is returned in different levels (depends on `at_level`).
#' If `at_level="compose"` then only final tibble is returned otherwise if the output is not `NULL` an attribute will be present
#' named `"read_cells_stage"`.
#'
#' If `simplify=FALSE` then it will return a [`read_cell_part`][read_cell_part-class] which you can process manually
#' and continue again with `read_cells` (perhaps then `from_level` may be useful).
#' @export
#'
#' @details
#' It performs following set of actions if called with default `at_level`.
#'
#' * **detect_and_read**: Detect file type based on content and attempt to read the same in a format suitable to convert as [`cell_df`][cell_df-class].
#' * **make_cells**: Convert the file content to [`cell_df`][cell_df-class] using [`as_cell_df`][as_cell_df()].
#' * **va_classify**: Run [Value Attribute Classification][value_attribute_classify()] using [`numeric_values_classifier`][numeric_values_classifier()].
#' * **analyze**: Analyze the cells using [`analyze_cells`][analyze_cells()].
#' * **compose**: Compose the cell-analysis to a tidy form using [`compose_cells`][compose_cells()].
#' * **collate**: Finally, collate columns based on content using [`collate_columns`][collate_columns()].
#'
#' \if{html}{
#'
#' Here is the flowchart of the same:
#'
#' \figure{read_cells.svg}{options: width=400}
#'
#' }
#'
#' @rdname read_cells
#'
read_cells <- function(x,
                       at_level = c("collate", "detect_and_read", "make_cells", "va_classify", "analyze", "compose"),
                       omit = NULL,
                       simplify = TRUE,
                       compose_main_cols_only = TRUE,
                       from_level,
                       silent = TRUE,
                       ...) {
  UseMethod("read_cells")
}


#' Read Cells from file
#'
#' @param x either a valid file path or a [`read_cell_part`][read_cell_part-class]
#' @param at_level till which level to process.
#' Should be one of `detect_and_read`, `make_cells`, `va_classify`, `analyze`, `compose`. Or simply a number.
#' @param omit (Optional) the file-types to omit. A character vector.
#' @param simplify whether to simplify the output. (Default `TRUE`). If set to `FALSE` a [`read_cell_part`][read_cell_part-class]
#' will be returned.
#' @param compose_main_cols_only whether to compose main columns only. (Default `TRUE`).
#' @param from_level (Optional) override start level. (`read_cells` will process after `from_level`)
#' @param silent if `TRUE` no message will be displayed.(Default `TRUE`)
#' @param ... further arguments
#'
#' @seealso
#' [`read_cells`][read_cells()]
#'
#' @rdname read_cells_internal
#' @keywords internal
#' @export
read_cells.read_cell_part <- function(x,
                                      at_level = c("collate", "detect_and_read", "make_cells", "va_classify", "analyze", "compose"),
                                      omit = NULL,
                                      simplify = TRUE,
                                      compose_main_cols_only = TRUE,
                                      from_level,
                                      silent = TRUE,
                                      ...) {
  if (is.character(at_level)) {
    at_level <- match.arg(at_level)
    at_level <- which(read_cell_task_orders == at_level)
  } else {
    if (!is.numeric(at_level)) {
      abort("'at_level' has to be either a integer or character scalar")
    }
  }
  at_level <- as.integer(at_level[1])
  if (identical(at_level, NA_integer_)) {
    abort("invalid 'at_level'")
  }

  rcp <- x

  if (!missing(from_level)) {
    if (is.numeric(from_level)) {
      from_level <- as.integer(from_level)
      if (from_level >= 1 & from_level <= length(read_cell_task_orders)) {
        from_level <- read_cell_task_orders[from_level]
      }
    }
    if (is.character(from_level)) {
      if (from_level %in% read_cell_task_orders) {
        in_stage <- rcp$stage
        if (!identical(from_level, in_stage)) {
          if (!silent) {
            message("Given 'read_cell part' and 'from_level' have different level. Taking 'from_level'.")
          }
        }
        rcp$stage <- from_level
      }
    }
  }

  out_l <- rcp
  simple <- out_l$file_name
  # if it is in last stage
  if (out_l$stage == read_cell_task_orders[6]) {
    if (is.data.frame(out_l$final)) {
      simple <- out_l$final
    }
  }

  this_level <- which(read_cell_task_orders == out_l$stage)
  if (length(this_level) < 1) {
    this_level <- 0
  }

  if (length(this_level) > 1) {
    abort("multiple stages in the 'read_cell part'")
  }

  ran_till <- "init"
  ok_so_far <- TRUE

  # detect_and_read
  if (ok_so_far) {
    stn <- run_stage_safe(do_detect_and_read(at_level, this_level, out_l, simplify, simple, omit))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[1]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  # make_cells
  if (ok_so_far) {
    stn <- run_stage_safe(do_make_cells(at_level, this_level, out_l, simplify, simple))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[2]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  # va_classify
  if (ok_so_far) {
    stn <- run_stage_safe(do_va_classify(at_level, this_level, out_l, simplify, simple))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[3]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  # analyze
  if (ok_so_far) {
    stn <- run_stage_safe(do_analyze(at_level, this_level, out_l, simplify, simple))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[4]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  # compose
  if (ok_so_far) {
    stn <- run_stage_safe(do_compose(at_level, this_level, out_l, simplify, simple))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[5]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  # collate
  if (ok_so_far) {
    stn <- run_stage_safe(do_collate(at_level, this_level, out_l, simplify, simple))
    if (stn$ok) {
      ran_till <- read_cell_task_orders[6]
      out_l <- stn$out_l
      simple <- stn$simple
    } else {
      ok_so_far <- FALSE
    }
  }

  this_lvl_mx <- min(max(at_level, this_level), length(read_cell_task_orders))
  reached_lvl <- which(read_cell_task_orders == ran_till)

  if (reached_lvl < this_lvl_mx) {
    warn(paste0(
      "Supplied at_level is ", read_cell_task_orders[at_level], ".",
      "\nWhile read_cells could reach till ", ran_till, ".",
      "\nPlease check manually."
    ))
  }

  if (simplify) {
    if (!is.null(simple) & out_l$stage != read_cell_task_orders[6]) {
      # because of "attempt to set an attribute on NULL" error
      attr(simple, "read_cells_stage") <- out_l$stage
    }
    if (out_l$stage == read_cell_task_orders[6] & !is.null(simple)) {
      attr(simple, "read_cells_stage") <- NULL
    }
    simple
  } else {
    class(out_l) <- read_cell_part_class
    out_l
  }
}

#' @rdname read_cells_internal
#' @export
read_cells.character <- function(x,
                                 at_level = c("collate", "detect_and_read", "make_cells", "va_classify", "analyze", "compose"),
                                 omit = NULL,
                                 simplify = TRUE,
                                 compose_main_cols_only = TRUE,
                                 ...) {
  file_name <- x
  common_file_error(file_name)

  out_l <- list(file_name = file_name, stage = "init")

  read_cells.read_cell_part(out_l,
    at_level = at_level,
    omit = omit,
    simplify = simplify,
    compose_main_cols_only = compose_main_cols_only
  )
}
#' @rdname read_cells_internal
#' @export
read_cells.default <- function(x,
                               at_level = c("collate", "detect_and_read", "make_cells", "va_classify", "analyze", "compose"),
                               omit = NULL,
                               simplify = TRUE,
                               compose_main_cols_only = TRUE,
                               from_level,
                               ...) {
  if (!missing(from_level)) {
    val <- validate_read_cell_part_object(x, level = from_level)
    from_level_this <- val$level
    if (val$chk) {
      attr(x, "read_cells_stage") <- from_level_this
    } else {
      if (from_level_this %in% read_cell_task_orders) {
        abort(paste0("the object does not have required type for ", from_level_this))
      } else {
        abort(paste0(
          "have you passed correct `from_level`. It should be one of ",
          paste0(read_cell_task_orders, collapse = ", ")
        ))
      }
    }
  }

  rcp <- make_read_cell_part(x)
  read_cells.read_cell_part(rcp,
    at_level = at_level,
    omit = omit,
    simplify = simplify,
    compose_main_cols_only = compose_main_cols_only
  )
}

#' @rdname read_cells_internal
#' @export
read_cells.NULL <- function(x, ...) {
  cat(cli_b("Please provide a valid file path to process.\n"))
  possible_to_support(print_info = TRUE, return_print_info = FALSE)

  return(invisible(NULL))
}
