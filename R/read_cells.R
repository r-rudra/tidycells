
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
#' @examples
#' # see supported files
#' read_cells()
#'
#' fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
#' # File extension is intentionally given wrong
#' # while filename is the actual identifier of the file type
#' fcsv <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]
#' # read the data
#' read_cells(fcsv)
#' read_cells(fcsv, simplify = FALSE)
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

  # detect_and_read
  if (at_level >= 1 & (this_level < 1)) {
    out_l$info <- detect_and_read(out_l$file_name, omit = omit)
    out_l$stage <- read_cell_task_orders[1]
    if (simplify) {
      simple <- out_l$info$content
    }
  }

  # make_cells
  if (at_level >= 2 & (this_level < 2)) {
    if (!is.null(out_l$info$content)) {
      out_l$is_empty <- FALSE

      if (is.data.frame(out_l$info$content)) {
        raw_bc <- list(out_l$info$content)
      } else {
        if (is.list(out_l$info$content)) {
          raw_bc <- out_l$info$content
        } else {
          abort("Unknown error occured in make_cells level of read_cells")
        }
      }

      out_l$cell_list <- raw_bc %>% map(as_cell_df)
      names(out_l$cell_list) <- names(raw_bc)
    } else {
      out_l$is_empty <- TRUE
    }

    out_l$stage <- read_cell_task_orders[2]
    if (simplify) {
      simple <- out_l$cell_list
    }
  }

  # va_classify
  if (at_level >= 3 & (this_level < 3)) {
    if (!out_l$is_empty) {
      out_l$cell_list <- out_l$cell_list %>% map(numeric_values_classifier)
      out_l$stage <- read_cell_task_orders[3]
      if (simplify) {
        simple <- out_l$cell_list
      }
    }
  }

  # analyze
  if (at_level >= 4 & (this_level < 4)) {
    if (!out_l$is_empty) {
      out_l$cell_analysis_list <- out_l$cell_list %>% map(analyze_cells)
      out_l$stage <- read_cell_task_orders[4]
      if (simplify) {
        simple <- out_l$cell_analysis_list
      }
    }
  }

  # compose
  if (at_level >= 5 & (this_level < 5)) {
    if (!out_l$is_empty) {
      raw_comp_no_pp <- out_l$cell_analysis_list %>%
        map(~ compose_cells_raw(.x, post_process = FALSE))

      rcn <- names(raw_comp_no_pp)
      rcn <- rcn[!is.na(rcn)]
      rcn <- rcn[nchar(rcn) > 0]
      rcn <- unique(rcn)
      if (length(rcn) == length(raw_comp_no_pp)) {
        table_tag <- names(raw_comp_no_pp)
      } else {
        table_tag <- seq_along(raw_comp_no_pp) %>% paste0("Table_", .)
      }

      if (simplify & at_level < 6) {
        # compose_cells post_process if simplify = TRUE and at_level < 6
        raw_comp <- raw_comp_no_pp %>%
          map(~ compose_cells_raw_post_process(.x, details = TRUE))

        final_compositions <- seq_along(raw_comp_no_pp) %>%
          map(~ {
            tn1 <- raw_comp_no_pp[[.x]]
            ttag <- table_tag[.x]
            raw_comp_d <- tn1 %>% map(~ mutate(.x, table_tag = ttag))

            tn2 <- raw_comp[[.x]]
            comp_d <- tn2$raw_data[c(tn2$must_cols, tn2$major_col, tn2$minor_col)] %>% mutate(table_tag = ttag)

            list(comp = comp_d, raw = raw_comp_d)
          })
      } else {
        # this means it will be going thourgh 'collate' process
        final_compositions <- seq_along(raw_comp_no_pp) %>%
          map(~ {
            tn1 <- raw_comp_no_pp[[.x]]
            ttag <- table_tag[.x]
            raw_comp_d <- tn1 %>% map(~ mutate(.x, table_tag = ttag))

            list(raw = raw_comp_d)
          })
      }



      out_l$raw_composition <- final_compositions %>% map("raw")
      out_l$final_composition <- final_compositions %>% map_df("comp")

      out_l$stage <- read_cell_task_orders[5]
      if (simplify) {
        simple <- out_l$final_composition
      }
    }
  }


  # collate
  if (at_level >= 6 & (this_level < 6)) {
    if (!out_l$is_empty) {
      raw_present <- FALSE
      if (!is.null(out_l$raw_composition)) {
        raw_present <- TRUE
      }

      if (raw_present) {
        dcl <- out_l$raw_composition
      } else {
        dcl <- list(out_l$final_composition)
      }

      out_l$final <- dcl %>% map_df(~ collate_columns(.x) %>% as_tibble())

      out_l$stage <- read_cell_task_orders[6]
      if (simplify) {
        simple <- out_l$final
      }
    }
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
