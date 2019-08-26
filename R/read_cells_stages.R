
do_detect_and_read <- function(at_level, this_level, out_l, simplify, simple, omit) {
  if (at_level >= 1 & (this_level < 1)) {
    out_l$info <- detect_and_read(out_l$file_name, omit = omit)
    out_l$stage <- read_cell_task_orders[1]
    if (simplify) {
      simple <- out_l$info$content
    }
  }

  list(out_l = out_l, simple = simple)
}

do_make_cells <- function(at_level, this_level, out_l, simplify, simple) {
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

  list(out_l = out_l, simple = simple)
}

do_va_classify <- function(at_level, this_level, out_l, simplify, simple) {
  if (at_level >= 3 & (this_level < 3)) {
    if (!out_l$is_empty) {
      out_l$cell_list <- out_l$cell_list %>% map(numeric_values_classifier)
      out_l$stage <- read_cell_task_orders[3]
      if (simplify) {
        simple <- out_l$cell_list
      }
    }
  }

  list(out_l = out_l, simple = simple)
}

do_analyze <- function(at_level, this_level, out_l, simplify, simple) {
  if (at_level >= 4 & (this_level < 4)) {
    if (!out_l$is_empty) {
      out_l$cell_analysis_list <- out_l$cell_list %>% map(analyze_cells)
      out_l$stage <- read_cell_task_orders[4]
      if (simplify) {
        simple <- out_l$cell_analysis_list
      }
    }
  }

  list(out_l = out_l, simple = simple)
}

do_compose <- function(at_level, this_level, out_l, simplify, simple) {
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

  list(out_l = out_l, simple = simple)
}

do_collate <- function(at_level, this_level, out_l, simplify, simple) {
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

      out_l$final <- dcl %>%
        map(~ collate_columns(.x, retain_cell_address = TRUE)) %>%
        collate_columns()

      out_l$stage <- read_cell_task_orders[6]
      if (simplify) {
        simple <- out_l$final
      }
    }
  }

  list(out_l = out_l, simple = simple)
}

run_stage_safe <- function(expr) {
  e <- try(expr = expr, silent = TRUE)
  if (inherits(e, "try-error")) {
    return(list(ok = FALSE))
  }
  e$ok <- TRUE
  e
}
