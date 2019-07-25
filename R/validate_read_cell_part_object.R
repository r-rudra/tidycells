
validate_read_cell_part_object <- function(x, level) {
  stage <- attr(x, "read_cells_stage")
  if (missing(level)) {
    level <- stage
  }

  if (is.null(level)) {
    abort("read_cell_part object is not valid")
  }

  if (length(level) != 1) {
    abort("read_cell_part object is not valid")
  }

  if (is.numeric(level)) {
    level <- as.integer(level)
    if (level >= 1 & level <= length(read_cell_task_orders)) {
      level <- read_cell_task_orders[level]
    }
  }

  if (is.character(level)) {
    if (level %in% read_cell_task_orders) {
      if (level == read_cell_task_orders[1]) {
        # NULL is possible
        if (is.null(x)) {
          return(TRUE)
        }
        # a df
        if (is.data.frame(x)) {
          if (nrow(x) > 0) {
            return(TRUE)
          }
        }
        # list of dfs
        if (is.list(x)) {
          if (length(x) > 0) {
            if (is.data.frame(x[[1]])) {
              return(TRUE)
            }
          }
        }
      }
      if (level == read_cell_task_orders[2]) {
        if (is.list(x)) {
          if (length(x) > 0) {
            if (x %>% map_lgl(is_cell_df) %>% all()) {
              return(TRUE)
            }
          }
        }
      }
      if (level == read_cell_task_orders[3]) {
        if (is.list(x)) {
          if (length(x) > 0) {
            if (x %>% map_lgl(is_cell_df) %>% all()) {
              if (x %>% map_lgl(~ hasName(.x, "type")) %>% all()) {
                return(TRUE)
              }
            }
          }
        }
      }
      if (level == read_cell_task_orders[4]) {
        if (is.list(x)) {
          if (length(x) > 0) {
            if (x %>% map_lgl(~ inherits(.x, cell_df_analysis_class[1])) %>% all()) {
              return(TRUE)
            }
          }
        }
      }
      if (level == read_cell_task_orders[5]) {
        if (is.data.frame(x)) {
          if (inherits(x, "rc_df")) {
            return(TRUE)
          }
        }
      }
    } else {
      abort("read_cell_part object is not valid")
    }
  }

  FALSE
}
