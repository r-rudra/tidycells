# row col df : rc_df

is_conforms_to_rcdf <- function(d) {
  UseMethod("is_conforms_to_rcdf")
}

is_conforms_to_rcdf.data.frame <- function(d) {
  msg <- character(0)

  decision <- FALSE

  if (hasName(d, "row") &
    hasName(d, "col")) {
    int_chk <- FALSE

    if (is.integer(d$row) & is.integer(d$col)) {
      int_chk <- TRUE
    } else {
      suppressMessages(suppressWarnings({

        # delete potential columns with same name
        # this is for checking only
        d$row_orig <- NULL
        d$col_orig <- NULL
        d$chk <- NULL
        # (so that no duplicate name come in following section of code)

        d <- d %>%
          rename(
            row_orig = row,
            col_orig = col
          ) %>%
          mutate(
            row = as.integer(row_orig),
            col = as.integer(col_orig),
            # diff with orig
            row_d = as.numeric(row_orig) - row,
            col_d = as.numeric(col_orig) - col,
            # collect overall checks
            chk = (row_d == 0) & (col_d == 0)
          )
      }))

      int_chk <- all(d$chk)
    }


    if (int_chk) {
      rest_chk <- tibble(chks = c(
        all(d$row > 0),
        all(d$col > 0),
        !any(is.na(d$row)),
        !any(is.na(d$col)),
        (d %>% count(row, col) %>% filter(n > 1) %>% nrow()) == 0
      ), neg_msgs = c(
        "all rows are not positive",
        "all cols are not positive",
        "NA present in rows",
        "NA present in cols",
        "for each (row, col) pair only one cell should be present: which is not true"
      ))

      decision <- all(rest_chk$chks)
      msg <- rest_chk$neg_msgs[!rest_chk$chks]
    } else {
      decision <- FALSE
      msg <- "row/col is not interger (or meaningful coercion not possible)"
    }
  } else {
    decision <- FALSE
    msg <- "row/col column not present"
  }

  if (length(msg)) {
    attr(decision, "msg") <- msg
  }

  decision
}

is_conforms_to_rcdf.matrix <- function(d) {
  d %>%
    as.data.frame() %>%
    is_conforms_to_rcdf.data.frame()
}


as_rc_df <- function(d) {
  d0 <- d %>% select(row, col)
  if (!is_conforms_to_rcdf(d0)) {
    abort("Data does not conforms to rcdf format")
  }
  class(d0) <- setdiff(class(d0), c("cell_df", "cells")) %>%
    c("rc_df", .) %>%
    unique()
  d0
}
