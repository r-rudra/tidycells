


as_cell_df_internal <- function(d, ...) {
  UseMethod("as_cell_df_internal")
}


as_cell_df_internal.default <- function(d,
                                        take_row_names = FALSE,
                                        take_col_names = FALSE) {
  if (!is.data.frame(d)) {
    abort("Data frame is expected")
  }

  if (nrow(d) < 1) {
    abort("at least 1 row is required")
  }

  if (validate_cells(d)) {
    return(new_cell_df(d))
  }

  # applicable on intermediate class "unknown"

  rcdf_possible <- is_conforms_to_rcdf(d)
  d_out <- tibble()

  if (!rcdf_possible) {
    warn_msg <- attr(rcdf_possible, "msg")
    wlvl <- attr(rcdf_possible, "lvl")
    if (length(wlvl) != 1) wlvl <- 0
    if (is.na(wlvl)) wlvl <- 0
  } else {
    warn_msg <- character(0)
    wlvl <- 0
  }

  conv_done <- FALSE
  translate <- FALSE

  if (rcdf_possible) {
    # presence of value column

    if (hasName(d, "value")) {
      if (rlang::is_atomic(d$value)) {
        d_out <- d %>% mutate(data_type = if_else(is.numeric(d$value), "numeric", "character"))
        conv_done <- TRUE
      } else {
        warn_msg <- warn_msg %>%
          c("The data has (row, col, value) but value column is not atomic.")
      }
    } else {
      rest_cols <- colnames(d) %>% setdiff(c("row", "col", "data_type"))

      # only single column except row, col and data_type
      if (length(rest_cols) == 1) {
        rest_col <- d[[rest_cols]]

        if (rlang::is_atomic(rest_col)) {
          d_out <- d %>%
            mutate(
              value = rest_col,
              data_type = if_else(is.numeric(value), "numeric", "character")
            )

          conv_done <- TRUE
        } else {
          warn_msg <- warn_msg %>%
            c("The data has (row, col) and a single column possibly containing values, but that column is not atomic.")
        }
      } else {
        warn_msg <- warn_msg %>%
          c("The data has (row, col). However, the column containing value is not detectable")
      }
    }

    if (!conv_done) {
      warn_msg <- warn_msg %>%
        c(paste0(
          "The data conforms minimal row col data structure. ",
          "However, the column containing value is not detectable. ",
          "Data transformation is used (check if the output is as expected)."
        )) %>%
        unique()

      wlvl <- max(wlvl, 2)

      translate <- TRUE
    }
  } else {
    translate <- TRUE
  }

  warn_msg <- unique(warn_msg)

  if (translate) {
    if (length(warn_msg) > 0 & wlvl > 1) {
      message("Data is transformed into row-col-value format. (check if the output is as expected)")
      warn(paste0(warn_msg, collapse = "\n"))
    }

    d_out <- d %>%
      unpivotr::as_cells(row_names = take_row_names, col_names = take_col_names) %>%
      attach_intermediate_class() %>%
      as_cell_df()

    conv_done <- TRUE
  }

  if (conv_done) {
    d_out$value <- as.character(d_out$value)
    new_cell_df(d_out)
  } else {
    abort("unknown error occurred")
  }
}

as_cell_df_internal.cell_df <- function(d) {
  new_cell_df(d)
}

as_cell_df_internal.tidyxl <- function(d) {
  if (!is.data.frame(d)) {
    abort("Data frame is expected")
  }

  if (nrow(d) < 1) {
    abort("at least 1 row is required")
  }

  n_sheets <- d %>%
    distinct(sheet) %>%
    nrow()

  if (n_sheets != 1) {
    abort(paste("Supplied data is detected to have data structure similar to tidyxl::xlsx_cells output.",
      "Which is already in cell_df like format. But it has multiple sheets.",
      "cell_df can be constructed for a single sheet only.",
      "You may like to split the data by sheet and then obtain cell_df for each sheets.",
      sep = "\n"
    ))
  }

  d_out <- d %>%
    filter(!is_blank) %>%
    mutate(value = case_when(
      data_type == "numeric" ~ as.character(numeric),
      data_type == "logical" ~ as.character(logical),
      data_type == "date" ~ as.character(date),
      data_type == "character" ~ as.character(character),
      TRUE ~ NA_character_
    )) %>%
    mutate(data_type = if_else(data_type == "numeric", "numeric", "character")) %>%
    filter(!is.na(value)) %>%
    distinct(row, col, data_type, value)

  new_cell_df(d_out)
}

as_cell_df_internal.unpivotr <- function(d) {
  if (!is.data.frame(d)) {
    abort("Data frame is expected")
  }

  if (nrow(d) < 1) {
    abort("at least 1 row is required")
  }

  cols <- tibble(
    cn = c("chr", "cplx", "dbl", "fct", "int", "lgl", "ord", "date", "dttm"),
    bv = list(NA_character_, NA_complex_, NA_real_, list(), NA_integer_, NA, list(), as.Date(NA), as.POSIXct(NA))
  )

  other_types <- setdiff(d$data_type, cols$cn)
  if (length(other_types) > 0) {
    warn(paste0(
      "Internal data contains types (",
      paste0(unique(other_types), collapse = ", "),
      ") which are not yet implemented."
    ))
  }

  for (cn in seq(nrow(cols))) {
    if (!hasName(d, cols$cn[cn])) {
      d[[cols$cn[cn]]] <- cols$bv[cn]
    }
  }

  d_out <- d %>%
    mutate(value = case_when(
      data_type == "chr" ~ as.character(chr),
      data_type == "cplx" ~ as.character(cplx),
      data_type == "fct" ~ (fct %>% map(as.character) %>% map_chr(~ if_else(length(.x) > 0, .x[1], NA_character_))),
      data_type == "dbl" ~ as.character(dbl),
      data_type == "int" ~ as.character(int),
      data_type == "lgl" ~ as.character(lgl),
      data_type == "ord" ~ (ord %>% map(as.character) %>% map_chr(~ if_else(length(.x) > 0, .x[1], NA_character_))),
      data_type == "date" ~ as.character(date),
      data_type == "dttm" ~ as.character(dttm),
      TRUE ~ NA_character_
    )) %>%
    mutate(data_type = if_else(data_type %in% c("cplx", "dbl", "int"), "numeric", "character")) %>%
    filter(!is.na(value)) %>%
    distinct(row, col, data_type, value)

  new_cell_df(d_out)
}

as_cell_df_internal.readr <- function(d) {
  if (!is.data.frame(d)) {
    abort("Data frame is expected")
  }

  if (nrow(d) < 1) {
    abort("at least 1 row is required")
  }

  d_out <- d %>%
    filter(data_type != "missing") %>%
    mutate(data_type = if_else(data_type %in% c("integer", "double"), "numeric", "character")) %>%
    filter(!is.na(value)) %>%
    distinct(row, col, data_type, value)

  new_cell_df(d_out)
}
