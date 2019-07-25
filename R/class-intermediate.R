
#' @importFrom utils hasName


detect_cell_df_pattern <- function(dat) {

  # this is built as per the description of the return values from supported packages
  # supported packages / functions :
  # 1) tidyxl::xlsx_cells
  # 2) unpivotr::as_cells
  # 3) readr::melt_csv (and family)

  chk <- tibble(
    type = c(
      "tidyxl",
      "unpivotr",
      "readr"
    ),

    col_names = list(
      c("sheet", "address", "row", "col", "is_blank", "data_type", "error", "logical", "numeric", "date", "character"),
      c("row", "col", "data_type"),
      c("row", "col", "data_type", "value")
    ),

    data_types = list(
      c("error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cplx", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      c("integer", "character", "date")
    ),

    optional_cols = list(
      c("sheet", "error", "logical", "numeric", "date", "character", "blank"),
      c("chr", "cplx", "cplx", "dbl", "fct", "int", "lgl", "list", "ord"),
      c()
    )
  )

  if (!hasName(dat, "data_type")) {
    # all of them has data_type
    return("unknown")
  }

  d_type <- chk %>%
    mutate(
      ccn = col_names %>% map_lgl(~ hasName(dat, .x) %>% all()),
      cdt = data_types %>% map_int(~ (.x %in% dat$data_type) %>% sum()),
      coc = optional_cols %>% map_int(~ hasName(dat, .x) %>% sum())
    ) %>%
    filter(ccn) %>%
    filter(cdt == max(cdt), coc == max(coc)) %>%
    pull(type)

  if (length(d_type) == 0) {
    return("unknown")
  }

  if (length(d_type) > 1) {
    d_type <- d_type[1]
  }

  d_type
}


attach_intermediate_class <- function(dat) {
  class(dat) <- c(class(dat), detect_cell_df_pattern(dat)) %>% unique()

  dat
}
