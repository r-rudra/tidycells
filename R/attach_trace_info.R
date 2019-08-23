
attach_trace_info <- function(ca, dc) {
  if (missing(dc)) {
    if (missing(ca)) {
      abort("either 'ca' or 'dc' required")
    }
    dc_t <- compose_cells_raw(ca, trace_it_back = TRUE, silent = TRUE)
    dc <- dc_t
  } else {
    if (!any(stringr::str_detect(colnames(dc), "cellAddress_"))) {
      if (!missing(ca)) {
        dc_t <- compose_cells_raw(ca, trace_it_back = TRUE, silent = TRUE)
        dc_t <- dc_t[c("row", "col", colnames(dc_t)[stringr::str_detect(colnames(dc_t), "cellAddress_")])]
        dc_t$chk_this <- "ok"
        dc <- dc %>% left_join(dc_t, by = c("row", "col"))
        if (any(is.na(dc$chk_this))) {
          abort("at least one row could not be mapped properly")
        }
      } else {
        abort("supplied composition does not contain trace information and 'ca' is not given")
      }
    }
  }

  dc
}
