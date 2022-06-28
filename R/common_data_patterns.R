

#@Dev
# quick added may need to think through about how to add it properly.
# common data patterns
cdp_va_classify_value_rect <- function(d){
  val_range <- d %>%
    filter(type == "value") %>%
    summarise(mr = min(row), mc = min(col), Mr = max(row), Mc =max(col))
  d$type[
    d$row>=val_range$mr &
      d$row<=val_range$Mr &
      d$col>=val_range$mc &
      d$col<=val_range$Mc
  ] <- "value"
  d
}

