# for sharing data
mask_data <- function(d) {
  validate_cells(d)
  if (hasName(d, "type")) {
    d0 <- d %>% mutate(value = recode(type, attribute = "A", value = "0", empty = ""))
  } else {
    d0 <- d %>% mutate(value = recode(data_type, character = "A", numeric = "0"))
  }
  as_tibble(d0)
}
