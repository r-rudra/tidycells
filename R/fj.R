fj <- function(x, y,
               join_by = c("row", "col", "value", "data_block"),
               sallow_join = FALSE, sep = " :: ") {
  c1 <- colnames(x)
  c2 <- colnames(y)
  comm_cols <- c1 %>%
    intersect(c2) %>%
    setdiff(join_by)
  chk <- comm_cols %>%
    length()
  if (chk > 0) {
    if (identical(sallow_join, TRUE)) {
      # try to merge the columns
      # check first content
      # need to update x, y so using for loop
      for (cn in comm_cols) {
        xc <- x[[cn]] %>% stringr::str_trim()
        yc <- y[[cn]] %>% stringr::str_trim()
        if (all(xc == yc)) {
          # both the columns are actually equal
          # removing from y
          y[[cn]] <- NULL
        } else {
          # string join in x
          x[[cn]] <- paste0(x[[cn]], sep, y[[cn]])
          # removing from y
          y[[cn]] <- NULL
        }
      }
    } else {
      abort(paste(
        "unexpected error while joining.",
        "(Please contact developer)"
      ))
    }
  }
  full_join(x, y, by = join_by)
}
