
norm_this <- function(x) {
  m <- min(x, na.rm = TRUE)
  M <- max(x, na.rm = TRUE)
  if (m == M) {
    if (m > 0.5) {
      x <- rep(1, length(x))
    } else {
      x <- rep(0, length(x))
    }
  } else {
    x <- (x - m) / (M - m)
  }
  x
}


# possibly table tag if across table collation is required
defcols <- c("row", "col", "value", "data_block", "table_tag")
