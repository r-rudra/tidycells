


get_col_representative <- function(x, cut_th = 500L, lower_it = TRUE, silent = TRUE) {
  x <- x[!is.na(x)]
  x <- unique(x)
  x <- as.character(x)
  if (lower_it) {
    x <- tolower(x)
  }
  x <- stringr::str_trim(x)
  x <- unique(x)
  dxt <- tibble(x, nc = nchar(x))
  dxt <- dxt[dxt$nc > 0, ]
  x <- dxt$x
  if (length(x) > cut_th) {
    if (!silent) {
      message(paste0(
        "Representatives for column is selected based on a sample.",
        "\nYou may need to set.seed() to have a reproducible outcome"
      ))
    }
    dxt <- dxt %>% arrange(nc)
    fidx <- round(seq(from = 1.5, to = nrow(dxt) - 0.5, length.out = 10))
    dxtr <- dxt[-fidx, ]
    x1 <- sample(dxtr$x, max(round(cut_th / 10), min(4, length(dxtr$x))))
    x2 <- dxt$x[fidx]
    x <- unique(x1, x2)
  }
  sort(unique(x))
}

get_all_col_representative <- function(d, except_cols = defcols, ...) {
  d <- d[setdiff(colnames(d), except_cols)]
  crs <- d %>% map(get_col_representative, ...)
  crsd <- tibble(nn = names(crs), tinf = crs %>% map_chr(~ paste0(sort(.x), collapse = " :: ")))
  crsd <- crsd %>% mutate(tndup = !duplicated(tinf))
  crs <- crs[crsd$nn[crsd$tndup]]

  crs
}
