
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
  extra <- NULL
  if (chk > 0) {
    if (identical(sallow_join, TRUE)) {
      # try to merge the columns
      # check first content
      # need to update x, y so using for loop

      fmg_wc <- full_join(x, y, by = join_by, suffix = c(".fj1", ".fj2"))

      for (cn in comm_cols) {
        xc <- fmg_wc[[paste0(cn, ".fj1")]] %>% stringr::str_trim()
        yc <- fmg_wc[[paste0(cn, ".fj2")]] %>% stringr::str_trim()

        xc[is.na(xc)] <- ""
        yc[is.na(yc)] <- ""

        fmg_wc[[paste0(cn, ".fj1")]] <- xc
        fmg_wc[[paste0(cn, ".fj2")]] <- yc

        # back to NA again to remove them from comparison
        xc[nchar(xc) == 0] <- NA
        yc[nchar(yc) == 0] <- NA

        if (all(xc == yc, na.rm = TRUE)) {
          # both the columns are actually equal
          # removing from x, y
          fmg_wc[[cn]] <- fmg_wc[[paste0(cn, ".fj1")]]
          x[[cn]] <- NULL
          y[[cn]] <- NULL
        } else {
          # string join in x
          fmg_wc[[cn]] <- paste0(fmg_wc[[paste0(cn, ".fj1")]], sep, fmg_wc[[paste0(cn, ".fj2")]])
          # removing from x, y
          x[[cn]] <- NULL
          y[[cn]] <- NULL
        }
      }

      extra <- unique(fmg_wc[c(join_by, comm_cols)])
    } else {
      abort(paste(
        "unexpected error while joining.",
        "(Please contact developer)"
      ))
    }
  }
  f0 <- full_join(x, y, by = join_by)
  if (!is.null(extra)) {
    if (is.data.frame(extra)) {
      if (nrow(extra) > 0) {
        if (length(setdiff(intersect(colnames(extra), colnames(f0)), join_by)) == 0) {
          f0 <- full_join(f0, extra, by = join_by)
        }
      }
    }
  }
  f0
}
