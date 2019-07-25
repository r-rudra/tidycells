

read_pdf_from_tabulizer <- function(fn) {
  if (!rlang::is_installed("tabulizer")) {
    abort("'tabulizer' package is required")
  }

  # read in both ways [time consuming!]
  # as decide may not function as expected
  pl <- try(tabulizer::extract_tables(fn, method = "lattice"), silent = TRUE)
  ps <- try(tabulizer::extract_tables(fn, method = "stream"), silent = TRUE)

  # final
  pf <- NULL

  # if both are try-error return one of them
  if (inherits(pl, "try-error") & inherits(ps, "try-error")) {
    pf <- pl
  } else {
    if (!inherits(pl, "try-error") & is.list(pl) & inherits(ps, "try-error")) {
      pf <- pl
    }

    if (inherits(pl, "try-error") & !inherits(ps, "try-error") & is.list(ps)) {
      pf <- ps
    }

    if (!inherits(pl, "try-error") & !inherits(ps, "try-error")) {
      # if length unequal return the same with more length
      if (length(pl) > length(ps)) {
        pf <- pl
      }

      if (length(pl) < length(ps)) {
        pf <- ps
      }

      # if both of them have same length
      if (length(pl) == length(ps)) {
        # do degenrate study
        degenrate_l <- pl %>%
          map(dim) %>%
          map_lgl(~ any(.x < 2))
        degenrate_s <- ps %>%
          map(dim) %>%
          map_lgl(~ any(.x < 2))

        pf <- seq_along(pl) %>%
          map(~ {
            if (degenrate_l[.x]) {
              return(ps[[.x]])
            }
            if (degenrate_s[.x]) {
              return(pl[[.x]])
            }

            # prefer low area
            al <- dim(pl[[.x]]) %>% prod()
            as <- dim(ps[[.x]]) %>% prod()
            if (al <= as) {
              return(pl[[.x]])
            }
            if (al > as) {
              return(ps[[.x]])
            }
          })
      }
    }
  }

  return(pf)
}
