
refine_cols <- function(dcomp_part, cn_df, sep = " :: ") {
  cn_df <- cn_df %>% filter(cname %in% colnames(dcomp_part))
  out <- dcomp_part
  if (nrow(cn_df) > 0) {
    cn_df <- cn_df %>%
      group_by(ag) %>%
      mutate(cname_new = rc_n %>% as.factor() %>% as.integer() %>% paste0(ag, "_", .)) %>%
      ungroup() %>%
      arrange(cname_new, cname_ord)

    cn_l <- cn_df %>%
      select(cname, cname_new) %>%
      split(.$cname_new)

    out_p <- cn_l %>%
      map(~ {
        .d0 <- dcomp_part[.x$cname]
        .d0 <- .d0 %>% mutate_all(stringr::str_trim)
        dcomp_part[.x$cname_new[1]] <- .d0 %>% apply(1, paste, collapse = sep)
        dcomp_part[c("row", "col", .x$cname_new[1])]
      }) %>%
      reduce(fj, join_by = c("row", "col"))

    out <- out_p %>% fj(dcomp_part, join_by = c("row", "col"))
  }

  out
}
