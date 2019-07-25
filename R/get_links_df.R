
get_links_df <- function(gid_joins) {
  gids <- unlist(gid_joins) %>%
    unique() %>%
    sort()
  ldf <- NULL
  repeat({
    l0 <- linked_to(gids[1], gid_joins)
    ldf <- ldf %>% bind_rows(tibble(gid = l0, new_gid = min(l0)))
    gids <- setdiff(gids, ldf$gid)
    if (length(gids) == 0) break()
  })
  ldf
}

linked_to_part <- function(x, gid_joins) {
  gid_joins %>%
    map(~ {
      if (x %in% .x) {
        .x
      } else {
        NULL
      }
    }) %>%
    unlist() %>%
    unique() %>%
    sort()
}

linked_to <- function(x, gid_joins) {
  l0 <- linked_to_part(x, gid_joins)
  repeat({
    l1 <- l0 %>%
      map(~ linked_to_part(.x, gid_joins)) %>%
      unlist() %>%
      unique() %>%
      sort()
    if (!identical(l1, l0)) {
      l0 <- l1
    } else {
      break()
    }
  })

  l1
}
