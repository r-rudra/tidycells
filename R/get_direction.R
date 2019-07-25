

#' get optimal direction (in terms of `unpivotr` directions)
#'
#' @param d_part parts of `data_attr_map_raw`
#' @details Used internally
#' @keywords internal
#' @return a string value denoting direction
#'

get_direction <- function(d_part) {
  directions <- get_unpivotr_direction_names()

  if (d_part$direction[1] %in% names(directions)) {
    dirs <- directions[[d_part$direction[1]]]

    if (length(dirs) > 1) {
      d0 <- d_part %>%
        distinct(data_gid, row = row_d, col = col_d)

      d0s <- d0 %>% summarise(mnr = min(row), mxr = max(row), mnc = min(col), mxc = max(col))

      d1 <- tibble(row = seq(from = d0s$mnr, to = d0s$mxr, by = 1), col = d0s$mnc) %>%
        bind_rows(
          tibble(col = seq(from = d0s$mnc, to = d0s$mxc, by = 1), row = d0s$mnr)
        ) %>%
        mutate(data_gid = d0$data_gid[1]) %>%
        unique()

      a1 <- d_part %>%
        distinct(attr_gid, row = row_a, col = col_a)

      dmd <- tibble()
      for (dir in dirs) {
        dm_now <- get_direction_metric(d1, a1, dir)
        dmd <- dmd %>% bind_rows(tibble(dm = dm_now, dir = dir))
        if (dm_now == 1) break()
      }
      dmd$dir[which.max(dmd$dm)]
    } else {
      dirs[1]
    }
  } else {
    abort("direction name not known.\n(have you tampered a cell-analysis?)")
  }
}
