


#' Get Group ID for joined cells
#'
#' @param dat the (data or attribute) cells (in at least row-col format)
#'
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Group ID attached information in a list
#'
get_group_id <- function(dat) {
  digi_sep <- dat %>%
    summarise(rm = max(row), cm = max(col)) %>%
    map(~ log(.x, base = 10) %>% ceiling()) %>%
    unlist() %>%
    max()
  digi_sep <- digi_sep + 1

  # attach r_id, c_id
  dat %>%
    select(row, col) %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(cid = (c(1, diff(col)) != 1) %>% cumsum()) %>%
    ungroup() %>%
    mutate(cid = cid + row * 10^digi_sep) %>%
    arrange(col, row) %>%
    group_by(col) %>%
    mutate(rid = (c(1, diff(row)) != 1) %>% cumsum()) %>%
    ungroup() %>%
    mutate(rid = rid + col * 10^digi_sep) -> drc_id

  # attach g_id
  drc_id <- drc_id %>% mutate(gid = rid)

  n_gid <- drc_id %>%
    summarise(n_distinct(gid)) %>%
    pull(1)

  repeat ({
    drc_id %>%
      group_by(cid) %>%
      mutate(gid = min(gid)) %>%
      group_by(rid) %>%
      mutate(gid = min(gid)) %>%
      ungroup() -> drc_id

    if (n_gid > (drc_id %>% summarise(n_distinct(gid)) %>% pull(1))) {
      n_gid <- drc_id %>%
        summarise(n_distinct(gid)) %>%
        pull(1)
    } else {
      break()
    }
  })

  drc_id <- drc_id %>% mutate(gid = as.character(gid))

  # boundary
  drc_boundary <- get_group_id_boundary(drc_id)

  list(group_id_map = drc_id, group_id_boundary = drc_boundary)
}


get_group_id_boundary <- function(drc_id) {
  drc_id %>%
    group_by(gid) %>%
    summarise(r_min = min(row), c_min = min(col), r_max = max(row), c_max = max(col))
}


get_group_id_join_gids <- function(old_group_id_info, gid_map) {
  old_group_id_info$group_id_map <- old_group_id_info$group_id_map %>%
    left_join(gid_map, by = "gid") %>%
    mutate(new_gid = if_else(is.na(new_gid), gid, new_gid)) %>%
    select(-gid) %>%
    rename(gid = new_gid)
  old_group_id_info$group_id_boundary <- get_group_id_boundary(old_group_id_info$group_id_map)
  old_group_id_info
}
