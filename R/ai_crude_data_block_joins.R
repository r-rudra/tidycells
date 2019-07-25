ai_crude_data_block_joins <- function(basic_admap, d_dat) {
  main_dir_attr <- basic_admap %>%
    filter(direction_group != "corner") %>%
    group_by(data_gid) %>%
    summarise(aid = attr_gid %>%
      paste0("_", direction) %>%
      unique() %>%
      sort() %>%
      paste0(collapse = "+"))

  crude_map <- main_dir_attr %>%
    group_by(aid) %>%
    mutate(new_gid = min(data_gid), n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    select(gid = data_gid, new_gid)

  done <- FALSE
  if (nrow(crude_map) > 0) {
    done <- TRUE
    d_dat <- get_group_id_join_gids(d_dat, gid_map = crude_map)
  }
  list(d_dat = d_dat, done = done)
}
