
ai_relative_data_split_attr <- function(basic_map, d_att, d_dat) {
  chk <- basic_map$map %>%
    group_by(data_gid, attr_gid) %>%
    summarise(n_dirs = n_distinct(direction)) %>%
    ungroup() %>%
    filter(n_dirs > 1)

  done <- FALSE

  if (nrow(chk) > 0) {
    # relative split required
    done <- TRUE

    rel_gids <- chk %>%
      select(-n_dirs) %>%
      rename(gid = attr_gid) %>%
      inner_join(basic_map$raw, by = c("data_gid", "gid"))
    rel_gids <- rel_gids %>%
      mutate(new_attr_gid = paste(gid, data_gid, direction, sep = "_"))

    d_att$group_id_map <- d_att$group_id_map %>%
      bind_rows(rel_gids %>%
        select(gid = new_attr_gid, row, col))
    d_att$group_id_boundary <- get_group_id_boundary(d_att$group_id_map)

    # information kept for missing link detection
    chk_this <- chk %>% distinct(gid = attr_gid, data_gid)
    d_att$missed_blocks <- chk_this

    admap_new <- ai_get_data_attr_map(
      dat_boundary = d_dat$group_id_boundary,
      att_gid_map = d_att$group_id_map
    )
    admap_new$raw <- admap_new$raw %>%
      anti_join(chk_this, by = c("gid", "data_gid"))
    admap_new$all_map <- admap_new$all_map %>%
      anti_join(chk_this, by = c("gid", "data_gid"))
    admap_new$map <- admap_new$map %>%
      anti_join(chk_this %>% rename(attr_gid = gid), by = c("attr_gid", "data_gid"))
  } else {
    admap_new <- basic_map
  }

  list(done = done, d_att = d_att, admap = admap_new)
}
