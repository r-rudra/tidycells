

ai_relative_data_join_attr <- function(admap_main, d_att) {
  chk <- admap_main$raw_map %>%
    distinct(attr_gid, data_gid, direction, attr_group) %>%
    group_by(data_gid, direction, attr_group) %>%
    mutate(n_att = n_distinct(attr_gid)) %>%
    ungroup() %>%
    filter(n_att > 1)

  done <- FALSE

  if (nrow(chk) > 0) {
    # relative join required
    done <- TRUE

    rel_gids <- chk %>%
      select(-n_att) %>%
      inner_join(admap_main$raw_map, by = c("attr_gid", "data_gid", "direction", "attr_group"))

    d_att_dat_map_raw_rest <- admap_main$raw_map %>%
      anti_join(chk, by = c("attr_gid", "data_gid", "direction", "attr_group"))

    rel_gids_att <- rel_gids %>%
      distinct(attr_gid, data_gid, direction, attr_group, row = row_a, col = col_a) %>%
      group_by(data_gid, direction, attr_group) %>%
      mutate(new_attr_gid = paste(min(attr_gid), data_gid, direction, sep = "_")) %>%
      ungroup()

    rel_gids <- rel_gids %>% inner_join(rel_gids_att %>% distinct(attr_gid, new_attr_gid), by = "attr_gid")

    rel_gids <- rel_gids %>%
      group_by(new_attr_gid, data_gid) %>%
      mutate(
        # this is possibly not required anymore as attr_group is in grouping vars
        new_attr_group = ifelse(any(attr_group == "major"), "major", "minor"),
        new_dist = min(dist)
      ) %>%
      ungroup()

    admap_main$raw_map <- rel_gids %>%
      select(-attr_group, -attr_gid, -dist) %>%
      rename(attr_gid = new_attr_gid, attr_group = new_attr_group, dist = new_dist) %>%
      bind_rows(d_att_dat_map_raw_rest)

    admap_main$map <- admap_main$raw_map %>%
      distinct(attr_gid, data_gid, direction, direction_group, dist, attr_group)

    # dimesion analysis is not kept

    # update d_att
    d_att$group_id_map <- d_att$group_id_map %>%
      bind_rows(rel_gids_att %>%
        select(gid = new_attr_gid, row, col))
    d_att$group_id_boundary <- get_group_id_boundary(d_att$group_id_map)

    chk_this <- chk %>% distinct(gid = attr_gid, data_gid)

    if (is.null(d_att$missed_blocks)) {
      d_att$missed_blocks <- chk_this
    } else {
      d_att$missed_blocks <- chk_this %>%
        bind_rows(d_att$missed_blocks) %>%
        unique()
    }
  }

  list(done = done, d_att = d_att, admap = admap_main)
}
