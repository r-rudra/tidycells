
ai_get_data_attr_map <- function(dat_boundary,
                                 att_gid_map,
                                 attr_to_near_data = FALSE) {

  # check relative location of each attr_gid (gid) wrt each data_gid
  d_att_map <- dat_boundary %>%
    split(.$gid) %>%
    map_df(~ get_direction_df(.x, datt = att_gid_map))

  # for each attr_gid (gid), data_gid, direction, direction_group :- get minimum distance
  d_gid_att_map <- d_att_map %>%
    group_by(gid, data_gid, direction, direction_group) %>%
    summarise(md = min(dist)) %>%
    ungroup()

  # attach nearest attr_gid to each data gid
  d_gid_att_map_min_d <- d_gid_att_map %>%
    group_by(data_gid, direction_group) %>%
    mutate(m_dist = min(md)) %>%
    ungroup() %>%
    filter(md == m_dist) %>%
    select(-md) %>%
    rename(attr_gid = gid, dist = m_dist)

  # by default each data gid will choose nearest attr_gid (multiple allowed)
  if (attr_to_near_data) {
    # this does same for attr gid too
    # after each data gid choose nearest attr then each attr will choose nearest data gid

    d_gid_att_map_min_d <- d_gid_att_map_min_d %>%
      group_by(attr_gid) %>%
      mutate(md = min(dist)) %>%
      ungroup() %>%
      filter(md == dist) %>%
      select(-md)
  }


  list(map = d_gid_att_map_min_d, all_map = d_gid_att_map, raw = d_att_map)
}
