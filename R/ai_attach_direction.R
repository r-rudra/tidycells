

ai_attach_direction <- function(d_att_dat_map_raw) {

  # asp: attr split
  d_att_dat_map_raw_asp <- d_att_dat_map_raw %>%
    # kept for tracking
    mutate(direction_basic = direction) %>%
    mutate(attr_gid_split = ifelse(direction_group == "NS" & attr_group == "major", row_a,
      ifelse(direction_group == "WE" & attr_group == "major", col_a, 0)
    ))

  d_att_dat_map_raw_asp <- d_att_dat_map_raw_asp %>%
    mutate(attr_gid_split = ifelse(direction_group == "corner", col_a, attr_gid_split))

  d_att_dat_map_raw_asp %>%
    group_by(data_gid, attr_gid, direction, attr_gid_split) %>%
    group_split() %>%
    map_df(~ .x %>% mutate(direction = get_direction(.x)))
}
