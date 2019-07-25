

ai_attach_direction <- function(d_att_dat_map_raw) {
  d_att_dat_map_raw %>%
    # kept for tracking
    mutate(direction_basic = direction) %>%
    mutate(attr_gid_split = ifelse(direction_group == "NS", row_a,
      ifelse(direction_group == "WE", col_a, 0)
    )) %>%
    group_by(data_gid, attr_gid, direction, attr_gid_split) %>%
    group_split() %>%
    map_df(~ .x %>% mutate(direction = get_direction(.x)))
}
