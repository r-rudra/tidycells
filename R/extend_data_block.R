
extend_data_block <- function(dbry, att_map, d_att) {
  att_map %>%
    select(gid_att = attr_gid, gid = data_gid) %>%
    right_join(dbry, by = "gid") %>%
    left_join(d_att$group_id_boundary, by = c("gid_att" = "gid")) %>%
    mutate(
      r_min = pmin(r_min.x, r_min.y, na.rm = TRUE),
      c_min = pmin(c_min.x, c_min.y, na.rm = TRUE),
      r_max = pmax(r_max.x, r_max.y, na.rm = TRUE),
      c_max = pmax(c_max.x, c_max.y, na.rm = TRUE)
    ) %>%
    group_by(gid) %>%
    summarise(
      r_min = min(r_min, na.rm = TRUE),
      c_min = min(c_min, na.rm = TRUE),
      r_max = max(r_max, na.rm = TRUE),
      c_max = max(c_max, na.rm = TRUE)
    )
}
