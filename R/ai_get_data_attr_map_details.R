
# dimention analysis and raw maps
ai_get_data_attr_map_details <- function(basic_map, d_dat, d_att, major_direction_relax = TRUE) {
  dimension_analysis <- list()

  dimension_analysis$data_gid_dim <- d_dat$group_id_map %>%
    group_by(gid) %>%
    summarise(
      r_dim_data = n_distinct(row),
      c_dim_data = n_distinct(col)
    )


  d_att_dat_map <- basic_map

  d_att_dat_map_raw <- d_att_dat_map %>%
    # join with data_gid to attach all data-cells
    inner_join(d_dat$group_id_map %>%
      select(row_d = row, col_d = col, data_gid = gid),
    by = "data_gid"
    ) %>%
    # join with attr_gid to attach all attr-cells
    inner_join(d_att$group_id_map %>%
      select(row_a = row, col_a = col, attr_gid = gid),
    by = "attr_gid"
    )

  # attach dimension
  dimension_analysis$attr_data_dim <- d_att_dat_map_raw %>%
    group_by(attr_gid, data_gid) %>%
    summarise(
      r_dim = row_d %>% intersect(row_a) %>% length(),
      c_dim = col_d %>% intersect(col_a) %>% length(),
      direction_group = direction_group[1]
    ) %>%
    ungroup() %>%
    inner_join(dimension_analysis$data_gid_dim, by = c("data_gid" = "gid")) %>%
    mutate(rel_dim = ifelse(direction_group == "NS", c_dim / c_dim_data, r_dim / r_dim_data)) %>%
    mutate(rel_dim = ifelse(direction_group == "corner", 0, rel_dim)) %>%
    mutate(full_dim = (rel_dim >= 1))

  # in case only non full dim major (NS or WE) attr present
  if (major_direction_relax) {
    dimension_analysis$attr_data_dim <- dimension_analysis$attr_data_dim %>%
      group_by(data_gid, direction_group) %>%
      mutate(
        is_full_dim_present = any(full_dim),
        this_attr_max_rel = (rel_dim == max(rel_dim))
      ) %>%
      ungroup() %>%
      rename(full_dim_orig = full_dim) %>%
      mutate(full_dim = ifelse(direction_group == "corner",
        full_dim_orig,
        ifelse(is_full_dim_present,
          full_dim_orig,
          this_attr_max_rel
        )
      ))
  }

  # fix major minor

  d_att_dat_map <- dimension_analysis$attr_data_dim %>%
    distinct(attr_gid, data_gid, full_dim) %>%
    right_join(d_att_dat_map, by = c("attr_gid", "data_gid")) %>%
    mutate(attr_group = ifelse(full_dim, "major", "minor")) %>%
    select(-full_dim)



  d_att_dat_map_raw <- d_att_dat_map_raw %>%
    inner_join(d_att_dat_map %>% select(attr_gid, data_gid, direction, attr_group),
      by = c("attr_gid", "direction", "data_gid")
    )

  list(raw_map = d_att_dat_map_raw, map = d_att_dat_map, dimension_analysis = dimension_analysis)
}
