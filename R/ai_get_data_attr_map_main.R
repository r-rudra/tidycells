
ai_get_data_attr_map_main <- function(d_dat, d_att, crude_join = TRUE) {

  #  start with simple attr data map
  admap0 <- ai_get_data_attr_map(
    dat_boundary = d_dat$group_id_boundary,
    att_gid_map = d_att$group_id_map
  )


  if (crude_join) {
    # crude joins
    # absolutely sure joins
    crude_djoins <- ai_crude_data_block_joins(basic_admap = admap0$map, d_dat = d_dat)
    if (crude_djoins$done) {
      d_dat <- crude_djoins$d_dat
      admap0 <- ai_get_data_attr_map(
        dat_boundary = d_dat$group_id_boundary,
        att_gid_map = d_att$group_id_map
      )
    }
  }


  # split attr gid relative to data_gid
  rel_chk <- ai_relative_data_split_attr(basic_map = admap0, d_att = d_att, d_dat = d_dat)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att %>% map(unique)
    admap0 <- rel_chk$admap
  }


  admap1_major_minor <- admap0$all_map %>%
    rename(attr_gid = gid, dist = md) %>%
    filter(direction_group != "corner") %>%
    ai_get_data_attr_map_details(d_dat, d_att)

  admap1_major_minor_compact <- admap1_major_minor$map %>%
    filter(attr_group == "major") %>%
    rename(md = dist) %>%
    group_by(data_gid, direction_group) %>%
    mutate(m_dist = min(md)) %>%
    ungroup() %>%
    filter(md == m_dist) %>%
    select(-md) %>%
    rename(dist = m_dist)

  admap0_pass0 <- admap1_major_minor_compact %>%
    filter(direction_group != "corner") %>%
    unique() %>%
    select(-attr_group)

  # dimension analysis done here (major minor classification)
  admap1 <- admap0_pass0 %>%
    ai_get_data_attr_map_details(d_dat, d_att)

  list(admap = admap1, d_dat = d_dat, d_att = d_att)
}
