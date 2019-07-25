ai_data_gid_join <- function(d_dat, data_attr_map, full_data) {
  repeat({
    data_gid_comb <- d_dat$group_id_map$gid %>%
      unique() %>%
      utils::combn(2) %>%
      as.data.frame(stringsAsFactors = FALSE)

    data_gid_comb_chk <- data_gid_comb %>%
      map_lgl(~ is_attachable(
        gid1 = .x[1], gid2 = .x[2],
        group_info = d_dat, whole_data = full_data,
        data_attr_map = data_attr_map
      ))
    if (any(data_gid_comb_chk)) {
      data_gid_joins <- data_gid_comb[data_gid_comb_chk]

      data_gid_join_map <- get_links_df(data_gid_joins)
      d_dat <- get_group_id_join_gids(d_dat, gid_map = data_gid_join_map)
    } else {
      break()
    }
  })
  d_dat
}
