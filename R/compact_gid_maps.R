
# diffrent split resulted in same attr_gid
# these will be compated to single group
compact_gid_maps <- function(gid_map, admap_main) {
  gid_map_raw <- gid_map$group_id_map

  foot_prints <- gid_map_raw %>%
    group_by(gid) %>%
    group_split() %>%
    map_df(~ .x %>%
      distinct(row, col, gid) %>%
      arrange(row, col) %>%
      summarise(gid = gid[1], fp = paste0(row, ",", col, collapse = ";")))

  ngmap <- foot_prints %>%
    group_by(fp) %>%
    mutate(new_gid = min(gid)) %>%
    ungroup() %>%
    distinct(gid, new_gid)

  gid_map_raw_new <- gid_map_raw %>% left_join(ngmap, by = "gid")
  gid_map_raw_new <- gid_map_raw_new %>%
    mutate(new_gid = if_else(is.na(new_gid), gid, new_gid)) %>%
    select(-gid) %>%
    rename(gid = new_gid) %>%
    distinct()

  gid_map$group_id_map <- gid_map_raw_new
  gid_map$group_id_boundary <- get_group_id_boundary(gid_map_raw_new)

  admap_main_raw_map_new <- admap_main$raw_map %>%
    left_join(ngmap %>% rename(attr_gid = gid, new_attr_gid = new_gid), by = "attr_gid")
  admap_main_raw_map_new <- admap_main_raw_map_new %>%
    mutate(new_attr_gid = if_else(is.na(new_attr_gid), attr_gid, new_attr_gid)) %>%
    select(-attr_gid) %>%
    rename(attr_gid = new_attr_gid) %>%
    distinct()

  admap_main$raw_map <- admap_main_raw_map_new

  admap_main$map <- admap_main$raw_map %>%
    distinct(attr_gid, data_gid, direction, direction_group, dist, attr_group)

  # dimesion analysis is not kept

  list(gid_map = gid_map, admap = admap_main)
}
