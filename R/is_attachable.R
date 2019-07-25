
is_attachable <- function(gid1, gid2, group_info, whole_data, data_attr_map) {

  # should have similar major sides (of attributes)
  if (!identical(
    data_attr_map %>% filter(data_gid == gid1) %>% pull(direction) %>% sort(),
    data_attr_map %>% filter(data_gid == gid2) %>% pull(direction) %>% sort()
  )) {
    return(FALSE)
  }

  # if any intersecting cell present
  gc1 <- group_info$group_id_map %>%
    filter(gid == gid1) %>%
    select(row, col)
  gc2 <- group_info$group_id_map %>%
    filter(gid == gid2) %>%
    select(row, col)
  gcj <- gc1 %>% inner_join(gc2, by = c("row", "col"))
  if (nrow(gcj) > 0) {
    return(TRUE)
  }

  # should have no other entry within the enclosed combined boundary
  this_group_info <- group_info
  this_group_info$group_id_map <- this_group_info$group_id_map %>% filter(gid %in% c(gid1, gid2))
  this_group_info <- get_group_id_join_gids(
    this_group_info,
    tibble(gid = c(gid1, gid2), new_gid = gid1)
  )
  combined_boundary <- this_group_info$group_id_boundary
  this_region_data <- whole_data %>%
    filter(
      row <= combined_boundary$r_max,
      row >= combined_boundary$r_min,
      col <= combined_boundary$c_max,
      col >= combined_boundary$c_min
    )
  this_region_data_rest <- this_region_data %>%
    anti_join(this_group_info$group_id_map, by = c("row", "col")) %>%
    filter(type %in% c("value", "attribute"))

  if (nrow(this_region_data_rest) > 0) {
    return(FALSE)
  }

  return(TRUE)
}
