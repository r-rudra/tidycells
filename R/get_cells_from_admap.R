
get_cells_from_admap <- function(admap, d_dat, d_att) {
  cells <- d_dat$group_id_map %>%
    select(gid, row, col) %>%
    mutate(cell_group_type = "data")

  cells <- admap$map %>%
    mutate(cell_group_type = paste0(attr_group, "_attr")) %>%
    select(attr_gid, data_gid, cell_group_type) %>%
    left_join(d_att$group_id_map, by = c("attr_gid" = "gid")) %>%
    select(gid = data_gid, row, col, cell_group_type) %>%
    bind_rows(cells) %>%
    distinct()

  cells
}
