
get_definiteness_checks <- function(definiteness_details, silent = TRUE) {
  definiteness_checks <- list(
    # each gid (attr) should map to only one data_gid (this can happen)
    gid_att = nrow(definiteness_details$gid_att) == 0,
    # all data id should have at least one NS and WE attr_gid
    # (this also can happen then data_gid joining maybe required)
    gid_data = nrow(definiteness_details$gid_data) == 0,
    all_gid_att = length(definiteness_details$all_gid_att) == 0
  )

  if (!definiteness_checks$gid_att) {
    attr(definiteness_checks$gid_att, "msg") <-
      "At least one attribute group-id (gid) is mapped to more than one data_gid (this can happen for overlapped blocks)"
  }

  if (!definiteness_checks$gid_data) {
    attr(definiteness_checks$gid_data, "msg") <-
      "At least one data group-id (gid) is lacking either NS or WE direction_group attirbute (this means 2D data block has only 1D attribute)"
  }

  if (!definiteness_checks$all_gid_att) {
    attr(definiteness_checks$all_gid_att, "msg") <-
      "At least one attribute group-id (gid) is not mapped with any block"
  }

  if (!all(unlist(definiteness_checks))) {
    if (!silent) {
      warn("Heuristic data detection may not be correct. Look at definiteness_checks.")
    }
  }

  definiteness_checks
}


get_definiteness_details <- function(admap_raw, all_attr_gids) {
  unmapped_attr_gids <- setdiff(all_attr_gids, admap_raw$attr_gid)

  this_admap <- admap_raw %>% distinct(attr_gid, data_gid, direction_group)

  definiteness_details <- list(
    # each gid (attr) should map to only one data_gid (this can happen)
    gid_att = this_admap %>%
      count(attr_gid) %>% filter(n > 1),
    # all data id should have at least one NS and WE attr_gid
    # (this also can happen then data_gid joining maybe required)
    gid_data = this_admap %>%
      group_by(data_gid) %>%
      summarise(ch = all(c("NS", "WE") %in% direction_group)) %>%
      filter(!ch),
    all_gid_att = unmapped_attr_gids
  )

  definiteness_details
}
