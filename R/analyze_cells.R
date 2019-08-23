


#' Analyze Cells
#'
#' @description After [`Value Attribute Classification`][value_attribute_classify()] done on a [`cell_df`][cell_df-class] next task to do is
#' analyze it's contents for data block detection, attribute orientation identification etc. The function `analyze_cells` (and also `analyse_cells`)
#' does the same for you.
#'
#' **Note**:
#' \if{html}{If you are not sure about what package functions actually do or how they work together,
#' please start with \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.}
#' \if{latex}{If you are not sure about what package functions actually do or how they work together,
#' please start with \code{vignette("tidycells-intro")}.}
#'
#' @param d A [`cell_df`][cell_df-class] after [`Value Attribute Classification`][value_attribute_classify()] done
#' @param silent logical scalar indicating whether to raise a warning if heuristic detection fails. (Default TRUE).
#'
#' @details it returns detailed analysis of the data structure including data block detection, attribute orientation detection etc.
#' The argument `silent` is set to `TRUE` by default, as the warning will be given whenever the [`cell_analysis`][cell_analysis-class] is printed.
#'
#' After this step one may like to do :
#' * [`compose_cells`][compose_cells()]
#'
#' If in an interactive session, following additional functions can be helpful for interactive visualizations:
#' * [`visual_data_block_inspection`][visual_data_block_inspection()]
#' * [`visual_orientation_modification`][visual_orientation_modification()]
#' * [`visual_traceback`][visual_traceback()]
#'
#' @return Detailed analysis of the cell data structure.
#' Which will be a [`cell_analysis`][cell_analysis-class] class object.
#'
#' @seealso [`compose_cells`][compose_cells()], [`collate_columns`][collate_columns()]
#' @export
#' @examples
#' d <- structure(c(
#'   "block 1", "", "C", "D", "", "block 2", "", "C",
#'   "D", "", "A", "1", "2", "", "", "A", "10", "20", "", "B", "3",
#'   "4", "", "", "B", "30", "40"
#' ), .Dim = c(9L, 3L))
#' d <- as.data.frame(d)
#' cd <- as_cell_df(d) %>% numeric_values_classifier()
#'
#' # see it
#' cd %>% plot(adaptive_txt_size = FALSE)
#' ca <- analyze_cells(cd)
#'
#' # look at the plot for detected directions
#' plot(ca)
analyze_cells <- function(d, silent = TRUE) {
  analyze_cells_raw(d = d, silent = silent)
}


analyze_cells_raw <- function(d, silent = TRUE) {
  if (!is_cell_df(d)) {
    abort("A Cell DF expected")
  }

  if (!hasName(d, "type")) {
    abort(paste("The type column not found.",
      "(You may like to do 'Value Attribute Classification'.",
      "Check basic_classifier, sample_based_classifier, numeric_values_classifier for details.",
      sep = "\n"
    ))
  }

  val <- validate_cells(d)
  if (!val) {
    abort(attr(val, "msg") %>% paste0(collapse = "\n"))
  }

  #  remove empty cells
  d_orig <- d
  d <- d %>% filter(type != "empty")

  data_cells <- d %>%
    filter(type == "value") %>%
    as_rc_df()

  attr_cells <- d %>%
    filter(type == "attribute") %>%
    as_rc_df()

  if (nrow(data_cells) == 0) {
    abort("No `value` cells found")
  }

  if (nrow(attr_cells) == 0) {
    abort("No `attribute` cells found")
  }

  d_dat_orig <- get_group_id(data_cells)
  d_att_orig <- get_group_id(attr_cells)

  d_dat <- d_dat_orig
  d_att <- d_att_orig

  setp1 <- ai_get_data_attr_map_main(d_dat, d_att)

  d_dat <- setp1$d_dat
  d_att <- setp1$d_att
  admap1 <- setp1$admap

  # data_gid join (if possible)
  if (nrow(d_dat$group_id_boundary) > 1) {
    d_dat0 <- ai_data_gid_join(d_dat,
      data_attr_map = admap1$map,
      full_data = d
    )
    if (!identical(d_dat0, d_dat)) {
      # this means results has been invalidated
      d_dat <- d_dat0

      # revert back to original form
      d_att <- d_att_orig

      setp2 <- ai_get_data_attr_map_main(d_dat, d_att, crude_join = FALSE)

      d_dat <- setp2$d_dat
      d_att <- setp2$d_att
      admap1 <- setp2$admap
    }
  }

  # join attr based on block merges possible
  rel_chk <- ai_relative_data_join_attr(admap_main = admap1, d_att = d_att)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att %>% map(unique)
    admap1 <- rel_chk$admap
  }

  #  now time for corners (potential)
  #  extend data block to include major (NS and WE) attributes
  d_dat$group_id_extended_boundary <- extend_data_block(d_dat$group_id_boundary, admap1$map, d_att)

  unmapped_attr_gids <- d_att$group_id_boundary$gid %>%
    setdiff(admap1$map$attr_gid) %>%
    setdiff(d_att$missed_blocks$gid)

  # fc: for corners
  admap_fc0 <- ai_get_data_attr_map(
    dat_boundary = d_dat$group_id_extended_boundary,
    att_gid_map = d_att$group_id_map %>% filter(gid %in% unmapped_attr_gids),
    attr_to_near_data = TRUE,
    leave_inside = TRUE
  )
  unmapped_attr_gids <-
    admap1$map$attr_gid %>%
    c(admap_fc0$map$attr_gid) %>%
    setdiff(d_att$group_id_boundary$gid, .) %>%
    setdiff(d_att$missed_blocks$gid)

  admap_fc1 <- admap_fc0$map %>%
    ai_get_data_attr_map_details(d_dat, d_att, major_direction_relax = FALSE)

  # try to attach rest attr_gid if any to nearest data_gid [on data_gid boundary]
  if (length(unmapped_attr_gids) > 0) {
    admap_other0 <- ai_get_data_attr_map(
      dat_boundary = d_dat$group_id_boundary,
      att_gid_map = d_att$group_id_map %>% filter(gid %in% unmapped_attr_gids),
      attr_to_near_data = TRUE
    )
    unmapped_attr_gids <-
      unmapped_attr_gids %>%
      setdiff(admap_other0$map$attr_gid)

    admap_other1 <- admap_other0$map %>%
      ai_get_data_attr_map_details(d_dat, d_att, major_direction_relax = FALSE)

    admap_fc1 <- merge_admaps(admap_fc1, admap_other1)
  }


  # last stage of analysis
  # this is not required
  # d_dat$group_id_whole_boundary <- extend_data_block(d_dat$group_id_extended_boundary, admap_fc1$map, d_att)

  admap2 <- merge_admaps(admap1, admap_fc1)

  # join attr based on block merges possible (one more time)
  rel_chk <- ai_relative_data_join_attr(admap_main = admap2, d_att = d_att)
  if (rel_chk$done) {
    d_att <- rel_chk$d_att %>% map(unique)
    admap2 <- rel_chk$admap
  }

  cmp <- compact_gid_maps(d_att, admap2)
  d_att <- cmp$gid_map
  admap2 <- cmp$admap

  admap3 <- admap2$map %>%
    select(-attr_group) %>%
    ai_get_data_attr_map_details(d_dat, d_att)

  if (!identical(admap3$map, admap2$map)) {
    # I think this can be iterated
    # KFL
    admap3_pass <- admap3$map %>%
      rename(md = dist) %>%
      group_by(data_gid, direction_group, attr_group) %>%
      mutate(m_dist = min(md)) %>%
      ungroup() %>%
      filter(md == m_dist) %>%
      select(-md) %>%
      rename(dist = m_dist)

    admap <- admap3_pass %>%
      select(-attr_group) %>%
      ai_get_data_attr_map_details(d_dat, d_att)
  } else {
    admap <- admap3
  }

  # once admap is done
  d_dat$group_id_extended_boundary <- NULL
  d_dat$group_id_whole_boundary <- extend_data_block(d_dat$group_id_boundary, admap$map, d_att)


  # str-detection done
  this_cells <- get_cells_from_admap(admap, d_dat, d_att)

  # natural gid for easier understanding
  gid_ngid <- d_dat$group_id_map %>%
    distinct(gid) %>%
    mutate(natural_gid = gid %>% as.factor() %>% as.numeric())

  # attach natural gid
  this_cells <- this_cells %>%
    left_join(gid_ngid, by = "gid")
  admap$raw_map <- admap$raw_map %>%
    mutate(gid = data_gid) %>%
    left_join(gid_ngid, by = "gid")
  admap$map <- admap$map %>%
    mutate(gid = data_gid) %>%
    left_join(gid_ngid, by = "gid") %>%
    select(-gid)
  d_dat$group_id_whole_boundary <- d_dat$group_id_whole_boundary %>%
    left_join(gid_ngid, by = "gid")

  # attach directions to it
  admap$raw_map <- ai_attach_direction(admap$raw_map)

  df_details <- get_definiteness_details(admap$raw_map,
    all_attr_gids = d_att$group_id_boundary$gid %>%
      setdiff(d_att$missed_blocks$gid)
  )
  definiteness_checks <- get_definiteness_checks(df_details, silent = silent)

  obj <- list(
    cells = this_cells,
    sections = d_dat$group_id_whole_boundary,
    details = list(
      attr_details = d_att,
      data_details = d_dat,
      data_attr_map_raw = admap$raw_map,
      definiteness_checks = definiteness_checks
    ),
    cell_df = d_orig
  )

  # attach cell_df_analysis class
  class(obj) <- cell_df_analysis_class

  obj
}
