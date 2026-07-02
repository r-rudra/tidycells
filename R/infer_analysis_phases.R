


infer_analysis_phase_0_preprocess <- function(d) {

  # Check and halt if the input is not a valid cells object. Otherwise, proceed
  # with the classification
  if(isFALSE(core_cells_validation_end_use(d))){
    rlang::abort(
      "Malformed Cells - Halted Execution! Please fix the issues.",
      call = rlang::caller_env())
  }

  # Check PoA and PoV columns are present
  if (!utils::hasName(d, "PoA") || !utils::hasName(d, "PoV")) {
    rlang::abort(
      paste0(
        "The Cells-Values needs to be classified for value/attribute. ",
        "Please use `value_attribute_classify()` to classify the Cells first."
      ),
      call = rlang::caller_env()
    )
  }

  expected_cols <- c("row", "col", "data_type", "value", "type", "PoA", "PoV", "PoE")
  extra_cols <- setdiff(colnames(d), expected_cols)

  if(length(extra_cols)>0){
    d_chk <- as.data.frame(unclass(d))[extra_cols]
    d_chk2 <- d_chk[stats::complete.cases(d_chk), ]
    if(NROW(d_chk2) < NROW(d_chk)){
      message("Extra columns with missing values detected. These columns will be discarded.")
      d <- d[setdiff(colnames(d), extra_cols)]
    } else {
      message("Extra columns detected. You may wish to discard them to prevent unintended interactions.")
    }
  }

  # Remove any NA cases
  d <- d[stats::complete.cases(d), ]

  # Probability of being Empty
  d$PoE <- 1 - (d$PoA + d$PoV)
  # Discard sure empty cells
  d <- d[d$PoE < 1 , ]

  # The list to return for next phase
  l <- list(orig = d)

  # Note: the term 'data' and 'value' are interchangeably used going forward.

  # Note: At a later stage, dynamic value–attribute classification can
  # potentially be introduced. This would involve, first, adjusting the
  # classification threshold; next, applying a complex method for
  # value–attribute classification with a dynamic threshold approach; and
  # finally, using pattern-based clustering of blocks and direct cell-level
  # inspection to revise the PoA and PoV.
  l$d_dat <- d[d$PoV >= 0.5, ] %>% core_as_rc_df()
  l$d_att <- d[d$PoA >= 0.5, ] %>% core_as_rc_df()

  if (NROW(l$d_dat) == 0) {
    rlang::abort(
      "No `value` cells found! Nothing to do!",
      call = rlang::caller_env())
  }

  if (NROW(l$d_att) == 0) {
    rlang::abort(
      "No `attribute` cells found! Nothing to do!",
      call = rlang::caller_env())
  }

  # Rename the data and attribute groups for clarity
  l$d_dat <- l$d_dat %>% infer_table_blocks(group_id_tag = "d") %>%
    dplyr::rename(data_gid = "gid")

  l$d_att <- l$d_att %>% infer_table_blocks(group_id_tag = "a") %>%
    dplyr::rename(attr_gid = "gid")

  return(l)
}


# This is the most vital phase of the analysis. It creates the fundamental
# attribute-data map (AdMap) which is used in subsequent phases to infer
# attribute clusters, header orientation tags, and others.
infer_analysis_phase_1_admap <- function(
    l0,
    do_L_shape_imposition = TRUE,
    do_initial_data_block_merging = TRUE) {
  # Phase 1: Attribute Data Map (AdMap) Creation
  # This phase creates a mapping of attributes to their respective data blocks.

  d_dat <- l0$d_dat
  d_att <- l0$d_att


  # Stage 1 of Phase 1: Preliminary L Shape Imposition
  if(do_L_shape_imposition){
    # At this stage d_dat and d_att both are subject to modifications.
    stage_0_L <- infer_preliminary_L_shape_imposition(d_dat, d_att)
    d_dat <- stage_0_L$d_dat
    d_att <- stage_0_L$d_att
  }

  # Stage 1 of Phase 2: Preliminary Data Block Merging (before AD-Map creation)
  if(do_initial_data_block_merging){
    # Note sequence matters here. The preliminary data block merging is done
    # before the AD-map creation and after the L-shape imposition.

    # This is to merge the data blocks that are fragmented and can be merged
    # even before AD-map creation.
    stage_0 <- infer_preliminary_data_block_merging(d_dat, d_att)
    d_dat <- stage_0$d_dat
  }

  # Create the AdMap by matching attributes to data cells
  #
  # Main objective here is to connect data blocks with attributes and connect
  # fragmented data-blocks.
  stage_1 <- infer_major_direction_stats(d_dat, d_att)

  d_dat <- stage_1$d_dat
  admap1 <- stage_1$ad_map

  # Remaining attributes that are not connected to any data those attributes
  # are to be connected to the data blocks. Here it is done.
  admap1_2 <- infer_minor_direction_stats(d_dat, d_att, admap1)
  admap2 <- dplyr::bind_rows(admap1, admap1_2) %>% dplyr::distinct()

  # Return the final AdMap and the data/attribute blocks for next phases
  return(
    list(
      # This AD-map is proper AD-Map. Means it has (data, attr) wise mapping and
      # no cell address.
      admap = admap2,
      d_dat = d_dat,
      d_att = d_att
    )
  )

}


infer_analysis_phase_2_attr_cluster <- function(l1) {

  # Here on-wards we take out implied_surrounding_blocks or info_blocks and
  # since that has both mapping and data attribute cells address (row, col) we
  # can use that to infer the clusters of attributes and data blocks.
  # impl_blocks or implied_surrounding_blocks or info_blocks are used
  # interchangeably.
  info_blocks <- infer_implied_surrounding_blocks(l1$admap, l1$d_dat, l1$d_att)

  # Include attr-sections which are within boundaries of
  # implied_surrounding_blocks. This is done to ensure that the
  # implied_surrounding_blocks are not fragmented and all the attributes that
  # are within the boundaries of implied_surrounding_blocks are included in the
  # info_blocks.
  info_blocks_expanded <-
    infer_engulf_attrs_in_implied_surrounding_blocks(info_blocks, l1$d_att)

  attr_split <- infer_attr_split(info_blocks_expanded)

  return(
    list(
      # This AD-Map is Attribute wise AD-Map. Means it has all the attributes
      # cell address (row, col) but only pointer to data-gid. In a way it is
      # d_att and admap of previous functions. going forward (for rest of
      # modules - header orientation tagging etc.) this type of admap (attr-wise
      # admap will be used interchangeably with admap)
      admap = attr_split
      # Since d_dat is not modified in this phase, we can reuse earlier phase
      # directly in top level functions.
      # d_dat = l1$d_dat # hence not including d_dat here
    )
  )

}


infer_analysis_phase_3_header_orientation_tag <- function(
    l2,
    # If this is TRUE, then the final output will only retain the required
    # columns, otherwise it will retain all the columns that are generated in
    # this phase.
    retain_required_only = TRUE,
    # If TRUE, then the nice names will be derived for the header cells. This
    # may be required if composing cells is what the output is used for. If
    # collate_columns is used then perhaps it is not required.
    derive_nice_names = core_opt_get("derive_nice_names_for_attributes", TRUE),
    # If TRUE, then the corner headers will be de-linked from data blocks.
    delink_corner_headers = core_opt_get("delink_corner_headers", FALSE)
) {

  # HOT: Header Orientation Tag

  # This step adds few more columns: "header_orientation_tag" "is_full_dim" and
  # "is_on_long_side", "relative_dimension_fraction"
  with_HOT <- infer_attach_header_orientation_tag(
    l2$admap, l2$d_dat,
    delinked_corner_attrs = delink_corner_headers)

  if(derive_nice_names) {
    # If derive_nice_names is TRUE, then infer nice names for the headers
    # This will add a column "nice_header_name" to the data frame
    with_nice_names <- infer_define_nice_header_names(with_HOT, l2$d_dat)
  } else {
    # If derive_nice_names is FALSE, then we just retain the original data
    # without adding nice names
    with_nice_names <- with_HOT
    # Default nice_header_name is attr_gid
    with_nice_names$nice_header_name <- with_nice_names$attr_gid
  }


  if(retain_required_only) {
    # Retain only the necessary columns for the final output
    with_nice_names <- with_nice_names %>%
      dplyr::mutate(
        # This is to reduce names of header_orientation_tag
        header_orientation_tag = as.character(.data$header_orientation_tag)
      ) %>%
      dplyr::distinct(
        .data$data_gid, .data$attr_gid,
        .data$row, .data$col, .data$header_orientation_tag,
        .data$nice_header_name
      )
  }

  list(
    # tagged is AD-map but attr-wise (d_att+admap)
    tagged = with_nice_names
    # d_dat is not modified in this phase, so we can reuse earlier phase
    # directly in top level functions.
    # d_dat = l2$d_dat # hence not including d_dat here
  )
}


infer_analysis_phase_4_postprocess <- function(
    l3,
    original_cells,
    single_data_chunk_filter =
      core_opt_get( "single_data_chunk_filter", FALSE),
    single_data_chunk_value_capture_threshold =
      core_opt_get( "single_data_chunk_value_capture_threshold", 0.75)) {

  o1 <- list(
    attr_data_map = l3$tagged,
    data_blocks = l3$d_dat,
    original_sheet = original_cells
  )

  # Post - Processing Filters and Fine Tuning (these are systematic post-processing)
  if(single_data_chunk_filter){
    # This checks if there is single large chunk of data (most usual case)
    schk <- o1$data_blocks |>
      dplyr::left_join(
        o1$original_sheet |>
          dplyr::select("row", "col", "value"),
        by = c("row","col")) |>
      dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)))

    schk <- schk |>
      dplyr::group_by(.data$data_gid) |>
      dplyr::summarise(nv = sum(!is.na(.data$value)), .groups = "drop")

    schk <- schk |>
      dplyr::mutate(v_frac = .data$nv / sum(.data$nv))

    if(any(schk$v_frac > single_data_chunk_value_capture_threshold)){
      # if this cross beyond certain fraction then rest data gids may be dropped (user demanded - not a general case)
      schk <- schk |> dplyr::filter(.data$v_frac > single_data_chunk_value_capture_threshold)
      o1$attr_data_map <- o1$attr_data_map |>
        dplyr::filter(.data$data_gid %in% schk$data_gid)

      o1$data_blocks <- o1$data_blocks |>
        dplyr::filter(.data$data_gid %in% schk$data_gid)

      o2 <- list(d_dat = o1$data_blocks, admap = o1$attr_data_map) |>
        infer_relabel_data_group_id()

      o1$attr_data_map <- o2$admap
      o1$data_blocks <- o2$d_dat
      o1$post_process <- unique(c(o1$post_process, "single_data_chunk_filter - applied"))
    }

  }

  obj <- o1

  # Assign the 'core_cells_analysis_class' class to the object
  class(obj) <- core_cells_analysis_class

  obj

}
