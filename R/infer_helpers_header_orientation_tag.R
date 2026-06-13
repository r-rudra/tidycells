

#' Infer HOT (header_orientation_tag) metric
#'
#' Computes a scaled fraction denoting the coverage (1 means full coverage) for the supplied direction.
#'
#' @param data_cells Data frame of data block cells (with data_gid)
#' @param header_cells Data frame of header/attribute cells (with attr_gid)
#' @param direction Direction name (should match one of those from infer_get_valid_header_orientation_tags())
#' @return A numeric value between 0 and 1: fraction of data_cells covered by the header in the given direction.
#' @keywords internal
infer_get_HOT_metric <- function(data_cells, header_cells, direction) {
  cov_count <- tryCatch(
    infer_get_HOT_metric_base(data_cells, header_cells, direction),
    error = function(e) 0
  )
  if (length(cov_count) != 1 || is.na(cov_count)) cov_count <- 0
  cov_count / NROW(data_cells)
}

# Coverage count for HOT metric (internal)
infer_get_HOT_metric_base <- function(data_cells, header_cells, direction) {
  # If header cell is a single cell or empty, treat all data cells as covered by
  # that header
  if (NROW(header_cells) <= 1) {
    return(NROW(data_cells))
  }
  # Otherwise, use bind_header to attach headers and count how many data cells are assigned a header in this direction
  attach_header(data_cells, header_cells, direction) %>%
    dplyr::filter(!is.na(.data$attr_gid)) %>%
    NROW()
}



# Adds each attribute-blocks a flag whether it is a full dimension or not. Also
# adds data_block wise longer_direction_group which may be required later to
# better naming of columns in compose stage (and which attribute lies on longer
# side). This is required in attaching header_orientation_tags.
infer_dimension_analysis_details <- function(admap, d_dat) {
  # Step 1: Compute dimensions for each data block (number of unique rows and columns)
  data_dims <- d_dat %>%
    dplyr::group_by(.data$data_gid) %>%
    dplyr::summarise(
      r_dim_data = dplyr::n_distinct(.data$row),  # Number of unique rows in data block
      c_dim_data = dplyr::n_distinct(.data$col),  # Number of unique columns in data block
      .groups = "drop"
    ) %>%
    # Calculate longer side (either NS or WE) based on r_dim and c_dim
    dplyr::mutate(
      longer_direction_group = dplyr::case_when(
        # North-South is longer if columns are more than rows otherwise
        # West-East is longer
        .data$r_dim_data <= .data$c_dim_data ~ "NS",
        TRUE ~ "WE"
      )
    )

  # Step 2: For each (attr_gid, data_gid) pair, compute intersection dimensions
  # and determine direction group

  # Overlapping rows (r_dim)
  r_overlap <- admap %>%
    dplyr::inner_join(d_dat, by = c("data_gid", "row")) %>%
    dplyr::group_by(.data$attr_gid, .data$data_gid) %>%
    dplyr::summarise(
      r_dim = dplyr::n_distinct(.data$row),
      direction_group = .data$direction_group[1],
      .groups = "drop"
    )

  # Overlapping columns (c_dim)
  c_overlap <- admap %>%
    dplyr::inner_join(d_dat, by = c("data_gid", "col")) %>%
    dplyr::group_by(.data$attr_gid, .data$data_gid) %>%
    dplyr::summarise(
      c_dim = dplyr::n_distinct(.data$col),
      direction_group = .data$direction_group[1],
      .groups = "drop"
    )

  # Combine (join) row and column overlaps
  attr_data_dim <- r_overlap %>%
    dplyr::full_join(c_overlap, by = c("attr_gid", "data_gid"),
                     suffix = c("_r", "_c")) %>%
    # Fill NA values for r_dim and c_dim and fix direction_group
    dplyr::mutate(
      r_dim = dplyr::coalesce(.data$r_dim, 0),  # Fill NA with 0
      c_dim = dplyr::coalesce(.data$c_dim, 0),  # Fill NA with 0
      # Use direction_group from either row or column
      direction_group = ifelse(
        is.na(.data$direction_group_r),
        .data$direction_group_c, .data$direction_group_r)
    ) %>%
    dplyr::select(
      "attr_gid", "data_gid", "r_dim", "c_dim",
      "direction_group"
    )

  attr_data_dim <- attr_data_dim %>%
    # Attach the data block dimensions for each pair
    dplyr::inner_join(data_dims, by = "data_gid") %>%
    # Step 3: Calculate relative dimension coverage for NS, WE, and corner direction groups
    dplyr::mutate(
      rel_dim = dplyr::case_when(
        .data$direction_group == "NS" ~ .data$c_dim / .data$c_dim_data,   # NS checks columns coverage
        .data$direction_group == "WE" ~ .data$r_dim / .data$r_dim_data,   # WE checks rows coverage
        .data$direction_group == "corner" ~ 0,                            # corner not considered full
        TRUE ~ 0
      ),
      # full_dim is TRUE if relative dimension coverage >= 1
      full_dim = .data$rel_dim >= 1
    ) %>%
    # Add is lying on longer side in short is_on_long_side
    dplyr::mutate(
      is_on_long_side = dplyr::case_when(
        .data$longer_direction_group == .data$direction_group ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Add "is_full_dim" and other measures to the admap_attr-wise
  out <- attr_data_dim %>%
    dplyr::distinct(
      .data$attr_gid, .data$data_gid,
      is_full_dim = .data$full_dim,
      is_on_long_side = .data$is_on_long_side,
      relative_dimension_fraction = .data$rel_dim
    ) %>%
    dplyr::right_join(admap, by = c("attr_gid", "data_gid"))

  return(out)
}

infer_get_valid_header_orientation_tags <- function(
    excel_orientation_tags_only = core_opt_get(
      "excel_specific_optimization", FALSE) ) {
  # This function returns the valid header orientation tags.
  # direction order
  # "v" "vl" "vr" "vm"
  # "h" "hu" "hd" "hm"
  # "v" "vl" "vr" "vm"
  # "h" "hu" "hd" "hm"

  # This is a great place to control multi sheet files and if all sheets are
  # similar and less number of direction has popped then search space can be
  # reduced.

  if(excel_orientation_tags_only){
    # Return orientation_tags which can come in excel for merged cells
    list(
      N = c("v", "vl"),
      W = c("h", "hu"),
      S = c("v", "vl"),
      E = c("h", "hu"),
      NE = "nothing", # vr does not come in Excel cell merging.
      NW = "vl",
      SE = "nothing", # vr does not come in Excel cell merging.
      SW = "vl"
    )

  } else {

    list(
      N = c("v", "vl", "vr", "vm"),
      W = c("h", "hu", "hd", "hm"),
      S = c("v", "vl", "vr", "vm"),
      E = c("h", "hu", "hd", "hm"),
      NE = "vr",
      NW = "vl",
      SE = "vr",
      SW = "vl"
    )

  }


}

infer_get_header_orientation_tag <- function(
    admap, d_dat,
    excel_specific_optimization = core_opt_get(
      "excel_specific_optimization", FALSE) ) {
  # 1. Get valid header orientation tag mappings
  header_tag_map <- infer_get_valid_header_orientation_tags()

  # 2. Select pairs with full dimension coverage in major directions (NS/WE)
  full_dim_pairs <- admap %>%
    dplyr::filter(.data$is_full_dim, .data$direction_group %in% c("NS", "WE"))

  # 3. Remaining pairs are not full coverage
  partial_dim_pairs <- admap %>%
    dplyr::anti_join(full_dim_pairs, by = c("data_gid", "attr_gid"))

  # 4. Assign tags for full coverage (N, W, S, E) using first listed tag in map
  cardinal_tags <- purrr::map_chr(header_tag_map[c("N", "W", "S", "E")], 1)
  full_dim_pairs$header_orientation_tag <- cardinal_tags[full_dim_pairs$direction]

  # Check if there is any partial_dim_pairs at all
  if( NROW(partial_dim_pairs) > 0) {
    # If there are partial pairs, we need to process them further

    # 5. For ambiguous (partial) cases, modify direction order (keeping v, h at
    # the last)
    header_tag_map_mod <- purrr::map(header_tag_map, function(tags) {
      # This is mainly for VH directions where v and h are kept in first
      # preference for infer_get_valid_header_orientation_tags() like list(N =
      # c("v", "vl", "vr", "vm"), W = c("h", "hu", ... etc. We reorder to keep
      # tags v and h at the last. As dirs[which.max(scores)][1] will pick as per
      # preference order. in infer_header_orientation_tag_for_pair()
      if(length(tags) > 1) c(tags[-1], tags[1]) else tags
    })

    if(excel_specific_optimization) {
      # If excel_specific_optimization is TRUE, we can further optimize the
      # direction tags to single one list(N = "vl", W = "hu", S = "vl", E =
      # "hu", NE = "nothing", NW = "vl", SE = "nothing", SW = "vl")
      #
      # Assuming that the direction tags are only for excel merged cells and
      # infer_get_valid_header_orientation_tags(excel_orientation_tags_only =
      # TRUE) is being used.
      #
      # This will avoid infer_get_HOT_metric calls for each direction
      header_tag_map_mod <- purrr::map(header_tag_map_mod, 1)
    }

    # 6. Prepare representative cell positions for data blocks and attribute groups
    data_block_reps <- d_dat %>%
      dplyr::filter(.data$data_gid %in% unique(partial_dim_pairs$data_gid)) %>%
      dplyr::distinct(.data$data_gid, .data$row, .data$col)

    # Here the data_gid representative (It reduces the data_gid cells to single cell per
    # row and col) gets computed
    data_block_reps <- infer_block_cell_representative(
      data_block_reps,
      type = "min")

    # Split data_block_reps by data_gid for easier access later
    data_block_reps <- split(data_block_reps, data_block_reps$data_gid)

    # It not like data_block_reps, but it is like actual attribute groups
    # retrieved again. Name may be misleading here.
    attr_group_reps <- partial_dim_pairs %>%
      dplyr::distinct(.data$attr_gid, .data$row, .data$col)
    # Split by attr_gid for easier access later
    attr_group_reps <-  split(attr_group_reps, attr_group_reps$attr_gid)

    # 7. Assign header_orientation_tag to each ambiguous group
    partial_dim_pairs_split <- split(
      partial_dim_pairs,
      # Not using dplyr::group_split as it is experimental as reported by dplyr
      # itself
      list(partial_dim_pairs$data_gid, partial_dim_pairs$attr_gid),
      drop = TRUE
    )
    partial_dim_pairs_annotated  <- partial_dim_pairs_split %>%
      purrr::map_dfr(function(group) {
        group %>%
          dplyr::mutate(
            header_orientation_tag = infer_header_orientation_tag_for_pair(
              data = group,
              data_block_reps = data_block_reps,
              attr_group_reps = attr_group_reps,
              direction_map = header_tag_map_mod
            )
          )
      })

  } else {
    # If there are no partial dimension pairs, we can skip this step
    partial_dim_pairs_annotated <- tibble::tibble()
  }

  # 8. Combine and return
  out <- dplyr::bind_rows(full_dim_pairs, partial_dim_pairs_annotated)
  return(out)
}

# Helper: assign tag for a (data_gid, attr_gid) group
infer_header_orientation_tag_for_pair <- function(
    data, data_block_reps, attr_group_reps, direction_map) {
  direction <- data$direction[1]
  attr_id <- data$attr_gid[1]
  data_id <- data$data_gid[1]
  dirs <- direction_map[[direction]]
  attr_rep <- attr_group_reps[[attr_id]]

  # If attribute group is a single cell, use "direct"
  if(NROW(attr_rep) == 1) return("direct")

  # If multiple directions, pick best by HOT metric, else use only available
  # direction
  if(length(dirs) > 1) {
    data_rep <- data_block_reps[[data_id]]

    # This method will calculate all dirs:
    #
    # scores <- purrr::map_dbl(dirs, ~infer_get_HOT_metric(data_rep, attr_rep, .x))
    # dirs[which.max(scores)][1]

    # The mapping functions in R (like lapply, map) do not short-circuit, but your
    # use of a mutable environment for state does let you stop computation for later
    # elements. The only remaining "waste" is that the mapping function is still
    # invoked for all elements, but the expensive calculation itself is avoided
    # after the threshold.

    # While this is not a short-circuit in the strictest sense, it does
    # effectively stop further calculations once the maximum is found. (for loop
    # can also be done here but it is not idiomatic)
    best_tag <- function(){
      # env_for_short_circuit
      env <- new.env()
      env$mx  <- 0
      scores <- purrr::map_dbl(dirs, function(dn) {
        if(env$mx < 1){
          env$mx <- infer_get_HOT_metric(data_rep, attr_rep, dn)
          env$mx
        }else{
          # Not calculating further as we have already found the maximum (1)
          0
        }
      })
      dirs[which.max(scores)][1]
    }

    # Run the function to get the best HOT (tag)
    best_tag()

  } else {
    # Use only available direction
    dirs[1]
  }
}

# Main function to infer header orientation tags
infer_attach_header_orientation_tag <- function(
    admap, d_dat,
    delinked_corner_attrs = FALSE) {

  # Special handling for de-linking corner attributes
  if(delinked_corner_attrs){
    admap_corners <- admap %>%
      dplyr::filter(.data$direction_group == "corner")
    # Remove corner attributes from admap
    admap <- admap %>%
      dplyr::filter(.data$direction_group != "corner")
  }

  # Single Cell Attribute Handling
  admap <- admap %>%
    dplyr::group_by(.data$attr_gid) %>%
    # Count distinct row and col of attribute cells
    dplyr::mutate(n_rc = dplyr::n_distinct(.data$row, .data$col)) %>%
    dplyr::ungroup()

  # For single cell attributes, we can directly assign the header orientation tag
  out1 <- admap %>%
    dplyr::filter(.data$n_rc == 1) %>%
    dplyr::mutate(
      header_orientation_tag = "direct",
      is_full_dim = FALSE,  # Single cell attributes are not full dimensions
      is_on_long_side = FALSE,  # This is to be fixed later as of now the best option is to set that it is not on long side
      relative_dimension_fraction = 0  # No dimension fraction for single cells
    ) %>%
    dplyr::select(-"n_rc")

  # For multi-cell attributes, we need to analyze further
  out2_part1 <- admap %>%
    dplyr::filter(.data$n_rc > 1) %>%
    dplyr::select(-"n_rc")

  if(NROW(out2_part1)>0){
    out2_part2 <- infer_dimension_analysis_details(out2_part1, d_dat)

    out2 <- infer_get_header_orientation_tag(out2_part2, d_dat)

    # Fix for Single cell attributes (is_on_long_side)
    if(any(out2$is_on_long_side) && NROW(out1)>0){
      long_short_dir_grp <- out2 %>%
        dplyr::filter(.data$is_on_long_side) %>%
        dplyr::group_by(.data$data_gid) %>%
        dplyr::summarise(ls_dg = .data$direction_group[1], .groups = "drop")

      out1 <- out1 %>%
        # Adds ls_dg column
        dplyr::left_join(long_short_dir_grp, by = "data_gid") %>%
        dplyr::mutate(is_on_long_side = dplyr::coalesce(.data$direction_group == .data$ls_dg, FALSE)) %>%
        dplyr::select(-"ls_dg")
    }

    out <- out1 %>% dplyr::bind_rows(out2)
  }else{
    out <- out1
  }

  if(delinked_corner_attrs){
    # For corner attributes, we can assign "nothing" as header_orientation_tag
    out_corner <- admap_corners %>%
      dplyr::mutate(
        header_orientation_tag = "nothing",
        is_full_dim = FALSE,  # Corner attributes are not full dimensions
        is_on_long_side = FALSE,  # Corner attributes are not on long side
        relative_dimension_fraction = 0  # No dimension fraction for corner attributes
      )
    # Combine corner attributes back to the main output
    out <- out %>% dplyr::bind_rows(out_corner)
  }

  return(out)
}


# This function is for assigning nice names to header so that better overall
# compose looks like.
infer_define_nice_header_names <- function(admap, d_dat) {
  # This function expects admap_attr-wise after
  # infer_attach_header_orientation_tag.

  # Split by each data-gid
  split(admap, admap$data_gid) %>%
    purrr::map_dfr(
      function(adm) {
        # For each data_gid, we can infer the nice header names
        d_dat_this <- d_dat %>%
          dplyr::filter(.data$data_gid == adm$data_gid[1])
        infer_define_nice_header_names_single(adm, d_dat_this)
      }
    )
}


# This function is for single cell data-block
infer_define_nice_header_names_single <- function(admap, d_dat) {

  # Take out attr gid and direction. We have to assign nice names to each
  # attr_gid. After doing so we can merge back to admap_attr-wise.
  d_attr_this <- admap %>%
    dplyr::group_by(.data$attr_gid) %>%
    dplyr::summarise(
      # These are required for meaningful names
      direction = .data$direction[1],
      direction_group = .data$direction_group[1],
      is_full_dim = .data$is_full_dim[1],
      relative_dimension_fraction = max(.data$relative_dimension_fraction),
      is_on_long_side = .data$is_on_long_side[1],
      dir_rc_mark = .data$dir_rc_mark[1],
      dist = min(.data$dist), .groups = "drop")


  # Design nice names for each attribute based on its parameters
  d_attr_this <- d_attr_this %>%
    dplyr::mutate(
      # Assign nice names based on direction and full dimension status

      # Part 1 : This part is based on direction_group and full_dim status. (VH
      # = Vertical/Horizontal) NS and WE direction full dim attributes are
      # considered high. Non full_dim attributes are considered mid. Corner
      # attributes are considered low.
      nice_header_name_pt1 = dplyr::case_when(
        # direction_group is on main axis (NS or WE) and full_dim
        (((.data$direction_group == "NS") | (.data$direction_group == "WE")) &
           .data$is_full_dim) ~ "high",
        # direction_group is on main axis (NS or WE) and not full_dim
        (((.data$direction_group == "NS") | (.data$direction_group == "WE")) &
           !.data$is_full_dim) ~ "mid",
        # direction_group is corner and for any full_dim
        TRUE ~ "low"
      ),

      # Part 2 : This part is based on whether the attribute is on long side
      # or not. If it is on long side, it is considered long otherwise short.
      nice_header_name_pt2 = dplyr::case_when(
        # is_on_long_side is TRUE
        .data$is_on_long_side ~ "long",
        # direction_group is corner and for any full_dim
        TRUE ~ "short"
      ),

      # Part 3 : Reuse some of the stats already calculated during
      # infer_attr_split -> split_tag and  infer_dimension_analysis_details. These
      # a) dir_rc_mark (obtained in infer_attr_split): which is side based row:col
      # character with 0:x and x:0 for VH directions. b)
      # relative_dimension_fraction (obtained in infer_dimension_analysis_details)
      # denotes the fraction of dimension covered by the attribute in the data
      # block (usually in range [0-1]). c) dist which is the distance of the
      # attribute from the data block (calculated long ago).
      #
      # The order is like: relative_dimension_fraction then dist then dir_rc_mark but within each
      # nice_header_name_pt1 and nice_header_name_pt2. So it is defined later.
      # nice_header_name_pt3 = [Defined later]

      # Part 4 : This is to distinguish between multiple attributes in the V or
      # H directions
      nice_header_name_pt4 = dplyr::case_when(
        # Normally N and W are very common
        .data$direction == "N" ~ "1",
        .data$direction == "W" ~ "1",
        # for S and E we can use 2
        TRUE ~ "2"
      )
    )


  # Now nice_header_name_pt3 has to be created based on re-indexing done on the
  # nice_header_name_pt1 and nice_header_name_pt2 as infer_attr_split ->
  # split_tag was not having this long short info.
  d_attr_this <- d_attr_this %>%
    dplyr::group_by(
      .data$nice_header_name_pt1, .data$nice_header_name_pt2) %>%
    dplyr::mutate(
      # Create a unique index for each combination of pt1 and pt2 based on
      # split_tag order
      nice_header_name_pt3 = util_hierarchical_rank(
        # opposite order for less relative_dimension_fraction rank will be more
        -.data$relative_dimension_fraction,
        .data$dist,
        .data$dir_rc_mark
      )
    ) %>%
    dplyr::ungroup()



  # Now we have to assign nice header names based on the parts we have

  # Special VH directional cases: NS and WE

  NS_fd_chk <- (d_attr_this %>%
                  dplyr::filter(.data$direction_group == "NS",
                                .data$is_full_dim) %>%
                  NROW())

  WE_fd_chk <- (d_attr_this %>%
                  dplyr::filter(.data$direction_group == "WE",
                                .data$is_full_dim) %>%
                  NROW())

  # NS_fd_chk && WE_fd_chk used later

  # Scenario: 1 We have only one full_dim attr in each direction NS and WE (best
  # situation) : NS_fd_chk == 1 && WE_fd_chk == 1

  # Scenario: 2 We have at least one full_dim attr in each direction NS and WE
  # (good situation) : NS_fd_chk >= 1 && WE_fd_chk >= 1

  # Scenario: 3 All other cases

  # The Name is nice_header_name_0 as duplicate/tie reduction may be required.
  # If nice_header_name_0 ensures unique Names then nice_header_name_0 is fine.
  # otherwise we can use nice_header_name_1 or nice_header_name_2 (subject to
  # modification later). If that is also not unique then we can use different
  # approach to make it unique.
  d_attr_this <- d_attr_this %>%
    dplyr::mutate(
      # Combine all parts to form the final nice header name
      nice_header_name_0 = paste0(
        .data$nice_header_name_pt1, "_",
        .data$nice_header_name_pt2, "_",
        # Use the index from pt3 to try uniqueness. Here we reset the order
        # though tie will be not broken.
        util_hierarchical_rank(.data$nice_header_name_pt3)
      ),
      # Define nice_header_name_1 and nice_header_name_2

      # At this stage both nice_header_name_0 and nice_header_name_1 are same
      nice_header_name_1 = .data$nice_header_name_0,

      # Last resort: nice_header_name_2
      nice_header_name_2 = paste0(
        .data$nice_header_name_pt1, "_",
        .data$nice_header_name_pt2, "_",
        # Use the index from pt3 & pt4 to try uniqueness. Here we reset the order
        # though tie will be not broken.
        util_hierarchical_rank(
          .data$nice_header_name_pt3, .data$nice_header_name_pt4
        )
      )

    )

  # Special Care for Scenario 1 & 2: If we have at least one full_dim attr in
  # each direction NS and WE, we can modify nice_header_name_0

  if(NS_fd_chk>=1 && WE_fd_chk>=1) {
    # If both NS and WE have only one full_dim attr, we modify nice_header_name_0
    d_attr_this <- d_attr_this %>%
      dplyr::mutate(
        nice_header_name_sc1 = paste0(
          .data$nice_header_name_pt1, "_",
          .data$nice_header_name_pt2
        )
      )

    # Modify nice_header_name_0, 1, 2 so that it goes through the uniqueness
    # check

    # Step 1 : Update nice_header_name_0
    d_attr_this <- d_attr_this %>% dplyr::mutate(
      nice_header_name_0 = dplyr::case_when(
        # If direction_group is NS and full_dim, use nice_header_name_sc1
        (.data$direction_group == "NS" & .data$is_full_dim) ~ .data$nice_header_name_sc1,
        # If direction_group is WE and full_dim, use nice_header_name_sc1
        (.data$direction_group == "WE" & .data$is_full_dim) ~ .data$nice_header_name_sc1,
        # Otherwise use nice_header_name_0
        TRUE ~ .data$nice_header_name_0
      )
    )

    # Step 2: update nice_header_name_1 and 2 per (nice_header_name_pt1, p2
    # group)
    d_attr_this <- d_attr_this %>%
      dplyr::group_by(
        .data$nice_header_name_pt1, .data$nice_header_name_pt2) %>%
      dplyr::mutate(

        # Modify nice_header_name_1 and nice_header_name_2 based on the new
        # nice_header_name_0
        nice_header_name_pt1 = paste0(
          .data$nice_header_name_0, "_",
          # Use the index from pt3 to try uniqueness. Here we reset the order
          # though tie will be not broken.
          util_hierarchical_rank(.data$nice_header_name_pt3)
        ),

        nice_header_name_pt2 = paste0(
          .data$nice_header_name_pt1, "_",
          # Use the index from pt3 & pt4 to try uniqueness. Here we reset the
          # order though tie will be not broken.
          util_hierarchical_rank(
            .data$nice_header_name_pt3, .data$nice_header_name_pt4
          )
        )
      ) %>%
      dplyr::ungroup()

  }


  # Now we have nice_header_name_0, nice_header_name_1 and nice_header_name_2
  # which are based on the direction, full_dim and is_on_long_side.
  # We check the uniqueness of these names and select the most unique one.

  # Check this uniqueness of nice_header_name_0 and nice_header_name_1
  if(dplyr::n_distinct(d_attr_this$nice_header_name_0) == NROW(d_attr_this)) {
    # If nice_header_name_0 is unique, use it
    d_attr_this <- d_attr_this %>%
      dplyr::select(
        "attr_gid",
        nice_header_name = "nice_header_name_0")
  } else if(dplyr::n_distinct(d_attr_this$nice_header_name_1) == NROW(d_attr_this)) {
    # If nice_header_name_1 is unique, use it
    d_attr_this <- d_attr_this %>%
      dplyr::select(
        "attr_gid",
        nice_header_name = "nice_header_name_1")
  } else if(dplyr::n_distinct(d_attr_this$nice_header_name_2) == NROW(d_attr_this)) {
    # If nice_header_name_2 is unique, use it
    d_attr_this <- d_attr_this %>%
      dplyr::select(
        "attr_gid",
        nice_header_name = "nice_header_name_2")
  } else {

    ucord <- c(
      dplyr::n_distinct(d_attr_this$nice_header_name_0),
      dplyr::n_distinct(d_attr_this$nice_header_name_1),
      dplyr::n_distinct(d_attr_this$nice_header_name_2)
    )

    # which.max will return single first element but here all maximum index are
    # required to pick the lowest index (which.max doe solve the purpose but it
    # is done explicitly)
    tar_idx <- min(which(ucord==max(ucord)))
    # tar_idx will be 1, 2 or 3 based on which of the nice_header_name_0,
    # nice_header_name_1 or nice_header_name_2 is more unique.
    # Based on tar_idx we can select the nice_header_name_0 etc.
    # As nice_header_name_x starts from 0 index, we need to subtract 1.
    tar_idx <- tar_idx-1

    # If neither is unique, we can use util_make_unique_minimal
    # Use util_make_unique_minimal to ensure uniqueness
    force_unique <- util_make_unique_minimal(
      # Select the appropriate nice_header_name_x based on tar_idx
      d_attr_this[[paste0("nice_header_name_", tar_idx)]]
    )

    # Make the order like _1, _2 from _1_1 etc
    unique_num_parts <- force_unique %>%
      stringr::str_remove_all("[a-z]") %>%
      stringr::str_remove_all("^_+")

    txt_parts <- force_unique %>%
      stringr::str_remove_all("[0-9]") %>%
      stringr::str_replace_all("_+","_") %>%
      stringr::str_remove_all("_$")

    # Since txt_parts is already separating the text parts, we have to take some
    # care getting ranks
    dnames <- tibble::tibble(
      txt = txt_parts,
      num = unique_num_parts
    )

    dnames <- dnames %>%
      dplyr::group_by(.data$txt) %>%
      dplyr::mutate(
        # This will ensure that the numbering is natural and ranks like 1, 2, 3
        # etc. (Within each text part)
        num_rnk = util_natural_segment_rank(.data$num),
        # This is the new name now
        new_names = paste0(
          .data$txt, "_", .data$num_rnk
        ) %>%
          stringr::str_remove_all("_$")
      )

    # After lots of troubles, we have the new names in dnames$new_names :-)
    d_attr_this$nice_header_name <- dnames$new_names
    d_attr_this <- d_attr_this %>%
      dplyr::select(
        "attr_gid", "nice_header_name")
  }


  # Delete it if present (this is safety)
  admap[["nice_header_name"]] <-NULL

  # Add it with admap_attr-wise and return
  admap %>%
    dplyr::left_join(
      d_attr_this,
      by = "attr_gid"
    )
}

