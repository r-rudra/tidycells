
##
## Infer Helper Functions
##

# Section: Table Blocks ----


#' Connected-Component Grouping for 4-Connected Grid Cells
#'
#' Identifies all 4-connected groups (components) in a data frame of grid cells.
#' Two cells are considered part of the same group if they are connected via a
#' path of horizontal or vertical (not diagonal) adjacency. Each group receives
#' a unique group id (`gid`).
#'
#' The algorithm applies an iterative label-propagation approach, a variant of
#' the classic Connected-Component Labeling (CCL) algorithm from image
#' processing and graph theory. Labels are propagated among adjacent cells via
#' repeated group-wise minimum assignments until all connected cells share the
#' same id.
#'
#' ## Underlying Theory This approach is a form of Connected-Component Labeling
#' (CCL), widely used in image analysis to find and label contiguous regions
#' (components) in an array or grid. The algorithm used here is an iterative
#' label-propagation method, which merges labels for horizontally and vertically
#' adjacent runs until each component is uniquely identified. See:
#' - https://en.wikipedia.org/wiki/Connected-component_labeling
#' - Gonzalez, R. C., & Woods, R. E. (2008). Digital Image Processing (3rd Ed.), Section 2.5.3.
#'
#' ## Steps Overview
#'
#' | Step      | What it does                                                      | Why                                                           |
#' |-----------|-------------------------------------------------------------------|---------------------------------------------------------------|
#' | Row runs  | Finds horizontally connected contiguous cells; assigns `cid`      | To group cells horizontally                                   |
#' | Col runs  | Finds vertically connected contiguous cells; assigns `rid`        | To group cells vertically                                     |
#' | Propagate | Iteratively spreads minimum label among connected runs            | To ensure all 4-connected cells are merged into one component |
#' | Stop      | Ends when no further label changes                                | At this point, grouping is complete                           |
#'
#' @param dat A `data.frame` or `tibble` with integer columns `'row'` and
#'   `'col'`.
#' @param retain_cells_info If `TRUE`, retains the original `cells` class and
#'   structure, allowing for further processing. If `FALSE`, returns a simple
#'   `data.frame` with 'row', 'col', and 'gid' columns only.
#' @param group_id_tag A string prefix for the group id (`gid`) column. Default
#'   is `"Gr_"` if `retain_cells_info` is `TRUE`. This is useful for
#'   distinguishing group ids from other ids in the data frame.
#'
#' @return The input data frame with an added character column 'gid' indicating
#'   component membership.
#' @keywords internal
infer_table_blocks <- function(
    dat, retain_cells_info = FALSE, group_id_tag = NULL) {


  if(!retain_cells_info) {
    # Remove `cells`-class
    dat <- tibble::as_tibble(unclass(dat))

    dat <- dat[, c("row", "col")]
  }

  # 1. Find the number of digits to safely combine row/col into unique keys
  max_val <- max(dat$row, dat$col)
  digit_sep <- nchar(as.character(max_val)) + 1

  # 2. For each row: assign a 'cid' to each contiguous run of columns
  dat <- dat %>%
    dplyr::arrange(.data$row, .data$col) %>%
    dplyr::group_by(.data$row) %>%
    dplyr::mutate(col_run = c(0, cumsum(diff(.data$col) != 1))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cid = .data$col_run + .data$row * 10^digit_sep)

  # 3. For each column: assign a 'rid' to each contiguous run of rows
  dat <- dat %>%
    dplyr::arrange(.data$col, .data$row) %>%
    dplyr::group_by(.data$col) %>%
    dplyr::mutate(row_run = c(0, cumsum(diff(.data$row) != 1))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rid = .data$row_run + .data$col * 10^digit_sep)

  # 4. Initialize group id as rid.
  dat$gid <- dat$rid

  # 5. Iteratively propagate the minimum gid over all connected cells
  repeat {
    old_gid <- dat$gid
    # Propagate min gid within each 'cid' (row-wise contiguous segments)
    dat <- dat %>%
      dplyr::group_by(.data$cid) %>%
      dplyr::mutate(gid = min(.data$gid)) %>%
      dplyr::ungroup()
    # Propagate min gid within each 'rid' (col-wise contiguous segments)
    dat <- dat %>%
      dplyr::group_by(.data$rid) %>%
      dplyr::mutate(gid = min(.data$gid)) %>%
      dplyr::ungroup()
    # Stop if all gids have stabilized (no more merges)
    if (all(dat$gid == old_gid)) break
  }

  if(retain_cells_info && is.null(group_id_tag)) {
    # Default group id tag if not provided in case of `retain_cells_info = TRUE`
    group_id_tag <- "Gr_"
  }

  if(is.null(group_id_tag)){
    dat$gid <- as.character(dat$gid)
  }else{
    # Add group id tag to gid
    dat$gid <- paste0(
      group_id_tag,
      # Convert gid natural sequence like 1,2,3 etc
      util_hierarchical_rank(dat$gid)
    )
  }

  # Return
  if(!retain_cells_info){
    dplyr::distinct(dat[, c("row", "col", "gid")])
  }else{
    dplyr::select(
      dat,
      -c("cid", "rid", "col_run", "row_run")) %>%
      # Ensure the class is 'cells'
      core_ensure_cells_format()
  }
}




#' Weighted Percentile Boundary of Grid Cells (blocks)
#'
#' Calculates the bounding box covering a specified proportion of filled cells,
#' using weighted percentiles of cell counts.
#'
#' @param df Data frame with 'row' and 'col' columns. (Optionally grouped)
#' @param coverage Proportion to cover, default is 0.95 (95% block).
#' @param exact If `TRUE`, uses exact min/max instead of percentiles. Default
#'   is `TRUE`.
#'
#' @return tibble: r_min, r_max, c_min, c_max. (group vars if `df` is grouped)
#' @keywords internal
infer_block_boundary <- function(df, coverage = 0.95, exact = TRUE) {
  boundary_fn <- function(rows, cols, coverage, exact) {
    if (exact) {
      r_min <- min(rows)
      c_min <- min(cols)
      r_max <- max(rows)
      c_max <- max(cols)
    } else {
      wtd_quantile <- function(x, w, probs) {
        ord <- order(x)
        x <- x[ord]
        w <- w[ord]
        cum_w <- cumsum(w) / sum(w)
        sapply(probs, function(p) {
          idx <- which(cum_w >= p)[1]
          x[idx]
        })
      }
      row_counts <- as.data.frame(table(rows), stringsAsFactors = FALSE)
      col_counts <- as.data.frame(table(cols), stringsAsFactors = FALSE)
      half_gap <- (1 - coverage) / 2
      probs <- c(half_gap, 1 - half_gap)
      rp <- wtd_quantile(as.integer(row_counts[[1]]), row_counts$Freq, probs)
      cp <- wtd_quantile(as.integer(col_counts[[1]]), col_counts$Freq, probs)
      r_min <- rp[1]
      r_max <- rp[2]
      c_min <- cp[1]
      c_max <- cp[2]
    }
    tibble::tibble(
      r_min = r_min,
      r_max = r_max,
      c_min = c_min,
      c_max = c_max
    )
  }

  dplyr::summarise(
    df,
    boundary = list(boundary_fn(.data$row, .data$col, coverage, exact)),
    .groups = "drop"
  ) %>%
    tidyr::unnest_wider(
      "boundary",
      names_repair = "minimal",
      simplify = TRUE
    )
}

# Infer Block Boundary Cells
infer_block_boundary_cells <- function(bd) {

  # Handle fractional block-boundaries by ensuring r_min, r_max, c_min, c_max
  # are integers. (contained inside boundary)
  bd <- bd %>%
    dplyr::mutate(
      r_min = ceiling(.data$r_min),
      r_max = floor(.data$r_max),
      c_min = ceiling(.data$c_min),
      c_max = floor(.data$c_max)
    )

  # Top and bottom edges
  top_bottom <- bd %>%
    dplyr::mutate(
      row = purrr::map2(.data$r_min, .data$r_max, ~c(.x, .y)),
      col = purrr::map2(.data$c_min, .data$c_max, ~seq(.x, .y))
    ) %>%
    tidyr::unnest(row) %>%
    tidyr::unnest(col)

  # Left and right edges (excluding corners to avoid duplicates)
  left_right <- bd %>%
    dplyr::mutate(
      row = purrr::map2(.data$r_min, .data$r_max, ~seq(.x, .y)),
      col = purrr::map2(.data$c_min, .data$c_max, ~c(.x, .y))
    ) %>%
    tidyr::unnest(row) %>%
    tidyr::unnest(col)

  # Combine and remove duplicates (corners)
  dplyr::bind_rows(top_bottom, left_right) %>%
    dplyr::distinct(.data$row, .data$col)
}

infer_block_cell_representative <- function(
    df, group_var_name = "data_gid",
    type = c("both", "min", "max") ) {
  type <- match.arg(type)

  get_extreme <- function(df, group_var, fun) {
    dplyr::bind_rows(
      df %>%
        dplyr::group_by(.data$row, .data[[group_var]]) %>%
        dplyr::summarise(col = fun(.data$col), .groups = "drop"),
      df %>%
        dplyr::group_by(.data$col, .data[[group_var]]) %>%
        dplyr::summarise(row = fun(.data$row), .groups = "drop")
    )
  }

  tmin <- NULL
  tmax <- NULL
  if(type %in% c("min", "both")) {
    # West and North side are minimum
    tmin <- get_extreme(df, group_var_name, min)
  }
  if(type %in% c("max", "both")) {
    # East and South side are maximum
    tmax <- get_extreme(df, group_var_name, max)
  }

  if(type == "both") {
    # Return based on type
    # Both means boundaries
    dplyr::bind_rows(tmin, tmax) %>% dplyr::distinct()
  } else if(type == "min") {
    tmin
  } else {
    tmax
  }
}




# Section: Major Direction Stats Helpers ----

infer_calc_intermediate_attr_gids_dimwise <- function(
    d_ad, d_att, wise = c("col", "row")) {
  wise <- match.arg(wise)
  # Set axis and interval variables dynamically
  axis_var <- if (wise == "col") "col" else "row"
  interval_var <- if (wise == "col") "row" else "col"

  # Step 1: Pre-compute attr_gid ranges per axis
  attr_ranges <- d_att %>%
    dplyr::group_by(.data[[axis_var]], .data$attr_gid) %>%
    dplyr::summarise(
      attr_min = min(.data[[interval_var]]),
      attr_max = max(.data[[interval_var]]),
      .groups = "drop"
    )

  # Step 2: Add row identifiers and compute search bounds
  d_query <- d_ad %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      # Taking pairwise minimum as attr can be on either side of data (N or S, E or W)
      search_min = pmin(.data[[paste0(interval_var, "_dat")]], .data[[paste0(interval_var, "_att")]]),
      search_max = pmax(.data[[paste0(interval_var, "_dat")]], .data[[paste0(interval_var, "_att")]])
    )

  # Step 3: Cartesian join on axis, then filter for overlaps
  overlaps <- d_query %>%
    dplyr::inner_join(
      attr_ranges, by = axis_var,
      relationship = "many-to-many", suffix = c("", "_ar")
    ) %>%
    dplyr::filter(
      .data$attr_gid != .data$attr_gid_ar,  # Exclude same attr_gid
      # Below is equivalent of pmax(.data$search_min, .data$attr_min) < pmin(.data$search_max, .data$attr_max)
      .data$attr_max > .data$search_min,     # Interval overlap condition 1
      .data$attr_min < .data$search_max      # Interval overlap condition 2
    ) %>%
    dplyr::group_by(.data$row_id) %>%
    dplyr::summarise(
      # Count distinct attr_gids that has intersection with the search interval
      no_of_attr_gids_between = dplyr::n_distinct(.data$attr_gid_ar),
      .groups = "drop"
    )

  # Step 4: Join back to original
  result <- d_query %>%
    dplyr::left_join(overlaps, by = "row_id") %>%
    dplyr::mutate(
      no_of_attr_gids_between = dplyr::coalesce(.data$no_of_attr_gids_between, 0L)
    )

  # Only keep original columns + new result
  result <- result[c(colnames(d_ad), "no_of_attr_gids_between")]

  return(result)
}

infer_calc_stats_for_admap_dimwise <- function(
    d_dat, d_att,
    wise = c("col", "row"), penalty_coeff = 0.5,
    attribute_wise = FALSE) {


  # If no data_gid and attr_gid given, return empty tibble as further processing
  # is not possible.
  if(NROW(d_dat)==0 || NROW(d_att)==0) return(tibble::tibble())

  wise <- match.arg(wise)
  # Set grouping, direction, and axis variables
  axis_var <- if (wise == "col") "col" else "row"
  opp_var  <- if (wise == "col") "row" else "col"
  direction_group_tag <- if (wise == "col") "NS" else "WE"
  direction_tag <- if (wise == "col") list(low = "S", high = "N") else list(low = "E", high = "W")

  # 1. Join data and attributes on axis
  d_ad <- d_dat %>%
    dplyr::left_join(
      d_att, by = axis_var, suffix = c("_dat", "_att"),
      relationship = "many-to-many")

  # 2. Add direction, direction_group, and distance
  d_ad <- d_ad %>%
    dplyr::mutate(
      direction = ifelse(
        .data[[paste0(opp_var, "_dat")]] < .data[[paste0(opp_var, "_att")]],
        direction_tag$low, direction_tag$high),
      direction_group = direction_group_tag,
      dist = abs(.data[[paste0(opp_var, "_dat")]] - .data[[paste0(opp_var, "_att")]]),
      # Another measure calculated later num_attr_cells (for each data-attr group)
    )

  # If no data_gid and attr_gid pairs found, return empty tibble
  if(NROW(d_ad)==0) return(tibble::tibble())

  # Early return if attribute-wise stats are requested
  if(attribute_wise){
    d_a_wise <- d_ad[stats::complete.cases(d_ad),]

    # Here also another filter is happening, so we need to ensure that
    # `d_a_wise` has at least one row if not empty tibble is returned
    if(NROW(d_a_wise) == 0) return(tibble::tibble())

    colnames(d_a_wise)[colnames(d_a_wise) == paste0(opp_var, "_att")] <- opp_var

    return(
      d_a_wise[c("row", "col", "attr_gid", "data_gid", "direction",
                 "direction_group", "dist")]
    )
  }

  # 3. Find smallest distance within axis and retain only that touch point
  d_ad_smallest_dist <- d_ad %>%
    dplyr::group_by(
      .data$data_gid, .data$attr_gid, .data$direction, .data$direction_group,
      .data[[axis_var]]
    ) %>%
    # num_attr_cells actually never used.
    # dplyr::mutate(
    #   # Calculate number of attribute cells attached to data_gid
    #   num_attr_cells = dplyr::n_distinct(
    #     .data[[axis_var]], .data[[paste0(opp_var, "_att")]])
    # ) %>%
    dplyr::filter(.data$dist == min(.data$dist)) %>%
    dplyr::ungroup()

  # If no data_gid and attr_gid pairs found (check after each filter), return
  # empty tibble
  if(NROW(d_ad_smallest_dist)==0) return(tibble::tibble())

  # 4. Calculate intermediate gids between data and attribute gids
  d_ad_stats <- infer_calc_intermediate_attr_gids_dimwise(
    d_ad_smallest_dist, d_att, wise = wise
  )


  # 5. Aggregate for each data_gid, attr_gid, direction, and direction_group
  d_ad_agg <- d_ad_stats %>%
    dplyr::group_by(.data$data_gid, .data$attr_gid,
                    .data$direction, .data$direction_group) %>%
    # This is required to calculate a continuous version of dimension length
    # which is based on distance penalized sum of axis_var
    dplyr::mutate(inv_far = 1/(1+penalty_coeff*(.data$dist - min(.data$dist)))) %>%
    dplyr::summarise(
      # Overall distance between a data_gid and attr_gid is the smallest
      # distance among all nearest connected cells
      dist = min(.data$dist),
      # Dimension length is indication of full info in each side
      # dim_len = dplyr::n_distinct(.data[[axis_var]]), # dim_len is never used.
      # Somewhat continuous version of dimension length may be required in case
      # `dim_len` alone is inconclusive (this will be normalized by for better
      # comparison)
      dim_len_cont = sum(.data$inv_far),
      # Number of intermediate gids between data_gid and attr_gid
      attr_gids_between = max(.data$no_of_attr_gids_between),
      # This measure is to check how many attribute cells are connected to
      # data_gid and attr_gid
      # num_attr_cells = max(.data$num_attr_cells), # num_attr_cells is never used.
      .groups = "drop"
    )

  # Special normalization to ensure that `dim_len_cont` is comparable
  d_ad_agg <- d_ad_agg %>%
    dplyr::group_by(.data$data_gid, .data$direction_group) %>%
    dplyr::mutate(dim_len_cont = .data$dim_len_cont/max(.data$dim_len_cont)) %>%
    dplyr::ungroup()

  # 6. Remove incomplete cases (NA removal)
  d_ad_agg <- d_ad_agg[stats::complete.cases(d_ad_agg), ]

  return(d_ad_agg)
}




# corner cases

infer_calc_stats_for_admap_corner_unit <- function(
    d_dat, d_att, direction, attribute_wise = FALSE) {

  # Early return if no data or attributes are provided
  if (NROW(d_dat) == 0 || NROW(d_att) == 0) return(tibble::tibble())


  # 1. Join data and attributes on axis
  jb <- switch (
    direction,
    "SE" = dplyr::join_by("row" < "row", "col" < "col"),
    "SW" = dplyr::join_by("row" < "row", "col" > "col"),
    "NE" = dplyr::join_by("row" > "row", "col" < "col"),
    "NW" = dplyr::join_by("row" > "row", "col" > "col"))

  d_ad <- d_dat %>%
    dplyr::left_join(
      d_att,
      by = jb,
      suffix = c("_dat", "_att"))

  # 2. Add direction, direction_group, and distance
  d_ad <- d_ad %>%
    dplyr::mutate(
      direction = direction,
      direction_group = "corner",
      dist = sqrt((.data$row_dat - .data$row_att)^2 + (.data$col_dat - .data$col_att)^2)
      # Another measure calculated later num_attr_cells (for each data-attr group)
    )

  d_ad <- d_ad[stats::complete.cases(d_ad),]

  # If no data_gid and attr_gid pairs found, return empty tibble (required before min distance based filtering)
  if (NROW(d_ad) == 0) return(tibble::tibble())

  # Early return if attribute-wise stats are requested
  if(attribute_wise){
    d_a_wise <- d_ad %>%
      # rename row, col that of attr's
      dplyr::rename(row= "row_att", col = "col_att")

    return(
      d_a_wise[c("row", "col", "attr_gid", "data_gid", "direction",
                 "direction_group", "dist")]
    )
  }

  # 3. Find smallest distance within axis and retain only that touch point
  d_ad_stats <- d_ad %>%
    dplyr::group_by(.data$data_gid, .data$attr_gid, .data$direction, .data$direction_group) %>%
    # num_attr_cells actually never used.
    # dplyr::mutate(
    #   # Calculate number of attribute cells attached to data_gid
    #   num_attr_cells = dplyr::n_distinct(.data$row_att, .data$col_att)
    # ) %>%
    dplyr::filter(.data$dist == min(.data$dist)) %>%
    dplyr::ungroup()

  # Calculation of intermediate gids between data and attribute gids is omitted for corner cases
  # d_ad_stats[["no_of_attr_gids_between"]] <- 0 (it's also not required)

  # 4. Aggregate for each data_gid, attr_gid, direction, and direction_group
  d_ad_agg <- d_ad_stats %>%
    dplyr::group_by(.data$data_gid, .data$attr_gid,
                    .data$direction, .data$direction_group) %>%
    dplyr::summarise(
      # Overall distance between a data_gid and attr_gid is the smallest
      # distance among all nearest connected cells
      dist = min(.data$dist),
      # Below columns are not applicable for corner cases, but are included
      # for consistency with other cases.
      #
      # Dimension length is indication of full info in each side (0 for corner
      # cases)
      # dim_len = 0, # dim_len is never used.
      # Somewhat continuous version of dimension length may be required in case
      # `dim_len` alone is inconclusive (corner cases is zero)
      dim_len_cont = 0,
      # Number of intermediate gids between data_gid and attr_gid (0 for corner
      # cases)
      attr_gids_between = 0,
      # This measure is to check how many attribute cells are connected to
      # num_attr_cells = max(.data$num_attr_cells), # num_attr_cells is never used.
      .groups = "drop"
    )

  # 5. Remove incomplete cases (NA removal)
  d_ad_agg <- d_ad_agg[stats::complete.cases(d_ad_agg), ]

  return(d_ad_agg)
}

infer_calc_stats_for_admap_corners <- function(d_dat, d_att, attribute_wise = FALSE) {
  dplyr::bind_rows(
    infer_calc_stats_for_admap_corner_unit(d_dat, d_att, "SE", attribute_wise),
    infer_calc_stats_for_admap_corner_unit(d_dat, d_att, "NE", attribute_wise),
    infer_calc_stats_for_admap_corner_unit(d_dat, d_att, "NW", attribute_wise),
    infer_calc_stats_for_admap_corner_unit(d_dat, d_att, "SW", attribute_wise)
  ) %>% dplyr::distinct()
}


# Section: Data-Block Merging ----

infer_group_connected_blocks <- function(df) {
  # Extract columns as vectors
  col1_vals <- df[[1]]
  col2_vals <- df[[2]]

  # Get all unique nodes
  all_nodes <- unique(c(col1_vals, col2_vals))
  n_nodes <- length(all_nodes)

  # Create node to index mapping for fast lookup
  node_to_idx <- stats::setNames(seq_len(n_nodes), all_nodes)

  # Create adjacency matrix using vectorized operations
  idx1 <- node_to_idx[col1_vals]
  idx2 <- node_to_idx[col2_vals]

  # Initialize adjacency matrix
  adj_matrix <- matrix(FALSE, n_nodes, n_nodes)
  adj_matrix[cbind(idx1, idx2)] <- TRUE
  adj_matrix[cbind(idx2, idx1)] <- TRUE  # Make symmetric

  # Find connected components using matrix powers (transitive closure)
  # This is the key optimization - no explicit loops for traversal
  reach_matrix <- adj_matrix
  prev_reach <- matrix(FALSE, n_nodes, n_nodes)

  # Iterate until no new connections found (usually converges quickly)
  while (!identical(reach_matrix, prev_reach)) {
    prev_reach <- reach_matrix
    reach_matrix <- reach_matrix | (reach_matrix %*% adj_matrix > 0)
  }

  # Add self-connections
  diag(reach_matrix) <- TRUE

  # Extract unique connected components
  visited <- logical(n_nodes)
  groups <- list()

  # Single loop to extract groups
  for (i in seq_len(n_nodes)) {
    if (!visited[i]) {
      component_indices <- which(reach_matrix[i, ])
      groups <- append(groups, list(all_nodes[component_indices]))
      visited[component_indices] <- TRUE
    }
  }

  groups
}

infer_implied_surrounding_blocks <- function(ad_map, d_dat, d_att){
  # This function is used to generate the implied surrounding-block of each data
  # block. "implied surrounding block" = exact cells containing data-block cells
  # and connected attr-blocks cells. This (`implied_surrounding_blocks_single`)
  # is for single data-block.
  implied_surrounding_blocks_single <- function(data_block){
    # For each data block, find the corresponding attribute blocks through
    # mapping (ad_map)
    this <- ad_map[ad_map$data_gid == data_block,]

    # Combine data cells and attribute cells for this data block (and connect
    # attributes)
    this_cells <- d_dat[d_dat$data_gid == data_block,] %>%
      # Mark as data cells
      dplyr::mutate(ad_type= "data") %>%
      # Combine with attr cells
      dplyr::bind_rows(
        d_att[d_att$attr_gid %in% this$attr_gid,] %>%
          # Mark as attr cells
          dplyr::mutate(ad_type= "attr")
      ) %>%
      dplyr::distinct(.data$row, .data$col, .data$ad_type)

    # This is new gid for this data block + attribute block : (info_gid) Info
    # block
    this_cells$gid <- data_block

    this_cells
  }

  # Calculate implied surrounding-blocks for each data block
  implied_surrounding_blocks <- unique(ad_map$data_gid) %>%
    purrr::map_dfr(implied_surrounding_blocks_single)

  implied_surrounding_blocks
}

infer_data_block_merging <- function(
    ad_map, d_dat, d_att,
    # configuration parameters
    chk_in_for_overlapping_cases =
      # As variable name check_attr_entrapment_on_full_overlap_join is better
      # than check_for_attr_inside_while_joining_in_completely_overlapping_cases
      core_opt_get("check_attr_entrapment_on_full_overlap_join", TRUE),
    threshold_complete_connect_attempt_partial_gid_joins =
      core_opt_get("threshold_complete_connect_attempt_partial_gid_joins", 1000)){

  # Helper function (it is to avoid repeating codes .. not meat for internal
  # end-use package-wide)
  calc_measures <- function(
    df,
    need_area = FALSE, calc_block_intersection = TRUE) {

    if(calc_block_intersection){
      # Calculate the block intersection measures (common in need_area FALSE/TRUE
      # cases)
      df <- df %>%
        dplyr::mutate(
          # Calculate row overlap length (if any); zero if no overlap
          r_overlap = pmax(0, pmin(.data$r_max, .data$r_max_2) - pmax(.data$r_min, .data$r_min_2) + 1),
          # Calculate column overlap length (if any); zero if no overlap
          c_overlap = pmax(0, pmin(.data$c_max, .data$c_max_2) - pmax(.data$c_min, .data$c_min_2) + 1),
          # Compute area of intersection between the two rectangles (overlap area)
          intersection_area = .data$r_overlap * .data$c_overlap
        )
    }

    if(need_area) {

      df <- df %>%
        dplyr::mutate(
          # Area of second rectangle
          area_2 = (.data$r_max_2 - .data$r_min_2 + 1) * (.data$c_max_2 - .data$c_min_2 + 1),
          # Fraction of area of second rectangle covered by intersection
          fraction_of_area_covered = .data$intersection_area / .data$area_2
        )

    } else {
      # If need_area is FALSE, so we do not need area and we need is_exact
      df <- df %>%
        dplyr::mutate(
          # Check if the two rectangles (blocks) are exactly the same
          is_exact = (.data$r_min == .data$r_min_2) & (.data$r_max == .data$r_max_2) &
            (.data$c_min == .data$c_min_2) & (.data$c_max == .data$c_max_2)
        )
    }

    df
  }

  # Calculate implied surroundings for each data block

  # This function is used to calculate the implied surroundings of each data
  # block. "implied surroundings" = exact cells boundary (containing rectangle)
  # of data-block cells and connected attr-blocks cells.
  implied_surroundings <- infer_implied_surrounding_blocks(
    ad_map, d_dat, d_att) %>%
    dplyr::group_by(.data$gid) %>%
    # Calculate the block boundaries for each data_gid
    infer_block_boundary(exact = TRUE) %>%
    # Rename data_gid from gid
    dplyr::rename(data_gid = "gid")

  # Self-join "implied surroundings" to find overlaps among combinations of two
  # data blocks
  implied_surroundings_overlap <- implied_surroundings %>%
    dplyr::inner_join(
      implied_surroundings,
      # Only combination of two gid is required so discard others
      by = dplyr::join_by("data_gid">"data_gid"),
      suffix = c("", "_2")
    )

  # Compute overlap between two rectangles (blocks)
  implied_surroundings_overlap <- implied_surroundings_overlap %>%
    calc_measures(need_area = FALSE, calc_block_intersection = TRUE)

  # >> Situation 1: Completely overlapping blocks or identical blocks in terms
  # of implied_surroundings.
  #
  # These are the data gids that can be combined directly (It is a map between
  # two join-able data-gids as stacked data.frame)
  #
  # This `combine_data_blocks` is one of the part of main combine_data_blocks as
  # a whole.
  combine_data_blocks <- implied_surroundings_overlap %>%
    dplyr::filter(.data$is_exact) %>%
    dplyr::distinct(.data$data_gid, .data$data_gid_2)

  # These are the data gids that have block divergence (overlap
  # intersection_area > 0 and not exactly the same blocks - is_exact FALSE)
  join_possible_data_blocks <- implied_surroundings_overlap %>%
    dplyr::filter(!.data$is_exact & .data$intersection_area > 0)
  # Both data_gid and data_gid_2 are the same in this case (are possibly
  # join-able)
  join_possible_data_blocks <- unique(
    c(join_possible_data_blocks$data_gid,
      join_possible_data_blocks$data_gid_2))

  # Those which are filtered out are the ones that have no overlap and are
  # non-join-able.

  # Resolution of these join_possible_data_blocks data blocks (if any)
  if(length(join_possible_data_blocks)>0){

    # >> Situation 2: Data-blocks which are completely or partially inside
    # implied_surroundings

    # We take portion of d_dat that has partial data blocks
    d_dat_partial <- d_dat[d_dat$data_gid %in%
                             join_possible_data_blocks,]
    # We calculate the exact block boundaries for those partial data blocks
    d_dat_partial_bd <- d_dat_partial %>%
      # This group by data_gid is required for calculating block boundaries.
      # Otherwise it will give boundaries for all data.
      dplyr::group_by(.data$data_gid) %>%
      infer_block_boundary(exact = TRUE)


    # Now we have to derive implied_surroundings_overlap_with_data_boundary from
    # implied_surroundings_overlap. The problem here is that
    # implied_surroundings_overlap has data_gid >data_gid_2, but we need to
    # have data_gid_2 for all join_possible_data_blocks


    # Step by step creation of implied_surroundings_overlap_with_data_boundary

    # Step 1 : get only required rows from implied_surroundings_overlap
    implied_surroundings_overlap_with_data_boundary <-
      implied_surroundings_overlap %>%
      # From implied_surroundings_overlap take only those data gids that have
      # `partial` (possibility of joining) (i.e. data gids and data gid 2 that are
      # in join_possible_data_blocks)
      dplyr::filter((.data$data_gid %in% join_possible_data_blocks) &
                      (.data$data_gid_2 %in% join_possible_data_blocks)) %>%
      dplyr::filter(.data$intersection_area > 0) %>%
      # Take only required columns
      dplyr::select(
        c("data_gid", "r_min", "r_max", "c_min", "c_max",
          "data_gid_2", "r_min_2", "r_max_2", "c_min_2", "c_max_2"))

    # Change the side of data_gid_2 and data_gid so that all
    # join_possible_data_blocks are present in the data_gid_2 column.

    swap <- implied_surroundings_overlap_with_data_boundary
    # Reverse data_gid_2 and data_gid (Note heavily dependent on the col names
    # order)
    colnames(swap) <- c(
      "data_gid_2", "r_min_2", "r_max_2", "c_min_2", "c_max_2",
      "data_gid", "r_min", "r_max", "c_min", "c_max")

    # Now it can be checked that
    # implied_surroundings_overlap_with_data_boundary$data_gid_2 %>%
    # setdiff(join_possible_data_blocks) is empty. These are to avoid frequent
    # calculations and expand.grid joins etc.
    implied_surroundings_overlap_with_data_boundary <-
      implied_surroundings_overlap_with_data_boundary %>%
      dplyr::bind_rows(swap)


    # Step 2 : Join with d_dat_partial_bd to get the block boundaries for
    # comparison
    implied_surroundings_overlap_with_data_boundary <-
      implied_surroundings_overlap_with_data_boundary %>%
      # Since ealier calculated measures are based on implied_surroundings,
      # we need to calculate measures based on d_dat_partial_bd.
      dplyr::select(c("data_gid", "r_min", "r_max", "c_min", "c_max", "data_gid_2")) %>%
      # Join with d_dat_partial_bd to get the block boundaries for partial data
      dplyr::left_join(
        d_dat_partial_bd,
        by = dplyr::join_by("data_gid_2" == "data_gid"),
        suffix = c("", "_2")
      )

    # Step 3 : Calculate measures including area and fraction of area covered
    implied_surroundings_overlap_with_data_boundary <-
      implied_surroundings_overlap_with_data_boundary %>%
      calc_measures(need_area = TRUE, calc_block_intersection = TRUE)

    # For each data_gid_2, find the one with maximum fraction of area covered
    # which are non-zero (will be done subsequently)
    implied_surroundings_overlap_from_data_block <-
      implied_surroundings_overlap_with_data_boundary %>%
      dplyr::filter(.data$fraction_of_area_covered > 0)

    # Check how many filtered by above criteria (these are Situation 3 type of
    # blocks)
    remaining_type_3_data_blocks <- setdiff(
      join_possible_data_blocks,
      # Unlike combine_data_blocks, we need to take data_gid_2 here only as data
      # block boundaries are for data_gid_2 only.
      implied_surroundings_overlap_from_data_block$data_gid_2
    )


    # Here init implied_surroundings_overlap_from_type_3 as blank tibble
    implied_surroundings_overlap_from_type_3 <- tibble::tibble()

    # Output of this if block is implied_surroundings_overlap_from_type_3
    if(length(remaining_type_3_data_blocks) > 0) {

      # >> Situation 3: Data-blocks which are not completely inside
      # implied_surroundings but have intersection with another
      # implied_surroundings (Here only option is to take
      # implied_surroundings_overlap and fraction_of_area_covered based on
      # implied_surroundings_overlap)

      # If there are still some join_possible_data_blocks data blocks left, we
      # need to handle them separately. We will take those as is and combine
      # them with the best solution.
      implied_surroundings_overlap_type_3 <-
        implied_surroundings_overlap %>%
        # From implied_surroundings_overlap take only those data gids that have
        # possibility of joining (i.e. data gids or data gid 2 that are in
        # remaining_type_3_data_blocks) [Note here OR is used in place of AND]
        dplyr::filter((.data$data_gid %in% remaining_type_3_data_blocks) |
                        (.data$data_gid_2 %in% remaining_type_3_data_blocks)) %>%
        dplyr::filter(.data$intersection_area > 0) %>%
        # Take only required columns
        dplyr::select(
          c("data_gid", "r_min", "r_max", "c_min", "c_max",
            "data_gid_2", "r_min_2", "r_max_2", "c_min_2", "c_max_2",
            "intersection_area"))

      # Similar to earlier case: Change the side of data_gid_2 and data_gid so
      # that all remaining_type_3_data_blocks are present in the data_gid_2
      # column.

      swap <- implied_surroundings_overlap_type_3
      # Reverse data_gid_2 and data_gid (Note heavily dependent on the col names
      # order)
      colnames(swap) <- c(
        "data_gid_2", "r_min_2", "r_max_2", "c_min_2", "c_max_2",
        "data_gid", "r_min", "r_max", "c_min", "c_max",
        "intersection_area"
      )

      implied_surroundings_overlap_type_3 <-
        implied_surroundings_overlap_type_3 %>%
        dplyr::bind_rows(swap)

      # Now filter on data_gid_2 for remaining_type_3_data_blocks
      implied_surroundings_overlap_type_3 <-
        implied_surroundings_overlap_type_3 %>%
        dplyr::filter(.data$data_gid_2 %in% remaining_type_3_data_blocks)

      # Calculate measures including area and fraction of area covered
      implied_surroundings_overlap_type_3 <-
        implied_surroundings_overlap_type_3 %>%
        calc_measures(
          need_area = TRUE, calc_block_intersection = FALSE)


      # For each data_gid_2, find the one with maximum fraction of area covered
      # (to be done later)
      implied_surroundings_overlap_from_type_3 <-
        implied_surroundings_overlap_type_3 %>%
        dplyr::filter(.data$fraction_of_area_covered > 0)

    }

    # Combine the results from implied_surroundings_overlap_from_data_block and
    # implied_surroundings_overlap_from_type_3. Basically two different area
    # measure used. For type 2 directly block coverage and for type 3 fraction
    # of area among implied_surroundings. Data block coverage is more preferable
    # and implied_surroundings is least preferable. That is why we are
    # classifying type 2 and 3.
    implied_surroundings_overlap_from_type_2_type_3 <-
      implied_surroundings_overlap_from_data_block %>%
      dplyr::bind_rows(implied_surroundings_overlap_from_type_3)

    best_solution <- implied_surroundings_overlap_from_type_2_type_3 %>%
      dplyr::group_by(.data$data_gid_2) %>%
      # For each data_gid_2, find the one with maximum fraction of area covered
      dplyr::filter(.data$fraction_of_area_covered==max(.data$fraction_of_area_covered)) %>%
      dplyr::ungroup()

    best_solution_c2 <- best_solution |> dplyr::distinct(data_gid, data_gid_2)
    best_solution_c2_2 <- best_solution_c2
    colnames(best_solution_c2_2) <- rev(colnames(best_solution_c2_2))
    best_solution_c2 <- best_solution_c2 |>
      dplyr::bind_rows(best_solution_c2_2) |>
      dplyr::distinct(data_gid, data_gid_2)

    best_solution_c2 <- best_solution_c2 |> dplyr::filter(data_gid>data_gid_2)

    if(NROW(best_solution_c2)<threshold_complete_connect_attempt_partial_gid_joins){

      best_solution_c2 <-
        infer_data_block_merging_part_operation_attr_inside_filter(
          data_join_map = best_solution_c2, d_dat = d_dat, d_att = d_att,
          check_for_attr_inside = TRUE
        )

      combine_data_blocks_partial <- best_solution_c2

    } else {

      combine_data_blocks_partial <- best_solution_c2 %>%
        dplyr::group_by(.data$data_gid_2) %>%
        # In case of multiple solutions for a data_gid_2, take any one
        dplyr::summarise(data_gid = .data$data_gid[1], .groups = "drop")

    }



  } else {
    # If no partial data blocks, set empty tibble
    combine_data_blocks_partial <- tibble::tibble()
  }

  # We have to tackle the combine_data_blocks and combine_data_blocks_partial
  # separately: combine_data_blocks is for complete overlap and
  # combine_data_blocks_partial is for partial overlap. We can combine them into
  # a single data frame but their merging logic is different. So we will keep
  # them separate for now.
  #
  # combine_data_blocks_final <- dplyr::bind_rows( combine_data_blocks,
  # combine_data_blocks_partial )

  # If no joins are there exit early with unchanged d_dat and ad_map
  if(NROW(combine_data_blocks) == 0 &&
     NROW(combine_data_blocks_partial) == 0) {
    # No blocks is join-able
    return(
      list(
        d_dat = d_dat,
        ad_map = ad_map
      )
    )
  }

  # Otherwise we have some data gids that can be combined. We need to
  # combine those data gids into single data gids.

  # Use connected components to find groups of data gids that can be combined.
  # It is a list of vectors where each node of list is character vector
  # depicting data-gids which are to be merged.

  # Since two type (one with check_for_attr_inside as TRUE another with FALSE)
  # of joins are there, we need a helper function. (combine_data_blocks and
  # combine_data_blocks_partial are type of data_gid_join_map_raw [data_gid,
  # data_gid_2] which is to be converted to data_gid_join_map [data_gid,
  # data_gid_to]. data_gid_join_map_raw is pair wise connections and
  # data_gid_join_map is group wise mapping.)

  join_them <- function(data_gid_join_map, d_dat, d_att, ad_map,
                        check_for_attr_inside = TRUE) {

    if(NROW(data_gid_join_map)==0){
      # Early return without any change
      return(
        list(
          d_dat = d_dat,
          ad_map = ad_map
        )
      )
    }

    # Filter which are valid data_gid
    data_gid_join_map <- data_gid_join_map |>
      dplyr::filter(
        .data$data_gid %in% d_dat$data_gid,
        .data$data_gid_2 %in% d_dat$data_gid)

    if(check_for_attr_inside) {
      data_gid_join_map <-
        infer_data_block_merging_part_operation_attr_inside_filter(
          data_join_map = data_gid_join_map,
          d_dat = d_dat, d_att = d_att,
          check_for_attr_inside = TRUE)
    }

    # Second time as many filter happened already
    if(NROW(data_gid_join_map)==0){
      # Early return without any change
      return(
        list(
          d_dat = d_dat,
          ad_map = ad_map
        )
      )
    }

    data_gid_join_map <- infer_group_connected_blocks(data_gid_join_map)

    # Convert it to a mappable format data.frame
    data_gid_join_map <- data_gid_join_map %>%
      purrr::map_dfr(~ tibble::tibble(
        data_gid = .x,
        # New data_gid is the minimum of all data gids in the group
        data_gid_to = min(.x))
      )


    # Use unified data_gid logic
    possible_join_lst  <- infer_data_block_merging_part_operation(
      data_join_map = data_gid_join_map,
      d_dat = d_dat,
      d_att = d_att,
      d_dat_dep = ad_map,
      check_for_attr_inside = check_for_attr_inside
    )

    # Form new d_dat equivalent with data_gid replaced by data_gid_to (wherever
    # applicable)
    d_dat_joined <- possible_join_lst$d_dat

    # Since ad_map is a mapping between data_gid and attr_gid, we need to replace
    # data_gid in ad_map with data_gid_to from data_gid_join_map also so that new
    # d_dat_joined and d_att are in sync. We follow same pattern.
    ad_map_joined <- possible_join_lst$d_dat_dep

    # However, we still need to ensure that for each data_gid and direction_group,
    # we have single attr_gid. For which we use slightly different method as
    # compared to infer_major_direction_stats. Before employing that method, we
    # need to ensure that we have distinct data_gid, attr_gid, direction, and
    # direction_group combinations. This is because we have combined data gids and
    # thus we may have duplicate entries in ad_map for same data_gid, attr_gid,
    # direction, and direction_group combinations. We need to summarize those
    # combinations.
    ad_map_joined <- ad_map_joined %>%
      # Since we combined data gids, we should have duplicate entries in data_gid,
      # attr_gid, direction, direction_group. We first calculate the summaries on
      # each such distinct combinations.
      dplyr::group_by(
        .data$data_gid, .data$attr_gid,
        .data$direction, .data$direction_group) %>%
      # Calculate proper summaries for each data_gid and attr_gid pair (direction,
      # direction_group) is probably fixed (Due to combination criteria).
      dplyr::summarise(
        # These measures will be best represented by maximum
        # dim_len = max(.data$dim_len), # dim_len is never used.
        dim_len_cont = max(.data$dim_len_cont),
        attr_gids_between = max(.data$attr_gids_between),
        # Minimum distance for this data_gid and attr_gid pair
        dist = min(.data$dist),
        .groups = "drop"
      )

    # Filtering started for any potential duplicates.
    ad_map_joined <- ad_map_joined %>%
      # Regroup by data_gid and direction_group.
      dplyr::group_by(.data$data_gid, .data$direction_group) %>%
      # For each data_gid and direction_group, find the one with maximum dimension length
      dplyr::filter(.data$dim_len_cont==max(.data$dim_len_cont)) %>%
      # Further filter the one with minimum distance
      dplyr::filter(.data$dist==min(.data$dist)) %>%
      # Ungroup to remove grouping
      dplyr::ungroup()

    # Finally return the joined data and ad_map

    list(
      d_dat = d_dat_joined,
      ad_map = ad_map_joined
    )

  }

  # First, we will combine data blocks which are completely overlapping

  step1_join <- join_them(
    data_gid_join_map = combine_data_blocks,
    d_dat = d_dat,
    d_att = d_att,
    ad_map = ad_map,
    # Here we do not check for attributes inside the bounding box of combined
    # data gids, as these are completely overlapping blocks. So
    # check_for_attr_inside = FALSE in default case but can override based on
    # user settings.
    check_for_attr_inside = chk_in_for_overlapping_cases
  )

  # Now we have to combine data blocks which are partially overlapping or
  # completely inside the bounding box of other data blocks. These are
  # combine_data_blocks_partial.

  step2_join <- join_them(
    data_gid_join_map = combine_data_blocks_partial,
    d_dat = step1_join$d_dat,
    d_att = d_att,
    ad_map = step1_join$ad_map,
    # Here we check for attributes inside the bounding box of combined data gids
    check_for_attr_inside = TRUE
  )

  return(step2_join)

}


infer_util_gid_join <- function(gid_join_map, gid_linked_list_of_df, gid_tag = "data_gid"){
  # Use case is like:
  #
  # Form new d_dat equivalent with data_gid replaced by data_gid_to (wherever
  # applicable)
  #
  # Since ad_map is a mapping between data_gid and attr_gid, we need to replace
  # data_gid in ad_map with data_gid_to from data_gid_join_map also so that new
  # d_dat_joined and d_att are in sync. We follow same pattern.
  #
  # This function is generalized to work with any gid_tag (data_gid or attr_gid)
  # and on any list of data.frames thaat has either gids present (old to new
  # mapping).

  gid_linked_list_of_df %>%
    purrr::map(function(df){
      df_out <- df %>%
        dplyr::left_join(gid_join_map, by = gid_tag)

      # If gid_to is NA, then we keep the original gid, else we replace it with
      # gid_to.
      df_out[[gid_tag]] <- ifelse(
        is.na(df_out[[paste0(gid_tag, "_to")]]),
        df_out[[gid_tag]],
        df_out[[paste0(gid_tag, "_to")]])

      # Remove gid_to column
      df_out[[paste0(gid_tag, "_to")]] <- NULL

      dplyr::distinct(df_out)
    })
}


infer_data_block_merging_part_operation_attr_inside_filter <- function(
    data_join_map, d_dat, d_att,
    check_for_attr_inside = TRUE){

  if(NROW(data_join_map) > 0){
    if(check_for_attr_inside){
      # First, we need to find whether any rows of d_att is captured by combined
      # bounding box of these data_gids

      chk_data_join_with_no_entrapped_attr <- function(d1, d2){
        if(d1==d2) return(TRUE)
        NROW(
          util_rect_intersect_filter(
            d_att,
            d_dat %>% dplyr::filter(.data$data_gid %in% c(d1, d2))
          )
        ) == 0
      }

      ## Alternative approach: but this approach is not full proof. Hence using alternative:
      # data_join_map_chk <- data_join_map %>%
      #   split(data_join_map$data_gid_to) %>%
      #   purrr::map_dfr(function(dds){
      #     chk <- NROW(
      #       util_rect_intersect_filter(
      #         d_att,
      #         d_dat %>% dplyr::filter(.data$data_gid %in% dds$data_gid)
      #       )
      #     ) == 0
      #     # This is to match the length of dds and eventually that of data_join_map
      #     tibble::tibble(
      #       data_gid_to = dds$data_gid_to[1],
      #       is_valid = chk
      #     )
      #   })
      #
      # # Join the validity check with data_join_map
      # data_join_map <- data_join_map %>%
      #   dplyr::left_join(data_join_map_chk, by = "data_gid_to")


      data_join_map$is_valid <- purrr::map2_lgl(
        # Hardcoded colmn names: data_join_map$data_gid, data_join_map$data_gid_to,
        data_join_map[[1]], data_join_map[[2]],
        chk_data_join_with_no_entrapped_attr
      )

      # If any data_gid_to is not valid, then we can not merge these data_gids
      # (filter)
      data_join_map <- data_join_map %>%
        dplyr::filter(.data$is_valid) %>%
        dplyr::select(-"is_valid")
    }
  }

  data_join_map
}

infer_data_block_merging_part_operation <- function(
    data_join_map, d_dat, d_att, d_dat_dep,
    check_for_attr_inside = TRUE,
    # It should be resolve_fn_for_multi_data_gid to single data_gid
    resolve_fn = c("min","max","random")){

  resolve_fn <- match.arg(resolve_fn)
  if(resolve_fn == "min"){
    rfun <- min
  } else if(resolve_fn == "max"){
    rfun <- max
  } else {
    rfun <- function(x){
      sample(unique(x))[1]
    }
  }
  # data_join_map must have comined_id and data_gid
  if(NROW(data_join_map) > 0){
    # This function has two types of use. In one comined_id present in another
    # one directly data_join_map with data_gid and data_gid_to is given.
    if(utils::hasName(data_join_map,"comined_id")){
      data_join_map <- data_join_map %>%
        dplyr::group_by(.data$comined_id) %>%
        # Allot the minimum data_gid to data_gids in the group
        dplyr::mutate(data_gid_to = rfun(.data$data_gid)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$data_gid, .data$data_gid_to)
    }

    # Check if each of these groups are valid or not. i.e. whether these
    # data-gids can be merged together. If there is any attribute_gid in
    # combined bounding box of these data-gids, then we can not merge them.

    if(check_for_attr_inside){
      data_join_map <- infer_data_block_merging_part_operation_attr_inside_filter(
        data_join_map = data_join_map,
        d_dat = d_dat, d_att = d_att,
        check_for_attr_inside = TRUE
      )
    }

    # If now any data_gid_map is present, then we can proceed with the merging
    if(NROW(data_join_map) > 0){
      # Now we can merge data blocks (also need to be changed in combined
      # df/ADMap DF). So two dfs affected are: d_dat_dep (either combined or
      # ad_map) and d_dat
      jl <- infer_util_gid_join(
        data_join_map,
        list(d_dat = d_dat, d_dat_dep = d_dat_dep),
        gid_tag = "data_gid")

      # Update these two DFs
      d_dat <- jl$d_dat
      d_dat_dep <- jl$d_dat_dep
    }

  }

  list(
    d_dat = d_dat,
    d_dat_dep = d_dat_dep
  )
}


infer_preliminary_data_block_merging <- function(
    d_dat, d_att){
  # This function merges data blocks at preliminary stage (before AD-map
  # creation). This is based on connected attributes and data blocks. (Similar
  # to block identification)

  #### Type 1: Data blocks connected to same attributes

  # First type is those data_blocks which are connected to attributes and
  # attributes which are connected to data_blocks will be merged suitably

  combined <- d_dat %>%
    dplyr::bind_rows(d_att) %>%
    infer_table_blocks(group_id_tag = "c_") %>%
    dplyr::rename(comined_id = "gid")


  # Join with d_dat and d_att
  combined <- combined %>%
    dplyr::left_join(d_dat, by = c("row", "col")) %>%
    dplyr::left_join(d_att, by = c("row", "col"))

  # Find data_gid joins induced by combined connected cells
  data_join_map <- combined %>%
    # Take only those rows which have data_gid (same as d_dat)
    dplyr::filter(!is.na(.data$data_gid)) %>%
    # For each combined_id, find unique data_gids
    dplyr::group_by(.data$comined_id) %>%
    # Note the name should be `data_gids` and not `data_gid` (last `s`)
    dplyr::mutate(n_data_gid = dplyr::n_distinct(.data$data_gid)) %>%
    dplyr::filter(.data$n_data_gid > 1) %>%
    dplyr::ungroup() %>%
    # Only these columns are needed by
    # infer_data_block_merging_part_operation
    dplyr::distinct(.data$comined_id, .data$data_gid)

  # Then it proceeds through the procedure of merging data blocks
  join_l <- infer_data_block_merging_part_operation(
    data_join_map = data_join_map,
    d_dat = d_dat,
    d_att = d_att,
    d_dat_dep = combined,
    resolve_fn = "min"
  )

  d_dat <- join_l$d_dat
  combined <- join_l$d_dat_dep


  #### Type 2: Data blocks covered by same attributes in a connected block

  # Second type is those data blocks which are not connected to any attributes but
  # are covered by same attributes in a connected block.

  combined_induced_blocks <- combined %>%
    dplyr::group_by(.data$comined_id) %>%
    infer_block_boundary(exact = TRUE)

  # The data cells under induced combined boundary
  d_under_cbd <- combined_induced_blocks %>%
    split(combined_induced_blocks$comined_id) %>%
    purrr::map_dfr(function(cbd){
      util_rect_intersect_filter(
        d_dat, boundary = cbd
      ) %>%
        dplyr::mutate(comined_id = cbd$comined_id[1])
    })

  # Discard those combined_ids which do not have any duplicate data_gid (nothing
  # to merge if only one data-gid is present)
  d_under_cbd <- d_under_cbd %>%
    dplyr::group_by(.data$comined_id) %>%
    dplyr::mutate(n_data_gid = dplyr::n_distinct(.data$data_gid)) %>%
    dplyr::filter(.data$n_data_gid>1) %>%
    dplyr::ungroup()


  # Calculate number of rows captured by each combined_id for each data_gids
  d_under_cbd <- d_under_cbd %>%
    dplyr::group_by(.data$comined_id, .data$data_gid) %>%
    dplyr::summarise(n_rows = dplyr::n(), .groups = "drop")

  # Check rows for each data_gid in d_dat and compare portion of rows captured by
  # combined_id. If it is fully captured, then we can merge these data_gid.
  d_n_cells <- d_dat %>%
    dplyr::group_by(.data$data_gid) %>%
    dplyr::summarise(n_rows_orig = dplyr::n(), .groups = "drop")

  # Join the number of rows captured by combined_id with the original number of
  # rows in d_dat for each data_gid. This is to check if the combined_id
  # captures all the rows of the data_gid. If it does, then we can merge these
  # data_gid.
  d_under_cbd <- d_under_cbd %>%
    dplyr::inner_join(d_n_cells, by = "data_gid") %>%
    # The filter is to check if the combined_id captures all the rows of the
    # data_gid.
    dplyr::mutate(
      is_fully_contained = ifelse(.data$n_rows == .data$n_rows_orig, TRUE, FALSE)
    ) %>%
    dplyr::filter(.data$is_fully_contained) %>%
    dplyr::distinct(.data$comined_id, .data$data_gid)

  # Then it proceeds through the procedure of merging data blocks
  join_l <- infer_data_block_merging_part_operation(
    data_join_map = d_under_cbd,
    d_dat = d_dat,
    d_att = d_att,
    # Since combined is not required for this operation, we can use dummy
    d_dat_dep = combined %>% dplyr::filter(FALSE),
    resolve_fn = "max"
  )

  d_dat <- join_l$d_dat

  return(list(
    d_dat = d_dat
  ))

}


# Section: Direction Stats ----


infer_get_direction_stats_basic <- function(
    d_dat, d_att,
    include_corner = TRUE, attribute_wise = FALSE) {
  # This is a basic version of infer_major_direction_stats that does not do
  # block merging and other complex operations.

  admap1 <- dplyr::bind_rows(
    infer_calc_stats_for_admap_dimwise(d_dat, d_att, wise = "col",
                                       attribute_wise = attribute_wise),
    infer_calc_stats_for_admap_dimwise(d_dat, d_att, wise = "row",
                                       attribute_wise = attribute_wise)
  )

  if(NROW(admap1) == 0) {
    # Special case: If no data_gid and attr_gid pairs found, construct empty
    # tibble
    admap1 <- tibble::tibble(
      data_gid = character(),
      attr_gid = character(),
      row = integer(),
      col = integer()
    )
  }

  # Calculate corner statistics if required
  if(include_corner) {
    admap2 <- infer_calc_stats_for_admap_corners(
      d_dat, d_att,
      attribute_wise = attribute_wise)

    if(!attribute_wise) {
      # Exclude corner statistics that are not required, (attr, data) pair which
      # already are in admap1
      admap2 <- admap2 %>%
        dplyr::anti_join(
          admap1,
          by = c("data_gid", "attr_gid")
        )
    } else {
      # If attribute_wise is TRUE, we need to exclude corner statistics that
      # are not required, i.e., those which already have been calculated in
      # admap1 for the same row, col, and attr_gid.
      admap2 <- admap2 %>%
        dplyr::anti_join(
          admap1,
          by = c("row", "col", "attr_gid")
        )
    }

  } else {
    admap2 <- tibble::tibble()
  }

  # Combine both admap1 and admap2
  ad_comb <- dplyr::bind_rows(admap1, admap2) %>%
    dplyr::distinct()

  # do a minor processing for attribute_wise = TRUE case

  if(attribute_wise) {
    # ad_comb is having multiple entry for single data_gid (for each
    # attr_gid-cells) we have to get the best one for each data_gid and
    # attr_gid-cell pair (data_gid,row, col, attr_gid)
    ad_comb2 <- ad_comb %>%
      dplyr::group_by(.data$row, .data$col, .data$attr_gid, .data$data_gid) %>%
      dplyr::filter(.data$dist == min(.data$dist)) %>%
      dplyr::ungroup()


    # Now each cells of attribute_gid to choose near most data_gid cell wise
    ad_comb3 <- ad_comb2 %>%
      dplyr::group_by(.data$row, .data$col, .data$attr_gid) %>%
      dplyr::filter(.data$dist == min(.data$dist)) %>%
      # if still multiple data_gid, then choose first one
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Return the final ad_comb3
    return(ad_comb3)

  }

  return(ad_comb)

}

# Main function responsible for computing major direction statistics
infer_major_direction_stats <- function(
    d_dat, d_att) {

  # There might be a way to reduce computation burden but still achieve similar
  # results if we use infer_block_cell_representative on d_dat. But for normal
  # data set doing it on actual d_dat is better.
  #
  # It has been tested and seen in case of large data set also such as
  # AER>>CPS1988 data set, the infer_block_cell_representative is not
  # significantly faster. So it is not used here. But it is kept for reference
  # and future use if required. If saves mildly on memory. 114.9MB > 75.3MB >
  # 55.4MB (actual, infer_block_cell_representative-> both,
  # infer_block_cell_representative-> min)

  # Stage 1: Calculate Directional Statistics
  d_ad_stage1 <- infer_get_direction_stats_basic(
    d_dat = d_dat,
    d_att = d_att,
    include_corner = FALSE
  )


  # If stage 1 misses any data_block that means all attributes are in corner
  # directions of these data blocks. In such situations we may use corner mapping as
  # initiation for those remaining data_blocks. (This should be part of minor
  # direction stats but since missing a data-block is last option, it can be done
  # here.) (If any data-block is missed it will not take part in data-block
  # merging). This have to be carefully reviewed and tested.


  # Stage 1 is complete!

  # Early checks for d_ad_stage1
  if(NROW(d_ad_stage1) == 0) {
    # If no data_gid and attr_gid pairs found, return empty tibble (as further
    # processing is not possible).
    return(
      list(
        d_dat = d_dat,
        # Empty ad_map
        ad_map = tibble::tibble()
      )
    )
  }

  # Note: - Here the filter is done based on attr_gids_between == 0 and
  # subsequently min/max operations are done on dim_len_cont dist etc. This is
  # discrete way of selecting the best attribute for each data_gid and
  # direction_group pair. There may be a continuous way of doing it by defining
  # a helper function.
  #
  #
  # infer_calc_eval_measure_for_admap <- function(d_ad_stg_1, penalty_coeff = 0.5){
  #
  #   if(dplyr::is_grouped_df(d_ad_stg_1)) {
  #     # If already grouped use the same group
  #     d_ad_stg_1_g <- d_ad_stg_1
  #     retain_group <- TRUE
  #   } else {
  #     # Group by data_gid and direction_group if not already grouped
  #     d_ad_stg_1_g <- d_ad_stg_1 %>%
  #       dplyr::group_by(.data$data_gid, .data$direction_group)
  #     retain_group <- FALSE
  #   }
  #
  #   # Define main measures
  #   res <- d_ad_stg_1_g %>%
  #     dplyr::mutate(
  #       # Calculate normalized distance
  #       inv_far = 1/(1+penalty_coeff*(.data$dist - min(.data$dist))),
  #       # Normalize dimension length
  #       dim_len_norm = 1/(1 + penalty_coeff*
  #                           (max(.data$dim_len_cont)-.data$dim_len_cont)),
  #       # Calculate a measure inversely proportional to (number of intermediate
  #       # gids between data_gid and attr_gid)
  #       inv_attr_gids_between = 1/(1+penalty_coeff*
  #                                    (.data$attr_gids_between -
  #                                       min(.data$attr_gids_between))),
  #       # Evaluation measure is a combination of all three
  #       eval_measure = .data$inv_far + .data$dim_len_norm +
  #         .data$inv_attr_gids_between
  #     )
  #
  #   # Return
  #   if(retain_group) {
  #     # If already grouped, return the same grouped only calculated DF
  #     # That means retains group
  #     res
  #   } else {
  #     # Un-group if grouped by this function
  #     dplyr::ungroup(res)
  #   }
  # }

  # This should be part 1 of stage 2 (should be named as d_ad_stage2_1)
  #
  # But as explained below, this is stage 2 only (no split into part 1 and 2
  # required). Hence named as d_ad_stage2
  d_ad_stage2 <- d_ad_stage1 %>%
    # In stage 2 part 1, we select only those data_gid and direction_group pairs
    # that have no intermediate gids between data_gid and attr_gid
    dplyr::filter(.data$attr_gids_between == 0)

  # Note: if NROW(d_ad_stage1)>0 then always NROW(d_ad_stage2) >= 0 (by
  # definition)
  #
  # Hence check like this if(NROW(d_ad_stage2) > 0) (# Before min/max
  # operations, we need to check for NROW to avoid errors) is not required.
  #
  # Now dplyr::filter(.data$attr_gids_between == 0) may-not be always ideal as
  # it does not give importance to other criteria such as dimension length and
  # distance. Thus we need to adjust in very complex situations specifically
  # when a low dimension length attr is near most to connected data_gid while
  # there exists a full dim or larger dim attr far from it in same direction. in
  # such situations  following approach may be taken. However there is
  # alternative approches to solve it like L-shape rectification etc. Kept for
  # refernce only:
  #
  # d_ad_stage1 %>% dplyr::group_by(.data$data_gid, .data$direction_group,
  # .data$attr_gids_between) %>% dplyr::summarise(dim_len_cont =
  # max(.data$dim_len_cont), dist = min(.data$dist), .groups = "drop") %>%
  # dplyr::mutate(attr_gids_between_orig = .data$attr_gids_between) %>%
  # dplyr::mutate(attr_gids_between = 0) %>% dplyr::group_by(.data$data_gid,
  # .data$direction_group) %>% infer_calc_eval_measure_for_admap() %>%
  # dplyr::filter(.data$eval_measure == max(.data$eval_measure))

  if(NROW(d_ad_stage2) > 0) {
    # Before min/max operations, we need to check for NROW to avoid errors; Even
    # though it's always will be satisfied if NROW(d_ad_stage1)>0
    d_ad_stage2 <- d_ad_stage2 %>%
      dplyr::group_by(.data$data_gid, .data$direction_group) %>%
      # For each data_gid and direction_group, find the one with maximum dimension (cont) length
      dplyr::filter(.data$dim_len_cont==max(.data$dim_len_cont)) %>%
      # Further filter the one with minimum distance
      dplyr::filter(.data$dist==min(.data$dist)) %>%
      # Note at this stage also multiple rows can be present for same
      # data_gid and direction_group.
      dplyr::ungroup()
  }


  # Now check the result with ideal scenario, where each data_gid should have at
  # least one attr_gid for each direction_group (NS and WE).
  #
  # Ideally we should have one row per data_gid and direction_group. But
  # sometimes multiple rows can be present for same data_gid and
  # direction_group. This is because there can be multiple attr_gid for same
  # data_gid and direction_group
  #
  # Theoretically this situation will arise but is it happens then there will be
  # no attribute at all in d_ad_stage1 in missing direction. Hence analysis
  # further will not add any value additions. So we can proceed with the next
  # stage of analysis. Following is kept for reference only.
  #
  # all_expected <- expand.grid(
  #   data_gid = unique(d_ad_stage2$data_gid),
  #   direction_group = c("NS", "WE"),
  #   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  # )
  #
  # # Those data_gid and direction_group pairs that are not in d_ad_stage2
  # remaining <- dplyr::anti_join(
  #   all_expected, d_ad_stage2,
  #   by = c("data_gid", "direction_group")
  # )



  # Stage 3: Joining Data Blocks Based on Implied Surroundings
  #
  # For each data block, we take both NS (North-South) and WE (West-East) side
  # attribute blocks and merge them to form a single block containing data and
  # attributes on both sides. We then identify enclosing boundaries for each
  # data_gid-induced block.
  #
  # Next, we calculate a metric known as "block divergence." If a block is fully
  # contained within another, its block divergence is zero. Otherwise, we
  # compute the overlap cells (even if empty) as block divergence. This measures
  # how well a data block is surrounded.
  #
  # Data blocks with no block divergence can be grouped with other blocks having
  # the same implied surroundings.
  #
  # For blocks with block divergence, we use `d_ad_stage1` to find suitable NS
  # and WE mappings to eliminate block divergence.
  #
  # There are two possibilities:
  #
  # 1. If the data block is contained within the implied surroundings of another
  # block, we simply use the NS and WE mapping of that block.
  #
  # 2. Otherwise, we use `d_ad_stage1` to find a match that minimizes overall
  # block divergence as an optimization measure.
  #
  # Note: This step is crucial as it enables the merging of data blocks.

  # Data Block Merges (if any)
  if(dplyr::n_distinct(d_dat$data_gid)>1){

    # If there are multiple data blocks, we can proceed with merging
    data_block_merging <- infer_data_block_merging(
      ad_map = d_ad_stage2, d_dat = d_dat, d_att = d_att
    )

    lo <- infer_relabel_data_group_id(
      list(
        d_dat = data_block_merging$d_dat,
        ad_map = data_block_merging$ad_map
      )
    )
  } else {
    # If only one data block is present, we can return the ad_map as it is
    lo <- list(
      d_dat = d_dat,
      ad_map = d_ad_stage2
    )
  }

  return(lo)

}

infer_relabel_data_group_id <- function(l_in){

  d_gids <- l_in$d_dat %>% dplyr::distinct(.data$data_gid)
  d_gids$new_data_gid <- paste0("d",util_natural_segment_rank(d_gids$data_gid))

  l_out <- l_in %>%
    purrr::map(~{
      .x %>% dplyr::left_join(d_gids, by = "data_gid") %>%
        dplyr::rename(data_gid_old = "data_gid") %>%
        dplyr::rename(data_gid = "new_data_gid") %>%
        dplyr::select(-"data_gid_old")
    })

  return(l_out)

}


infer_minor_direction_stats <- function(d_dat, d_att, admap){

  # Check Remaining attributes that are not mapped in major direction

  if(NROW(admap)==0) {
    # Special case handling so that dplyr::anti_join etc does not fail
    admap <- tibble::tibble(
      attr_gid = character(),
      data_gid = character()
    )
  }
  remaining_attrs <- d_att %>%
    dplyr::anti_join(
      admap, by = "attr_gid"
    )

  if(NROW(remaining_attrs)==0){
    # If no remaining attributes, we can return empty tibble as nothing to map
    return(tibble::tibble())
  }

  # Since major attributes are already mapped, we can use the implied
  # surrounding blocks of data gids to find the minor direction stats. This
  # recreates the "implied surrounding blocks" of data gids considering mapped
  # major attributes also part of data blocks.

  # Usual scenario where admap is present
  if(NROW(admap)>0) {
    # Infer implied surrounding blocks based on admap and attributes
    d_implied_surrounding_blocks <- infer_implied_surrounding_blocks(
      admap, d_dat, d_att) %>%
      dplyr::distinct(.data$row, .data$col, .data$gid) %>%
      # Rename gid to data_gid for consistency
      dplyr::rename(data_gid = "gid")
  }

  # Special case handling where admap is empty
  if(NROW(admap)==0) {
    # If no admap is present, we can not infer implied surrounding blocks
    # the take data_blocks only
    d_implied_surrounding_blocks <- d_dat
  }


  # create base map now with include_corner = TRUE
  ad_map_minor_1 <- infer_get_direction_stats_basic(
    d_dat = d_implied_surrounding_blocks,
    d_att = remaining_attrs,
    include_corner = TRUE
  )

  # By design if NROW(remaining_attrs) > 0 then NROW(ad_map_minor_1) >= 0 (as
  # corner is included)

  # here each attr_gid to choose near most data_gid

  ad_map_minor <- ad_map_minor_1 %>%
    dplyr::group_by(.data$attr_gid) %>%
    # For each attr_gid, find the one with minimum distance
    dplyr::filter(.data$dist == min(.data$dist)) %>%
    # If still multiple rows, take the first one: (As this is just to define
    # implied surrounding blocks, later subsequent spiting and grouping is done
    # on d_att.)
    dplyr::slice(1) %>%
    dplyr::ungroup()

  ad_map_minor

}

infer_extend_data_block_with_boundary_cells_excluding_attrs <- function(
    d_dat_sub, d_att_sub) {
  # This function is used to extend the data block with boundary cells excluding
  # attributes cells.

  # d_dat should have only one data_gid. We may aoid this check as it will be
  # called from internal functions only:
  #
  # if(dplyr::n_distinct(d_dat$data_gid) != 1) {
  #   rlang::abort("d_dat should have only one data_gid", call = NULL)
  # }

  # Get boundary of the singel data block
  bd <- infer_block_boundary(d_dat_sub, exact = TRUE)

  # Get the boundary cells of the data block
  bd_cells <- infer_block_boundary_cells(bd)

  # bd_cells should have only one data_gid.
  bd_cells$data_gid <- d_dat_sub$data_gid[1]

  # Combined with the data block cells and remove attributes-cells from it.
  d_dat_ext  <- d_dat_sub %>%
    dplyr::bind_rows(bd_cells) %>%
    # Remove attributes from the data block
    dplyr::anti_join(d_att_sub, by = c("row", "col")) %>%
    dplyr::distinct()

  d_dat_ext

}

# Section: Split Attribute Blocks ----

infer_engulf_attrs_in_implied_surrounding_blocks <- function(
    implied_surrounding_blocks, d_att) {
  # This function is used to engulf attributes in implied surrounding blocks
  # which falls under the boundaries of implied surrounding blocks.

  info_bd <- implied_surrounding_blocks %>%
    dplyr::group_by(.data$gid) %>%
    infer_block_boundary(exact = TRUE)

  # Attr cells under implied surrounding blocks
  a_under_ibd <- info_bd %>%
    split(info_bd$gid) %>%
    purrr::map_dfr(function(ibd){
      util_rect_intersect_filter(
        d_att, boundary = ibd
      ) %>%
        dplyr::mutate(gid = ibd$gid[1], ad_type  = "attr")
    })

  # Make sure that the columns of a_under_ibd match with
  # implied_surrounding_blocks
  a_under_ibd <- a_under_ibd[colnames(implied_surrounding_blocks)]

  implied_surrounding_blocks %>%
    dplyr::bind_rows(a_under_ibd) %>%
    dplyr::distinct()

}

infer_attr_split <- function(info_blocks) {
  # This function is used to infer the attribute split based on the provided
  # information blocks.

  d_dat <- info_blocks[info_blocks$ad_type == "data", ]
  d_att <- info_blocks[info_blocks$ad_type == "attr", ]
  d_att$ad_type <- NULL

  d_dat <- d_dat %>%
    # Here retain the gid column as data_gid
    dplyr::rename(data_gid = "gid") %>%
    # Remove the gid column (and others) as it is not needed
    dplyr::distinct(.data$data_gid, .data$row, .data$col)

  d_att <- d_att %>%
    dplyr::mutate(
      # Create a dummy attr_gid "a"+dblk for attribute. It essentially means that
      # all attributes are grouped together.
      attr_gid = paste0("a", .data$gid)
    )


  for_each_info_block <- function(d_blk) {
    # This function is used to process each block of information.

    d_dat_sub <- d_dat %>%
      dplyr::filter(.data$data_gid == d_blk)

    # Attr subsection
    # This is a trick to use attr focused - get_direction_df to re-classify each
    # attributes into direction groups
    d_att_sub <- d_att %>%
      dplyr::filter(.data$gid == d_blk) %>%
      # Remove the gid column (and others) as it is not needed
      dplyr::select("attr_gid", "row", "col")

    d_dat_sub_ext <-
      infer_extend_data_block_with_boundary_cells_excluding_attrs(
        d_dat_sub, d_att_sub
      )

    # Get the direction for this block and sub-attributes: It has to be
    # attr-wise i.e. for each attr - (row, col) there should be an entry.
    infer_get_direction_stats_basic(
      d_dat = d_dat_sub_ext,
      d_att = d_att_sub,
      # Include corner attributes as well
      include_corner = TRUE,
      # This is important as we are interested in attribute-wise direction
      attribute_wise = TRUE
    )
  }

  admap_for_attr_split <- unique(d_dat$data_gid) %>%
    purrr::map_dfr(for_each_info_block)


  admap_for_attr_split <- admap_for_attr_split %>%
    dplyr::group_by(.data$data_gid, .data$direction) %>%
    dplyr::mutate(
      # Assign an order to each unique distance within each group
      dist_order = util_hierarchical_rank(.data$dist),

      # Create dir_rc_mark (split_tag'part) based on direction and position
      dir_rc_mark = dplyr::case_when(
        .data$direction == "N" ~ paste0(max(.data$row) - .data$row + 1, ":0"),
        .data$direction == "S" ~ paste0(.data$row, ":0"),
        .data$direction == "W" ~ paste0("0:", max(.data$col) - .data$col + 1),
        .data$direction == "E" ~ paste0("0:", .data$col),
        .data$direction_group == "corner" ~ paste0(.data$row, ":", .data$col),
        TRUE ~ "0:0"
      ),

      # Create a unique split_tag for each group/order/position
      split_tag = util_hierarchical_rank(
        .data$dist_order, .data$dir_rc_mark
      ),

      # Construct a micro attribute gid for each split
      micro_attr_gids = paste0(
        "a", .data$data_gid, "_", .data$direction, "_", .data$split_tag
      )
    )

  # Final Touch for Creation of attr_split
  attr_split <- admap_for_attr_split %>%
    dplyr::ungroup() %>%
    dplyr::select(
      attr_gid = "micro_attr_gids", "data_gid",
      "direction", "direction_group",
      "row", "col",
      # These Two are still required for HOT and nice name assignment
      "dist", "dir_rc_mark"
    ) %>%
    dplyr::distinct()

  return(attr_split)
}

# Section: Infer L-Shaped Block-Boundaries ----


infer_preliminary_L_shape_imposition <- function(
    d_dat, d_att,
    # Any fraction of data cells in openings of L above this threshold will be
    # considered for further action.
    thresold_for_L_data_area_fraction_hi = 0.8,
    # If fraction of data cells in openings of L is above this threshold but
    # below threshold_for_L_data_area_fraction_hi, then
    # (threshold_for_L_relative_area_fraction will be activated.)
    thresold_for_L_data_area_fraction_lo = 0.6,
    # In above mentioned case, if area fraction of L-shape covered area is below this
    # threshold, then it is considered for further action.
    thresold_for_L_relative_area_fraction = 0.5,
    # What will happen to the data cells captured by L-shape attributes (either
    # empty or attr)
    action_on_L_captured_cells = "attr") {

  action_on_L_captured_cells <- match.arg(
    action_on_L_captured_cells,
    c("attr", "empty")
  )

  # This function is used to impose L-shaped boundaries on the data and attr
  # combined blocks. Return of this function is d_dat and d_att potentially
  # modified.

  # Even though this section is repeated in infer_preliminary_data_block_merging
  # The input d_dat and d_att are different. So it is kept separate.
  combined <- d_dat %>%
    dplyr::bind_rows(d_att) %>%
    infer_table_blocks(group_id_tag = "c_") %>%
    dplyr::rename(comined_id = "gid")

  # Join with d_dat and d_att
  combined <- combined %>%
    dplyr::left_join(d_dat, by = c("row", "col")) %>%
    dplyr::left_join(d_att, by = c("row", "col"))

  attr_L_shape <- combined %>%
    # Take combined_id where at least one data_gid is not NA
    dplyr::group_by(.data$comined_id) %>% dplyr::filter(any(!is.na(.data$data_gid))) %>%
    dplyr::ungroup() %>%
    # Then Take attributes only
    dplyr::filter(!is.na(.data$attr_gid)) %>%
    dplyr::group_by(.data$comined_id) %>%
    infer_bounding_L_shape()

  # Early return for attr_L_shape blank case
  if(NROW(attr_L_shape) == 0) {
    # If no L-shape is found return unchanged d_dat and d_att
    return(list(
      d_dat = d_dat[c("data_gid", "row", "col")],
      d_att = d_att[c("attr_gid", "row", "col")]
    ))
  }

  # If NROW(attr_L_shape)>0 proceed for further analysis

  # Extract d_dat from combined (to get combined id)
  d_dat <- combined %>%
    # Then Take data_cells only
    dplyr::filter(!is.na(.data$data_gid))

  # Here openings means open area apart from L shape.
  openings <- attr_L_shape %>% dplyr::filter(.data$is_outside)

  # Find the data cells that are not part of L-shape attributes
  d_dat_outside_L <- openings %>%
    split(openings$comined_id) %>%
    purrr::map_dfr(function(dp){
      # For each opening, we need to find the data cells that are not part of L-shape
      d_dat %>%
        dplyr::filter(.data$comined_id == dp$comined_id[1]) %>%
        # Remove the data cells that are part of L-shape
        util_rect_intersect_filter(
          boundary = dp
        )
    })

  # Check summary of d_dat_outside_L to decide whether to take action or not. It
  # is based on fraction of data cells captured by L-shape. If that is large
  # then no action on those combined ids.

  # Calculate the fraction of data cells outside L-shape for each combined_id
  # This is done by summarizing the data cells and then joining with the
  # summary of data cells outside L-shape.
  d_dat_summary <- d_dat %>%
    dplyr::group_by(.data$comined_id) %>%
    dplyr::summarise(
      n_data_cells = dplyr::n(), .groups = "drop")

  # Summarize the d_dat_outside_L
  d_dat_action_summary <- d_dat_outside_L %>%
    dplyr::group_by(.data$comined_id) %>%
    dplyr::summarise(
      n_data_cells_outside_L = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(d_dat_summary, by = "comined_id") %>%
    dplyr::mutate(
      # Calculate fraction of data cells outside L-shape
      fraction_outside_L = .data$n_data_cells_outside_L / .data$n_data_cells
    )

  # Take area covered under L with respect to whole enclosure of L. This measure
  # may be required to make sure actionable or not
  attr_L_shape_area_stats <- attr_L_shape %>%
    dplyr::distinct(.data$comined_id, .data$area_fraction)

  # Append area fraction to the d_dat_action_summary
  d_dat_action_summary <- d_dat_action_summary %>%
    dplyr::left_join(attr_L_shape_area_stats, by = "comined_id")

  d_dat_actionable <- d_dat_action_summary %>%
    dplyr::mutate(
      is_actionable = dplyr::case_when(
        # If fraction of data cells outside L-shape is above the threshold, then
        # it is actionable
        .data$fraction_outside_L > thresold_for_L_data_area_fraction_hi ~ TRUE,
        # If fraction of data cells outside L-shape is above the lower threshold
        # but below the higher threshold, then check area fraction of L-shape
        .data$fraction_outside_L > thresold_for_L_data_area_fraction_lo &
          .data$area_fraction < thresold_for_L_relative_area_fraction ~ TRUE,
        # Otherwise, it is not actionable
        TRUE ~ FALSE
      )
    )

  d_dat_actionable <- d_dat_actionable %>%
    dplyr::filter(.data$is_actionable)

  # Early return for d_dat_actionable blank case
  if(NROW(d_dat_actionable) == 0) {
    # If no L-shape is actionable >> return unchanged d_dat and d_att
    return(list(
      d_dat = d_dat[c("data_gid", "row", "col")],
      d_att = d_att[c("attr_gid", "row", "col")]
    ))
  }

  # If NROW(d_dat_actionable)>0 proceed for further actions

  # Fine tune the d_dat based on actionable L-shapes
  d_dat_fine_tune <- d_dat_outside_L %>%
    # Take outside L shape data cells only for actionable combined_ids
    dplyr::semi_join(
      d_dat_actionable, by = "comined_id"
    ) %>%
    # Part 2 is to take data cells as it is from non-actionable combined_ids
    dplyr::bind_rows(
      d_dat %>% dplyr::anti_join(
        d_dat_actionable, by = "comined_id"
      )
    )

  # Rest two steps are to be done later depdending on action_on_L_captured_cells
  #
  # # Remove duplicates if any and take only required columns
  # dplyr::distinct(.data$row, .data$col) %>%
  # # Recompute data_gid based on row and col as cells are removed
  # infer_table_blocks(group_id_tag = "d") %>%
  # dplyr::rename(data_gid = "gid")

  # Now fine Tune d_att based on actionable L-shapes

  # Two possible actions for cells captured by L-shape: 1) it can be discarded 2)
  # it can be considered as attribute
  #
  # action_on_L_captured_cells can be "empty" or "attr"

  if(action_on_L_captured_cells == "empty"){
    # In this case no change in
    d_att_fine_tune <- d_att

    # Complete rest of d_dat_fine_tune operations
    d_dat_fine_tune <- d_dat_fine_tune %>%
      # Remove duplicates if any and take only required columns
      dplyr::distinct(.data$row, .data$col) %>%
      # Recompute data_gid based on row and col as cells are removed
      infer_table_blocks(group_id_tag = "d") %>%
      dplyr::rename(data_gid = "gid")


  } else {
    # It means action_on_L_captured_cells == "attr"

    # In this case, we need to find d_dat cells removed by L-shape and add them
    # to attr. d_dat - d_dat_fine_tune is what we need.

    # This is not yet data cells that are part of L-shape attributes, but rather
    # it is (data cells that are part of L shape) + (data cells that are not
    # part of L shape but are outside enclosure of L-shape)
    d_dat_inside_L_with_extra <- d_dat %>%
      dplyr::anti_join(
        d_dat_fine_tune, by = c("row", "col")
      ) %>%
      # Take only row, col for further processing
      dplyr::select("row", "col")

    # Now we need to find whole enclosed area of L-shape attributes and
    # intersect with d_dat_inside_L to get the data cells that are part of
    # L-shape attributes
    attr_whole_L <- attr_L_shape %>%
      dplyr::group_by(.data$comined_id) %>%
      # Derive whole area implied by L-shape
      dplyr::summarise(
        r_min = min(.data$r_min),
        c_min = min(.data$c_min),
        r_max = max(.data$r_max),
        c_max = max(.data$c_max),
        .groups = "drop"
      )

    # Find the data cells that are not part of L-shape attributes
    d_dat_inside_L <- attr_whole_L %>%
      split(openings$comined_id) %>%
      purrr::map_dfr(function(dp){
        # For each enclosed L-shape, we need to find the data cells that are
        # part of L-shape
        d_dat_inside_L_with_extra %>%
          # Keep the data cells that are part of L-shape
          util_rect_intersect_filter(
            boundary = dp
          )
      })


    # Create new attributes for the L-shape captured cells
    d_att_fine_tune <- d_att %>%
      # Take only row, col for further processing
      dplyr::select("row", "col") %>%
      dplyr::bind_rows(d_dat_inside_L) %>%
      dplyr::distinct(.data$row, .data$col) %>%
      # Recompute attr_gid based on row and col as cells are removed
      infer_table_blocks(group_id_tag = "a") %>%
      dplyr::rename(attr_gid = "gid")

    # Now add those extra back to data gid
    d_dat_outside_L_extra <- d_dat_inside_L_with_extra %>%
      dplyr::anti_join(d_dat_inside_L, by = c("row", "col"))

    # Add those extra data cells to d_dat_fine_tune

    # Complete rest of d_dat_fine_tune operations
    d_dat_fine_tune <- d_dat_fine_tune %>%
      # Remove duplicates if any and take only required columns
      dplyr::distinct(.data$row, .data$col) %>%
      # Add those extra data cells (d_dat_outside_L_extra) to d_dat_fine_tune
      dplyr::bind_rows(d_dat_outside_L_extra) %>%
      # Recompute data_gid based on row and col as cells are removed
      infer_table_blocks(group_id_tag = "d") %>%
      dplyr::rename(data_gid = "gid")

  }

  # Finally return the fine-tuned d_dat and d_att

  list(
    d_dat = d_dat_fine_tune,
    d_att = d_att_fine_tune
  )
}


infer_bounding_L_shape <- function(df) {

  # If the input data frame is empty, return an empty tibble
  if (NROW(df) == 0) {
    return(tibble::tibble())
  }

  s_help_f <- function(r, c){
    infer_bounding_L_shape_unit(tibble::tibble(row = r, col = c))
  }

  df_1 <- df %>%
    dplyr::summarise(
      boundary_L = list(s_help_f(.data$row, .data$col)),
      .groups = "drop")

  df_2 <- df_1 %>%
    tidyr::unnest_wider(
      "boundary_L", names_repair = "minimal",
      simplify = TRUE)

  # If no bounding L-shape is found, return an empty tibble
  if(!utils::hasName(df_2, "rect")) return(tibble::tibble())

  df_2 %>%
    tidyr::unnest_longer(
      c("rect",
        "r_min", "c_min", "r_max", "c_max",
        "is_corner", "is_outside", "type_of_L",
        "area_fraction"),
      names_repair = "minimal",
      simplify = TRUE)

}

infer_bounding_L_shape_unit <- function(df) {
  # Call the parameterized function for all four directions
  d_all_dirs <- dplyr::bind_rows(
    infer_bounding_L_shape_unit_for_a_dir(df, "SW"),
    infer_bounding_L_shape_unit_for_a_dir(df, "SE"),
    infer_bounding_L_shape_unit_for_a_dir(df, "NW"),
    infer_bounding_L_shape_unit_for_a_dir(df, "NE")
  )

  # If no bounding L-shape is found, return an empty tibble
  if (NROW(d_all_dirs) == 0) {
    return(tibble::tibble())
  }

  # From all valid L-shapes, filter the one(s) with the minimum area
  d_all_dirs <- d_all_dirs %>%
    dplyr::filter(.data$area == min(.data$area))

  # If there's a tie in area, pick the first one based on direction order
  this_dir <- d_all_dirs$type_of_L[1]

  # The L-shape data for the chosen direction
  d_all_dirs <- d_all_dirs %>%
    dplyr::filter(.data$type_of_L == this_dir)

  whole_area <- util_area_of_rect(
    min(d_all_dirs$r_min), min(d_all_dirs$c_min),
    max(d_all_dirs$r_max), max(d_all_dirs$c_max)
  )

  # Return the final L-shape data for the chosen direction
  d_all_dirs %>%
    dplyr::mutate(
      # Fraction of area covered by L
      area_fraction = .data$area/whole_area
    ) %>%
    dplyr::select(-"area")
}

infer_bounding_L_shape_unit_for_a_dir <- function(df, direction) {
  # Set up parameters based on the direction (e.g., "SW", "NE")
  params <- list(
    col_fun = if (stringr::str_detect(direction, "W")) max else min,
    row_fun = if (stringr::str_detect(direction, "S")) min else max,
    r_adj   = if (stringr::str_detect(direction, "S")) -1 else 1,
    c_adj   = if (stringr::str_detect(direction, "W")) 1 else -1
  )

  # Find extreme points to identify potential L-shape corners
  dr <- df %>%
    dplyr::group_by(.data$row) %>%
    dplyr::summarise(mc = params$col_fun(.data$col), .groups = "drop")
  dc <- df %>%
    dplyr::group_by(.data$col) %>%
    dplyr::summarise(mr = params$row_fun(.data$row), .groups = "drop")

  # Generate a grid of all possible corner pin locations
  possible_L_corner_pins <- expand.grid(
    row = unique(dc$mr), col = unique(dr$mc),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )

  # Get the exact bounding box of the entire data frame
  df_d <- df %>% dplyr::ungroup() %>% infer_block_boundary(exact = TRUE)

  # Helper functions
  in_between <- function(x, a, b) x >= pmin(a, b) & x <= pmax(a, b)

  # Define the three rectangles that form the L-shape
  get_rects <- function(r, c) {
    switch(direction,
           "SW" = list(
             list(r = c(df_d$r_min, r - 1), c = c(df_d$c_min, c)),
             list(r = c(df_d$r_max, r),     c = c(df_d$c_min, c)),
             list(r = c(df_d$r_max, r),     c = c(df_d$c_max, c + 1)),
             # The remaining rectangle which is not part of L-shape
             list(r = c(df_d$r_min, r - 1), c = c(df_d$c_max, c + 1))
           ),
           "SE" = list(
             list(r = c(df_d$r_max, r),     c = c(df_d$c_min, c - 1)),
             list(r = c(df_d$r_max, r),     c = c(df_d$c_max, c)),
             list(r = c(df_d$r_min, r - 1), c = c(df_d$c_max, c)),
             # The remaining rectangle which is not part of L-shape
             list(r = c(df_d$r_min, r - 1), c = c(df_d$c_min, c - 1))
           ),
           "NW" = list(
             list(r = c(df_d$r_max, r + 1), c = c(df_d$c_min, c)),
             list(r = c(df_d$r_min, r),     c = c(df_d$c_min, c)),
             list(r = c(df_d$r_min, r),     c = c(df_d$c_max, c + 1)),
             # The remaining rectangle which is not part of L-shape
             list(r = c(df_d$r_max, r + 1), c = c(df_d$c_max, c + 1))
           ),
           "NE" = list(
             list(r = c(df_d$r_min, r),     c = c(df_d$c_min, c - 1)),
             list(r = c(df_d$r_min, r),     c = c(df_d$c_max, c)),
             list(r = c(df_d$r_max, r + 1), c = c(df_d$c_max, c)),
             # The remaining rectangle which is not part of L-shape
             list(r = c(df_d$r_max, r + 1), c = c(df_d$c_min, c - 1))
           )
    )
  }

  # Filter for corner pins that create a valid L-shape
  possible_L_corner_pins <- possible_L_corner_pins %>%
    dplyr::filter(
      in_between(.data$row + params$r_adj, df_d$r_min, df_d$r_max) &
        in_between(.data$col + params$c_adj, df_d$c_min, df_d$c_max)
    )

  # If no valid corner pins remain, return an empty tibble
  if (NROW(possible_L_corner_pins) == 0) return(tibble::tibble())

  # Calculate rectangles for each pins
  possible_L_corner_pins <- possible_L_corner_pins %>%
    dplyr::mutate(
      rects = purrr::map2(.data$row, .data$col, get_rects)
    )

  # Check if the L-shape from each pin encloses all data points
  possible_L_corner_pins$cov_all <- purrr::map_lgl(
    possible_L_corner_pins$rects,
    function(rects) {
      # Check if all points are within at least one of the three rectangles
      all(
        (in_between(df$row, rects[[1]]$r[1], rects[[1]]$r[2]) &
           in_between(df$col, rects[[1]]$c[1], rects[[1]]$c[2])) |
          (in_between(df$row, rects[[2]]$r[1], rects[[2]]$r[2]) &
             in_between(df$col, rects[[2]]$c[1], rects[[2]]$c[2])) |
          (in_between(df$row, rects[[3]]$r[1], rects[[3]]$r[2]) &
             in_between(df$col, rects[[3]]$c[1], rects[[3]]$c[2]))
      )
    }
  )

  # Filter out corner pins that do not cover all points
  possible_L_corner_pins <- possible_L_corner_pins %>% dplyr::filter(.data$cov_all)

  # If no valid corner pins remain, return an empty tibble
  if (NROW(possible_L_corner_pins) == 0) return(tibble::tibble())

  # Calculate area for each valid L-shape
  possible_L_corner_pins$area <- purrr::map_dbl(
    possible_L_corner_pins$rects,
    function(rects) {
      util_area_of_rect(rects[[1]]$r[1], rects[[1]]$c[1], rects[[1]]$r[2], rects[[1]]$c[2]) +
        util_area_of_rect(rects[[2]]$r[1], rects[[2]]$c[1], rects[[2]]$r[2], rects[[2]]$c[2]) +
        util_area_of_rect(rects[[3]]$r[1], rects[[3]]$c[1], rects[[3]]$r[2], rects[[3]]$c[2])
    }
  )

  # Find the pin with the minimum area
  corner_pin <- possible_L_corner_pins %>%
    dplyr::filter(.data$area == min(.data$area)) %>%
    dplyr::slice(1)

  # Construct the final rectangles for the chosen pin
  rects <- corner_pin$rects[[1]]

  bd_L <- seq(1,4) %>% purrr::map_dfr(
    function(i){
      tibble::tibble(
        rect = i,
        r_min = min(rects[[i]]$r),
        c_min = min(rects[[i]]$c),
        r_max = max(rects[[i]]$r),
        c_max = max(rects[[i]]$c)
      )
    }
  )

  # Add final metadata
  bd_L %>%
    dplyr::mutate(
      # Rect 2 is the corner rectangle (by design)
      is_corner = .data$rect == 2,
      # 4 th rectangle is the one that is outside the L shape
      is_outside = .data$rect == 4,
      type_of_L = direction,
      area = corner_pin$area
    )
}


# Section: Modifications to Cells-Analysis Object ----


#' Modify cell analysis based on relationships between data and attribute blocks
#'
#' @description An internal utility function that allows for modifications of
#'   cell analysis objects based on the relationship between two points (data or
#'   attribute blocks). It supports operations like joining data blocks,
#'   connecting attributes to data blocks, detaching attributes, and changing
#'   header orientation tags.
#'
#' @param ca A cell analysis object to be modified
#' @param point_1 A point object containing type (data or attr) and which
#'   (row/column position)
#' @param point_2 A point object containing type (data or attr) and which
#'   (row/column position)
#' @param return_actions Logical; if TRUE returns possible actions instead of
#'   modifying the object
#' @param do_action Character; the action to perform ("join", "connect",
#'   "detach", "change_HOT")
#' @param HOT_to Character; new header orientation tag value when changing HOT
#'
#' @return If return_actions is TRUE, returns a character vector of possible
#'   actions (also include conflict indication like
#'   "conflict_multiple_selection"). Otherwise, returns a modified cell analysis
#'   object.
#'
#' @keywords internal
infer_util_cells_analysis_modification <- function(
    ca, point_1, point_2, return_actions = FALSE,
    do_action = "none",
    HOT_to = NULL) {

  # Initialize actions variable
  actions <- NULL

  # Early return if either point is empty
  if(NROW(point_1$which) == 0 || NROW(point_2$which) == 0) {
    return(if(return_actions) actions else ca)
  }

  # Ensure points have only one row
  point_1$which <- point_1$which[1, ]
  point_2$which <- point_2$which[1, ]

  # If returning actions, no need to perform any
  if(return_actions) do_action <- "none"

  # Helper function to wrap common return pattern
  wrap_result <- function(ca_mod) {
    class(ca_mod) <- core_cells_analysis_class
    return(ca_mod)
  }

  # Helper to extract data/attr GIDs based on point type
  get_gid <- function(point) {
    if(point$type == "data") {
      ca$data_blocks %>%
        dplyr::filter(
          .data$row == point$which$row,
          .data$col == point$which$col) %>%
        dplyr::pull(.data$data_gid)
    } else {
      ca$attr_data_map %>%
        dplyr::filter(
          .data$row == point$which$row,
          .data$col == point$which$col) %>%
        dplyr::pull(.data$attr_gid)
    }
  }

  # Extract GIDs based on point types
  gid1 <- get_gid(point_1)
  gid2 <- get_gid(point_2)

  # Check if gid1 and gid2 are of length 1, if not, check if they are linked
  if(length(gid1)>1 || length(gid2)>1){
    # Attempt to find a common gid in attr_data_map
    # This is to handle cases where multiple GIDs are returned
    con_chk <- ca$attr_data_map %>%
      dplyr::filter(
        .data$attr_gid %in% c(gid1, gid2),
        .data$data_gid %in% c(gid1, gid2)) %>%
      dplyr::distinct(.data$attr_gid, .data$data_gid)
    if(NROW(con_chk)==1) {
      gid1 <- gid1 %>% intersect(as.character(con_chk[1,]))
      gid2 <- gid2 %>% intersect(as.character(con_chk[1,]))
    }
  }

  # if gid1 and gid2 are still not of length 1, it means multiple selections or
  # no valid selection was made, so we return a conflict action(its not actually
  # action rather a message)
  if(length(gid1)>1 || length(gid2)>1) {
    # Early return with
    actions <- "conflict_multiple_selection"
    # Default case: return original ca or actions
    return(if(return_actions) actions else ca)
  }

  if(gid1 == gid2){
    # Early return with
    actions <- "conflict_same_selection"
    # Default case: return original ca or actions
    return(if(return_actions) actions else ca)
  }

  # Case 1: Both points are data blocks
  if(point_1$type == "data" && point_2$type == "data") {
    if(gid1 != gid2) {
      # If both data blocks are different, can perform join action
      actions <- c("join")

      if(do_action == "join") {

        # Join the two data blocks
        data_join_map <- tibble::tibble(data_gid = c(gid1, gid2)) %>%
          dplyr::mutate(data_gid_to = min(.data$data_gid))

        ca_out <- infer_util_gid_join(
          gid_join_map = data_join_map,
          gid_linked_list_of_df = ca[c("attr_data_map", "data_blocks")],
          gid_tag = "data_gid")

        ca_out$original_sheet <- ca$original_sheet

        # Fix for attr_data_map for nice_header_name which need to be unique per
        # attribute block within a data-block connected scope.
        ca_out <- infer_util_cells_analysis_fix_nice_header_name(
          ca_out,
          this_data_gid = data_join_map$data_gid_to[1])

        return(wrap_result(ca_out))
      }
    }
  }

  # Case 2: One point is data and one is attr
  if((point_1$type == "data" && point_2$type == "attr") ||
     (point_1$type == "attr" && point_2$type == "data")) {

    # Ensure data_gid and attr_gid are correctly assigned
    if(point_1$type == "data") {
      data_gid_this <- gid1
      attr_gid_this <- gid2
    } else {
      data_gid_this <- gid2
      attr_gid_this <- gid1
    }

    # Check if these are already linked
    this_existing_map <- ca$attr_data_map %>%
      dplyr::filter(.data$attr_gid == attr_gid_this & .data$data_gid == data_gid_this)

    if(NROW(this_existing_map) > 0) {
      # If linked already, can detach or change HOT
      actions <- c("detach", "change_HOT")
    } else {
      # If not linked, can connect
      actions <- c("connect")
    }

    if(do_action %in% actions) {
      ca_out <- ca  # Create modified copy

      if(do_action == "connect") {

        # Connect attribute block to data block
        attr_data_map_part <- ca$attr_data_map %>%
          dplyr::filter(.data$attr_gid == attr_gid_this) %>%
          dplyr::mutate(
            data_gid = data_gid_this,
            header_orientation_tag = ifelse(
              !is.null(HOT_to),
              HOT_to[1],
              util_most_frequent(.data$header_orientation_tag)
            ),
            nice_header_name = util_most_frequent(.data$nice_header_name)
          ) %>%
          dplyr::distinct()

        ca_out$attr_data_map <- ca_out$attr_data_map %>%
          dplyr::bind_rows(attr_data_map_part) %>%
          dplyr::distinct()

        # Fix for attr_data_map for nice_header_name which need to be unique per
        # attribute block within a data-block connected scope.
        ca_out <- infer_util_cells_analysis_fix_nice_header_name(
          ca_out,
          this_data_gid = data_gid_this)

        return(wrap_result(ca_out))
      }

      if(do_action == "detach") {
        disjoin <- tibble::tibble(data_gid = data_gid_this, attr_gid = attr_gid_this)
        ca_out$attr_data_map <- ca_out$attr_data_map %>%
          dplyr::anti_join(disjoin, by = c("data_gid", "attr_gid"))

        return(wrap_result(ca_out))
      }

      if(do_action == "change_HOT" && !is.null(HOT_to)) {
        ca_out$attr_data_map$header_orientation_tag[
          ca_out$attr_data_map$data_gid == data_gid_this &
            ca_out$attr_data_map$attr_gid == attr_gid_this
        ] <- HOT_to

        return(wrap_result(ca_out))
      }
    }
  }

  # Case 3: Both points are attribute blocks
  if(point_1$type == "attr" && point_2$type == "attr") {
    if(gid1 != gid2) {
      # If both attribute blocks are different, can perform join
      actions <- c("join")

      if(do_action == "join") {
        # Join the two attribute blocks
        attr_join_map <- tibble::tibble(attr_gid = c(gid1, gid2)) %>%
          dplyr::mutate(attr_gid_to = min(.data$attr_gid))

        ca_out <- infer_util_gid_join(
          gid_join_map = attr_join_map,
          gid_linked_list_of_df = ca[c("attr_data_map")],
          gid_tag = "attr_gid")

        # Fix multiple nice_header_name and header_orientation_tag
        ca_out$attr_data_map <- ca_out$attr_data_map %>%
          dplyr::group_by(.data$attr_gid, .data$data_gid) %>%
          dplyr::mutate(
            nice_header_name = util_most_frequent(.data$nice_header_name),
            header_orientation_tag = util_most_frequent(.data$header_orientation_tag)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::distinct()

        # Restore data blocks and original sheet
        ca_out$data_blocks <- ca$data_blocks
        ca_out$original_sheet <- ca$original_sheet

        return(wrap_result(ca_out))
      }
    }
  }

  # Default case: return original ca or actions
  return(if(return_actions) actions else ca)
}


# Helper function for ensuring unique nice_header_name per data-block connection
# for each attr_gids
infer_util_cells_analysis_fix_nice_header_name <- function(
    ca_out, this_data_gid) {

  if(missing(this_data_gid)){
    this_data_gid <- unique(ca_out$data_blocks$data_gid)
  }

  # Fix for attr_data_map for nice_header_name which need to be unique per
  # attribute block within a data-block connected scope.
  attr_data_map_this_d <- ca_out$attr_data_map %>%
    dplyr::filter(.data$data_gid %in% this_data_gid)
  attr_data_map_rest <- ca_out$attr_data_map %>%
    dplyr::filter(!(.data$data_gid %in% this_data_gid))

  # Recreate new nice_header_name from old nice_header_name
  nice_header_name_map <- attr_data_map_this_d %>%
    dplyr::distinct(.data$attr_gid,
                    nice_header_name_old = .data$nice_header_name) %>%
    dplyr::mutate(
      nice_header_name =
        util_make_unique_minimal(.data$nice_header_name_old)) %>%
    dplyr::select(c("attr_gid","nice_header_name"))

  # Replace these new names only for this data block
  attr_data_map_this_d <- attr_data_map_this_d %>%
    dplyr::select(-"nice_header_name") %>%
    dplyr::left_join(nice_header_name_map, by = "attr_gid")

  # Finally update in ca output
  ca_out$attr_data_map <- attr_data_map_this_d %>%
    dplyr::bind_rows(attr_data_map_rest) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$data_gid, .data$attr_gid)

  return(ca_out)
}

