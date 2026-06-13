

#' Compose and Stitch Cells from a Cell-Analysis Object
#'
#' This function reconstructs ("composes") by stitching data/value cells with
#' attribute cells from a given cell-analysis object. It combines attribute
#' mapping and data blocks to produce a data frame (or a list of data frames)
#' representing the structured, semantically-stitched table.
#'
#' @param ca A `cell_analysis` object containing cell attribute mapping and data
#'   block information.
#' @param simplify Logical. If `TRUE` (default), returns a single data frame of
#'   stitched cells. If `FALSE`, returns a list of data frames (composition of
#'   cells) for further processing (like collate columns).
#'
#' @return If `simplify = TRUE`, a data frame of stitched cells; otherwise, a
#'   list of data frames of class `core_cells_composition_class`.
#'
#' @details This function expects a valid `cell_analysis` object produced by the
#' table structure learning pipeline. It combines attribute-data mappings with
#' data blocks, joining cell values and header information according to the
#' mapping and orientation tags.
#'
#' If the object does not contain attribute-data mapping, an invisible `-1` is
#' returned and a warning is issued.
#'
#' @export
compose <- function(ca, simplify = TRUE) {

  # compose is a wrapper function to compose cells from a cell-analysis
  # object (ca) and returns a data frame with stitched cells.
  # ca: A cell-analysis object containing the necessary data for stitching.
  # Returns: A list of data frame with stitched cells.

  # Check that input is a valid cell_analysis object.
  if (!inherits(ca, "cells_analysis")) {
    rlang::abort("The input must be a cell_analysis object.")
  }

  # Warn and exit if attribute-data mapping is missing.
  if (is.null(ca$attr_data_map)) {
    rlang::warn("The cell_analysis object does not contain 'Attribute-wise Attribute-Data mapping'.")
    return(invisible(-1))
  }

  # Get the original cell table, remove class, convert to tibble.
  os <- ca$original_sheet

  # This is to reduce cells DF to normal tibble
  # Remove `cells`-class
  os <- tibble::as_tibble(unclass(os))

  # Retain only unique combinations of cell addresses and values.
  os <- os %>%
    dplyr::distinct(.data$row, .data$col, .data$value)

  # Left-join attribute mapping to get attribute values at addresses.
  admap_attr_wise_with_val <- ca$attr_data_map %>%
    dplyr::left_join(os, by = c("row", "col")) %>%
    dplyr::rename(attr_value = "value")

  # Left-join data blocks to get values at data cell addresses.
  d_dat_wth_val <- ca$data_blocks %>%
    dplyr::left_join(os, by = c("row", "col"))

  # Split the attribute-data mapping by data_gid and then by attr_gid.
  ca_adm_l <- admap_attr_wise_with_val %>%
    split(ca$attr_data_map$data_gid) %>%
    purrr::map(function(node) split(node , node$attr_gid))

  # For each data block, stitch all relevant attribute parts.
  comp_l0 <- ca_adm_l %>%
    purrr::map(function(node) {
      # For each group, subset the relevant data block.
      this_d_dat <- d_dat_wth_val %>%
        dplyr::filter(.data$data_gid == node[[1]]$data_gid[1])

      # node is a list of data frames, each corresponding to an attribute
      # For each attribute node, perform stitching with the relevant attribute and data.
      purrr::map(node, function(adm_part) {
        compose_stitch_direction(adm_part, this_d_dat)
      })
    })

  # For each data block, join all attribute-wise stitched data frames by cell address.
  comp_l <- purrr::map(
    comp_l0,
    function(df_list) {
      # For all but first, drop data_gid and value as they are same
      dfs2 <- purrr::map2(df_list, seq_along(df_list), function(x, i) {
        if (i == 1) {
          x
        } else {
          dplyr::select(x, -"data_gid", -"value")
        }
      })
      # Full-join all stitched attribute data by row and col, then remove duplicate rows.
      purrr::reduce(dfs2, function(x, y) {
        dplyr::full_join(
          x, y,
          by = c("row", "col")
        )
      }) %>%
        dplyr::distinct()
    })

  # If simplify is TRUE, bind all results into one data frame.
  if(simplify){
    dplyr::bind_rows(comp_l)
  } else {
    # If not simplified, return a list of data frames specifying the
    # "composition of cells" - class - so that it can be used for further
    # processing. (in collate_columns)
    class(comp_l) <- core_cells_composition_class
    comp_l
  }
}


# Helper function to stitch data and attribute cells together based on
# attribute-data mapping and direction.
compose_stitch_direction <- function(adm_part, d_dat_part) {

  # Note: A de-normalized form is used in amd_part here HOT is assigned. But
  # we'll leave it as it is.

  # Select distinct attribute cell addresses and values from the attribute mapping.
  a0 <- adm_part %>%
    dplyr::distinct(.data$row, .data$col, .data$attr_value)

  # Select distinct data cell addresses and values from the data block.
  d0 <- d_dat_part %>%
    dplyr::distinct(.data$data_gid, .data$row, .data$col, .data$value)

  # Extract attribute name and header orientation for naming and stitching.
  attr_name <- adm_part$nice_header_name[1]
  direction <- adm_part$header_orientation_tag[1]

  # Attach headers to data block cells according to orientation and address.
  d1 <- attach_header(dat = d0, hdr = a0, direction = direction)

  # Rename the attached attribute column to the friendly attribute name.
  colnames(d1)[which(colnames(d1) == "attr_value")] <- attr_name

  d1
}



