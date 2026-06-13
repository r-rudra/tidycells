
#' Analyze Cells
#'
#' Performs a multi-phase analysis pipeline on a classified
#' [`cells`][cells-class] object, detecting data blocks, attribute orientation,
#' header tags, and post-processing the cell structure.
#'
#' This function sequentially possesses the cell data, builds attribute-data
#' mappings, clusters attributes, tags header orientation, and produces a
#' comprehensive analysis result. Input should be a `cells` object that has
#' completed value-attribute classification stage.
#'
#' @param d A [`cells`][cells-class] object that has completed value-attribute
#'   classification.
#'
#' @details The analysis proceeds in the following phases:
#' \enumerate{
#'   \item Pre-processing of the cell data.
#'   \item Fundamental attribute-data mapping and modification.
#'   \item Attribute clustering and regrouping.
#'   \item Header orientation tagging.
#'   \item Post-processing and generation of the final analysis result.
#' }
#'
#'   The returned object contains detailed information about detected data
#'   blocks, attribute clusters, header orientation, and other cell structure
#'   attributes.
#'
#' @return A detailed `cells_analysis` object describing the cell data structure
#'   and orientation.
#'
#' @seealso [`compose`][compose()],
#'   [`collate_columns`][collate_columns()],
#'
#' @export
analyze_cells <- function(d) {

  # Pre-processing
  l0 <- infer_analysis_phase_0_preprocess(d)

  # Fundamental Attr-Data Map
  #
  # In this phase: 1) d_att is modified (potentially) 2) d_dat is modified
  # (potentially) 3) admap is created
  l1 <- infer_analysis_phase_1_admap(l0)

  # Split/Regroup of Form Cluster of Attributes
  #
  # In this phase: 1) d_att is modified (potentially) 2) d_dat is not modified
  # 3) admap is modified (potentially) and format is changed to attr-wise means
  # for all attr cell address is given.
  l2 <- infer_analysis_phase_2_attr_cluster(
    list(
      # Taken from l1
      admap = l1$admap,
      d_dat = l1$d_dat,
      # Taken from l0
      d_att = l1$d_att
    )
  )

  # Attach Header Orientation Tags
  #
  # In this phase: 1) d_att is not modified and not required  2) d_dat is not
  # modified 3) admap is modified with added header orientation tags
  l3 <- infer_analysis_phase_3_header_orientation_tag(
    list(
      # Taken from l2
      admap = l2$admap,
      # Taken from l1
      d_dat = l1$d_dat
      # Note d_att is no longer required as it's part of admap-"attr-wise"
    )
  )

  # Post-processing - which will be returned as the final result
  infer_analysis_phase_4_postprocess(
    list(
      # Taken from l3 (this is HOT tagged admap)
      tagged = l3$tagged,
      # Taken from l1
      d_dat = l1$d_dat
    ),
    original_cells = d)

}
