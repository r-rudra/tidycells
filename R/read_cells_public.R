# Read Cells


#' Read and Process Tabular Data as Cells and Sheets
#'
#' Reads a file and returns its tabular contents as a standardized `sheets`
#' object, with optional multi-stage processing for cell classification, block
#' detection, composition, and column collation.
#'
#' This function detects the file type, reads the contents as tidy cells, and
#' optionally advances through one or more finalization stages, including
#' value-attribute classification, cell analysis, composition, and column
#' collation. Stages may be controlled via arguments for flexible and
#' incremental workflows.
#'
#' @param file Path to the tabular data file (Excel, CSV, etc.).
#' @param finalize_through_stages Logical. If \code{TRUE}, process the sheets
#'   through classification and analysis stages. If \code{FALSE}, return the raw
#'   sheets after reading and basic detection. Default from package option if
#'   missing.
#' @param finalize_till Character. If \code{finalize_through_stages} is
#'   \code{TRUE}, specifies the last stage to finalize: \code{"collate_column"},
#'   \code{"compose"}, \code{"analyze_cells"}, or
#'   \code{"value_attribute_classify"}. Default is \code{"collate_column"} (from
#'   package option if missing).
#' @param collate_columns_on_whole Logical. If \code{TRUE} (default), collation
#'   is done across all sheets together. If \code{FALSE}, columns are collated
#'   per sheet independently. Default from package option if missing.
#' @param show_progress Logical. Display progress bar during multi-stage
#'   processing. Default is \code{finalize_through_stages} value.
#'
#' @details The main workflow is as follows:
#' \enumerate{
#'   \item Detect file type and read contents as tidy `cells` objects.
#'   \item If \code{finalize_through_stages} is \code{TRUE}, optionally finalize
#'         through multiple stages:
#'     \itemize{
#'       \item Value-attribute classification (\code{"value_attribute_classify"})
#'       \item Cells analysis (\code{"analyze_cells"})
#'       \item Compose cells (\code{"compose"})
#'       \item Collate columns (\code{"collate_column"})
#'     }
#'   \item Progress is reported at each stage if \code{show_progress} is \code{TRUE}.
#'   \item If \code{collate_columns_on_whole} is \code{TRUE}, columns are collated
#'         across all sheets; otherwise, per sheet.
#' }
#'
#' The returned object is:
#' \itemize{
#'   \item A `sheets` object (list of `cells` objects) if no finalization.
#'   \item A processed list after the requested stage, e.g. value-attribute classified,
#'         analyzed, composed, or collated columns.
#' }
#'
#' For more details on the `cells` and `sheets` classes, see [cells-class] and
#' [sheets-class].
#'
#' @return A `sheets` object or a processed list of cell/cell composition
#' objects, depending on the finalization stage requested.
#'
#' @seealso [cells-class], [sheets-class], [compose()], [collate_columns()],
#'   [analyze_cells()]
#' @export
read_cells <- function(
    # Path of file
  file,
  # If TRUE then finalize through all stages, if FALSE then return sheets as
  # is after reading based on file signature based type detection and invoking
  # appropriate read function
  finalize_through_stages,
  # If finalize_through_stages == TRUE, then this argument is used to
  # determine till which stage to finalize the sheets. Default is
  # "collate_column" which means to finalize till collate columns stage. If
  # "compose" then finalize till compose stage, if "analyze_cells" then
  # finalize till analyze cells etc. valid values are
  # "collate_column","compose", "analyze_cells", "value_attribute_classify"
  finalize_till,
  # Default is TRUE, if FALSE then collate columns on each sheets separately
  collate_columns_on_whole,
  show_progress = finalize_through_stages) {

  # If missing file, return NULL
  if (missing(file) || is.null(file)) {
    # TODO possible to support
    return(NULL)
  }

  # If finalize_through_stages is not provided, see if it is set in package
  # options, if not then set it to FALSE
  if (missing(finalize_through_stages)) {
    finalize_through_stages <- core_opt_get(
      "finalize_through_stages", default = FALSE)
  }

  # If finalize_till is not provided, see if it is set in package options,
  # if not then set it to "collate_column"
  if(missing(finalize_till)) {
    finalize_till <- core_opt_get(
      "finalize_till", default = "collate_column")
  }

  # Validate finalize_till argument
  finalize_till <- match.arg(
    finalize_till, c("collate_column","compose", "analyze_cells", "value_attribute_classify"))

  # If collate_columns_on_whole is not provided, see if it is set in package
  # options, if not then set it to TRUE
  if (missing(collate_columns_on_whole)) {
    collate_columns_on_whole <- core_opt_get(
      "collate_columns_on_whole", default = TRUE)
  }

  # Validate file input
  file_op_validate_file_input(file)

  # Progress bar for entire porcess
  if(show_progress) pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)

  # Helper function
  progress_update <- function(value, final = FALSE) {
    if(show_progress) {
      if(value >= 100) final <- TRUE
      utils::setTxtProgressBar(pb, value)
      if(final) {
        close(pb)
      }
    }
  }

  # Read the cells from the file as sheets
  this_sheets <- file_op_detect_and_read(file) %>%
    as_sheets()

  progress_update(10)

  # If finalize_through_stages is FALSE, then we do not process the sheets
  if(!finalize_through_stages) {
    # If not finalizing through all stages, return the sheets as is
    progress_update(100)
    return(this_sheets)
  }

  # If finalize_through_stages is TRUE, then process the sheets through various
  # stages based on finalize_till argument.
  if(finalize_through_stages) {
    # If finalizing through all stages (till collate column), process the sheets
    this_sheets_va <- this_sheets %>%
      purrr::map(value_attribute_classify)

    progress_update(20)

    # If finalize_till is "value_attribute_classify", return the sheets after
    # value-attribute classification
    if(finalize_till == "value_attribute_classify") {
      progress_update(100)
      return(this_sheets_va)
    }

    # Otherwise, continue with further processing. Next is cells analysis.
    this_sheets_ca <- this_sheets_va %>%
      # This is for progress bar update. Otherwise normal map should have
      # worked.
      purrr::map2(
        seq_along(this_sheets_va),
        function(x,i) {
          ca <- analyze_cells(x)
          progress_update(20 + (i / length(this_sheets_va)) * 60)
          ca
        })

    progress_update(80)

    # If finalize_till is "analyze_cells", return the sheets after analysis
    if(finalize_till == "analyze_cells") {
      progress_update(100)
      return(this_sheets_ca)
    }

    # Compose cells based on the analysis results but behavior depends on
    # finalize_till == "compose" or not

    # Otherwise, continue with further processing. Next is composing cells based
    # on cells analysis.
    if(finalize_till == "compose"){
      # Get as list of data frames format
      this_sheets_comp <- this_sheets_ca %>%
        purrr::map(~compose(.x, simplify = TRUE))
    }

    if(finalize_till == "collate_column") {
      # It means next stage i.e. collate_columns is requested, so we need to
      # get the sheets in a format that can be used for collating columns.

      # Get as list of cells composition object
      this_sheets_comp <- this_sheets_ca %>%
        purrr::map(~compose(.x, simplify = FALSE))

      # Prepare the composition for collating columns in the next stage

      if(collate_columns_on_whole){
        # In this case, we need to collate columns on the whole file across all
        # sheets

        # Attach sheet names to the composed sheets

        # Tag the composed sheets with their sheet names
        this_sheets_comp_tagged <- purrr::imap(
          this_sheets_comp,
          function(.x, .y) {
            purrr::map(.x, function(df) dplyr::mutate(df, sheet_name = .y))
          }) %>%
          purrr::flatten()

        # Set names for the composed sheets with sheet name and column names
        names(this_sheets_comp_tagged) <- purrr::imap(
          this_sheets_comp,
          function(sublist, sheet_name) {
            paste0(sheet_name, "_", names(sublist))
          }) %>% purrr::flatten() %>%
          as.character()

        # Assign core_cells_composition_class
        class(this_sheets_comp_tagged) <- core_cells_composition_class

      }

    }

    progress_update(90)


    # If finalize_till is "compose", return the sheets after composing cells
    if(finalize_till == "compose") {
      progress_update(100)
      return(this_sheets_comp)
    }

    # Otherwise, continue with further processing. Next is collating columns.
    if(finalize_till == "collate_column") {
      if(collate_columns_on_whole){
        this_sheets_cc <- collate_columns(this_sheets_comp_tagged)
      } else {
        this_sheets_cc <- this_sheets_comp %>%
          purrr::map(collate_columns)
      }

      progress_update(100)
      return(this_sheets_cc)
    }
  }

}
