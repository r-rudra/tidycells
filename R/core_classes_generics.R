

#' Print a Cells Analysis Object
#'
#' @param x A Output of cells/sheet analysis.
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.cells_analysis <- function(x, ...) {
  # Print a summary of the Cells Analysis object
  msg <- c(
    paste0("A ", cli_bold_blue("Cells Analysis"), " Object - with"),
    paste0(cli_green(cli_bullet()),"No. of data/value-blocks(s): ",
           cli_blue(dplyr::n_distinct(x$attr_data_map$data_gid)))
  )

  cat(paste0(msg, collapse = "\n"))

  return(invisible(x))
}


#' Print a Cells Composition Object
#'
#' Prints a summary of the Cells Composition object, including the number of
#' node/data/value-blocks.
#'
#' @param x A Cells Composition object.
#' @param ... Further arguments passed to methods (currently unused).
#'
#' @return Invisibly returns the Cells Composition object.
#' @keywords internal
#' @export
print.cells_composition <- function(x, ...) {
  # Print a summary of the Cells Composition object
  msg <- c(
    paste0("A ", cli_bold_blue("Cells Composition"), " Object - with"),
    paste0(cli_green(cli_bullet()),"No. of node/data/value-blocks(s): ",
           cli_blue(length(x)))
  )

  cat(paste0(msg, collapse = "\n"))

  return(invisible(x))
}

#' Print a Sheets Object
#'
#' Prints a summary and details of a Sheets object, including the number of
#' sheets and their sizes. Optionally validates the object before printing.
#'
#' @param x A Sheets object.
#' @param ignore_validation Logical. If TRUE, skips validation of the object.
#'   Default is FALSE.
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.sheets <- function(x, ignore_validation = FALSE, ...) {
  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    # Validate the sheets object
    chk <- core_validate_sheets(x)
  }

  if(isFALSE(chk)){
    msg <- cli_bold_red(paste0("Malformed ",cli_blue("Sheets")," Object!"))
    msg <- c(
      msg,
      paste0(cli_red(cli_bullet()),"Errors:"),
      paste0(cli_red(cli_cross())," ", (attr(chk, "msg")))
    )
    cat(paste0(paste0(msg, collapse = "\n"), "\n"))
    return(invisible(x))
  }

  # Print the summary message
  msg <- paste0(
    "A ",cli_bold_blue("Sheets")," Object - with ",
    cli_blue(length(x)), " sheet(s). "
  )

  cat(paste0(msg, "\n"))

  purrr::iwalk(
    x,
    function(sheet, name) {
      # Print each sheet
      msg <- paste0(
        cli_bullet(),
        cli_bold_blue("Sheet: "), cli_blue(name), " - ",
        cli_blue(NROW(sheet)), " cell(s). ",
        ifelse(
          NCOL(sheet)*NROW(sheet) > 0,
          yes = paste0(
            "Size ",cli_blue(max(sheet$row)), " x ", cli_blue(max(sheet$col))
          ),
          no = "<Empty Cells Object>"
        )
      )

      cat(paste0(msg, "\n"))

    }
  )

  return(invisible(x))
}

#' Print Summary for Cells Object
#'
#' Prints a summary of a Cells object, including the number of cells and their
#' dimensions. Optionally glances at the data in a styled view and validates the
#' object before printing.
#'
#' @param x A Cells object.
#' @param ignore_validation Logical. If TRUE, skips validation of the object.
#'   Default is FALSE.
#' @param xl_view Logical. If TRUE (default), displays a Excel - styled glimpse
#'   of the cell data (if required packages are available).
#' @param ... Further arguments passed to methods (currently unused).
#' @keywords internal
#' @export
print.cells <- function(x, ignore_validation = FALSE, xl_view = TRUE, ...) {
  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    chk <- core_cells_validation_end_use(x)
  }


  # If validation fails, then it's Malformed Cells object
  if(isFALSE(chk)){
    return(invisible(x))
  }

  # If validation passes, then proceed further
  msg <- c(
    paste0(
      "A ",cli_bold_blue("Cells")," Object - with ",
      cli_blue(NROW(x)), " cell(s), ",
      ifelse(
        NCOL(x)*NROW(x) > 0,
        yes = paste0(
          "Size ",cli_blue(max(x$row)), " x ", cli_blue(max(x$col))
        ),
        no = "<Empty Cells Object>"
      )
    )
  )

  if (utils::hasName(x, "type")) {
    msg <- c(
      msg,
      paste0(
        cli_bullet(),
        "With ",
        cli_blue("Value/Attribute Classification")
      )
    )
  }

  # print msgs
  cat(paste0(paste0(msg, collapse = "\n"), "\n"))

  if(xl_view && cli_is_available()) {
    cat(
      paste0(
        cli_bullet(), cli_blue("Glimpse of The Data Content:\n"),
        cli_bullet(), "(Note: Blank rows/columns are not considered)\n"
      )
    )

    # Print a glimpse of the data content
    cli_print_excel_styled_matrix(
      mat = as.matrix.cells(x),
      col_width = 8,
      max_cols = 10,
      max_rows = 10
    )
  }

  return(invisible(x))

}

#' Convert Cells Object to Matrix
#'
#' Converts a valid Cells object to a matrix. Ensures structure is appropriate
#' for conversion and arranges data accordingly.
#'
#' @param x A Cells object.
#' @return A matrix representing the cell values.
#' @keywords internal
#' @export
as.matrix.cells<- function(x, ...) {

  convertible <- FALSE

  if(utils::hasName(x, "row") && utils::hasName(x, "col") && utils::hasName(x, "value")) {
    if(is.integer(x$row) && is.integer(x$col) && is.character(x$value)) {
      if(all(x$row > 0) && all(x$col > 0)) {
        # Check if the values are convertible to a matrix
        convertible <- TRUE
      }
    }

  }

  if(!convertible) {
    rlang::abort(
      "The input object is not a valid 'cells' object for conversion to matrix."
    )
  }

  # restore the class to 'data.frame'
  class(x) <- "data.frame"

  # note this method assumes data is free from sparse nature
  # i.e. empty rows and columns will be removed
  m <- x[c("row", "col", "value")] %>%
    dplyr::arrange(.data$row,.data$col) %>%
    tidyr::spread(.data$col, .data$value) %>%
    dplyr::select(-"row") %>%
    as.matrix()

  colnames(m) <- NULL
  row.names(m) <- NULL
  m

}




#' Plot a Cells Object as a Grid
#'
#' Visualizes a \code{Cells} object as a grid using \code{ggplot2}, where each
#' cell is represented by a tile. The fill color and text labels can be
#' customized.
#'
#' @param x A \code{Cells} object to be plotted.
#' @param ... Additional arguments passed. (currently unused)
#' @param fill Character. Name of the column in \code{x} to use for tile fill
#'   colors. If missing, uses "type" if present, otherwise "data_type".
#' @param fill_alpha Numeric. Alpha transparency for the fill color of tiles.
#' @param ignore_validation Logical. If \code{TRUE}, skips validation of the
#'   object before plotting. Default is \code{FALSE}.
#' @param no_fill Logical. If \code{TRUE}, disables fill coloring for tiles.
#'   Default is \code{FALSE}.
#' @param txt_size Numeric. Text size for cell labels.
#' @param txt_alpha Numeric. Alpha transparency for cell labels.
#' @param no_txt Logical. If \code{TRUE}, disables text labels on the grid.
#'   Default is \code{FALSE}.
#' @param max_txt_len Integer. Maximum length of text displayed in each cell
#'   before truncation. Default is \code{10}.
#' @param no_plot Logical. If \code{TRUE}, returns the ggplot object without
#'   displaying / plotting it. Default is \code{FALSE}.
#' @param boundaries Data frame containing boundary rectangles to be drawn on
#'   the grid. Should have columns `c_min`, `c_max`, `r_min`, `r_max` for
#'   coordinates, and optionally a grouping variable.
#' @param add_cell_address Logical. If \code{TRUE}, adds cell addresses as text
#'   labels on the grid. Default is \code{FALSE}.
#' @param no_legend Logical. If \code{TRUE}, disables the legend in the plot.
#'   Default is \code{TRUE}.
#' @param cell_connection_map Data frame containing information about cell
#'   connections (required columns: `col_from`, `row_from`, `col_to`, `row_to`;
#'   optional column: `color`). Used to draw arrows between cells. This is
#'   mainly for inspecting how cells are connected post composition or collate
#'   stage.
#' @param connection_line_type Character. Type of connection lines to draw.
#'   Options are "L-shaped" or "Curved". Default is "L-shaped".
#' @param connection_line_alpha Numeric. Alpha transparency for the connection
#'   lines between cells. Default is \code{0.7}.
#' @param connection_line_add_jitter Logical. If \code{TRUE}, adds slight random
#'   jitter to the start and end points of connection lines to reduce overlap.
#'   Default is \code{TRUE}.
#' @param connection_line_overlay_text_color Logical. If \code{TRUE}, overlays
#'   text color on top of connection lines based on the `color` column in
#'   \code{cell_connection_map}. Default is \code{TRUE}.
#' @param allow_rc_df Logical. If \code{TRUE}, allows the input to be any rc_df.
#'   This is designed for mainly testing and development purposes. Default is
#'   \code{FALSE}.
#' @param omit_NSE Logical. If \code{TRUE}, treats the \code{fill} argument as a
#'   character string rather than a symbol. This is mainly for internal use to
#'   omit non-standard evaluation (NSE) in the fill argument. Default is
#'   \code{FALSE}.
#' @param display_warning_as_plot Logical. If \code{TRUE}, displays a warning
#'   plot instead of a text warning when the object has more than 200 rows or
#'   columns. Default is \code{FALSE}. This is mainly for shiny modules or for
#'   displaying warnings in a more user-friendly way.
#' @param scale_fill_values Named character vector. If provided, specifies the
#'   fill colors for specific types or data types in the plot. This allows to
#'   customize the color mapping for specific fill types, especially for plot of
#'   cells-analysis.
#'
#'
#'
#' @details
#' - Requires the \code{ggplot2} package.
#' - Validates the \code{Cells} object unless \code{ignore_validation} is \code{TRUE}.
#' - Warns and does not do anything if the object has more than 200 rows or columns.
#' - If the specified \code{fill} column is not found in \code{x}, the function aborts with an error.
#' - Text in cells longer than \code{max_txt_len} is truncated and appended with "...".
#' - The function returns (invisibly) the \code{ggplot} object.
#'
#' @return Invisibly returns the \code{ggplot} object for further customization
#'   or display.
#'
#' @keywords internal
#' @export
plot.cells <- function(
    x, ...,
    fill,
    fill_alpha = 1,
    ignore_validation = FALSE,
    no_fill = FALSE,
    txt_size = 3, txt_alpha = 1, no_txt = FALSE, max_txt_len = 10,
    no_plot = FALSE,
    boundaries = NULL,
    add_cell_address = FALSE,
    no_legend = TRUE,
    # For adding L-shaped arrows between cells (mainly required for cell trace
    # plot). For inspecting how cells are connected post composition or collate
    # stage.
    cell_connection_map = tibble::tibble(),
    connection_line_type = c("L-shaped", "Curved"),
    connection_line_alpha = 0.7,
    connection_line_add_jitter =  TRUE,
    connection_line_overlay_text_color = TRUE,
    # This is mainly for testing and development purposes
    allow_rc_df = FALSE,
    rc_df_auto_txt_n_fill = TRUE,
    # This is for omitting NSE in fill (required internally)
    omit_NSE = FALSE,
    display_warning_as_plot = FALSE,
    scale_fill_values = NULL,
    auto_round_values = FALSE,
    declutter_based_on_value = FALSE,
    unify_on_row_col = FALSE
    ) {

  # Check if ggplot2 is available
  if(!pkg_is_available("ggplot2")) {
    rlang::warn(
      "The 'ggplot2' package is required for plotting cells objects."
    )
    return(invisible(NULL))
  }

  if(declutter_based_on_value){
    unify_on_row_col <- TRUE
  }

  # This is mainly to assist in testing and development
  if(allow_rc_df){
    ignore_validation <- TRUE

    val <- ""

    if(rc_df_auto_txt_n_fill){

      cht <- x |> purrr::map_lgl(is.character)
      if(any(cht)){
        val <- x[[which(cht)[1]]]
      }

    }

    if(!utils::hasName(x, "value")) {
      x$value <- val
    }

    if(missing(fill)) {
      fill <- "value"
    }
  }

  if(auto_round_values){
    x$value <- util_auto_round(x$value)
  }

  if(unify_on_row_col){
    dchk <- (x |> dplyr::distinct(.data$row, .data$col) |> NROW()) == NROW(x)
    if(!dchk){
      x <- x |>
        dplyr::group_by(.data$row, .data$col) |>
        dplyr::summarise(
          # Apply mean to all numeric columns
          dplyr::across(
            tidyselect::where(is.numeric),
            \(x) mean(x, na.rm = TRUE)
          ),
          # Apply paste logic to EVERYTHING ELSE (the default behavior)
          dplyr::across(
            !tidyselect::where(is.numeric),
            \(x) unique(x) |> sort() |> paste0(collapse = "+")
          ),
          .groups = "drop"
        )
    }
  }

  if(declutter_based_on_value){
    x <- x |>
      dplyr::group_by(
        dplyr::across(!c("row", "col")), .data$value) |>
      dplyr::mutate(
        drc = (.data$row - mean(.data$row, na.rm = TRUE))^2 +
          (.data$col - mean(.data$col, na.rm = TRUE))^2) |>
      dplyr::mutate(
        value_declutter = dplyr::if_else(
          .data$drc == min(.data$drc, na.rm = TRUE),
          .data$value, "")) |>
      dplyr::ungroup() |>
      dplyr::select(-c("value","drc"))
    colnames(x)[colnames(x)=="value_declutter"] <- "value"
  }

  # If ignore_validation is TRUE, skip validation
  if (ignore_validation) {
    chk <- TRUE
  }else{
    chk <- core_cells_validation_end_use(x)
  }

  # If validation fails, then it's Malformed Cells object
  if(isFALSE(chk)){
    return(invisible(NULL))
  }

  # Size based warning
  if(max(x$row)-min(x$row)>200 || max(x$col)-min(x$col)>200){

    if(display_warning_as_plot) {
      # If display_warning_as_plot is TRUE, then show a warning plot
      warn_g <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate(
          "text",
          x = 0.5, y = 0.5,
          label = "This display is not suitable for cells with more than 200 rows or columns.",
          size = 6, color = "orange", fontface = "bold"
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)

      if(!no_plot) {
        plot(warn_g, ...)
      }

      return(invisible(warn_g))

    }

    # Else just warn
    rlang::warn(
      "Plot is not suitable for cells with more than 200 rows or columns."
    )

    return(invisible(NULL))

  }

  # If validation passes, then proceed further

  full_grid <- expand.grid(row = seq(min(x$row), max(x$row)),
                           col = seq(min(x$col), max(x$col)),
                           stringsAsFactors = FALSE)

  if (missing(fill)) {
    if (utils::hasName(x, "type")) {
      fill <- "type"
    } else {
      fill <- "data_type"
    }
  }

  # By default, fill is a NSE compatible (i.e. fill = col and fill = "col" both
  # plays same role), but if omit_NSE is TRUE, then we take fill as character
  # string only.
  if(!omit_NSE){
    # Fill can be either a character string or a symbol
    fill <-  rlang::as_name(rlang::enexpr(fill))
    # After this fill will always be a character string
  }


  if (!(fill %in% colnames(x)) && !no_fill) {
    rlang::abort("`fill` should be either of column names.")
  }

  # truncate text values if they exceed max_txt_len
  x$value <- ifelse(
    nchar(x$value) > max_txt_len,
    paste0(substr(x$value, 1, max_txt_len), "..."),
    x$value
  )

  # Pre-calculate text angle if required later
  if (!no_txt) {
    row_len <- max(x$row)-min(x$row)+1
    col_len <- max(x$col)-min(x$col)+1
    # If the number of columns is greater than rows, then we can rotate the text
    # (optionally)
    if(col_len/row_len > 3){
      # Calculate text angle based on the length of the text
      x$text_angle <- ifelse(nchar(x$value) > max_txt_len/4, 90, 0)
    } else {
      x$text_angle <- 0
    }
  }

  if (no_fill) {
    g <- ggplot2::ggplot(x, ggplot2::aes(.data$col, -.data$row))+
      ggplot2::geom_tile(color = "#00000046", alpha = 0.1, na.rm = TRUE, width = 1, height = 1)
  } else {
    g <- ggplot2::ggplot(x, ggplot2::aes(.data$col, -.data$row, fill = get(fill))) +
      ggplot2::labs(fill = tools::toTitleCase(stringr::str_replace_all(fill,"_"," ")))

    g <- g +
      ggplot2::geom_tile(color = "#00000046", na.rm = TRUE, width = 1, height = 1, alpha = fill_alpha)
  }

  g <- g +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      # For transparent background
      plot.background  = ggplot2::element_rect(fill = "transparent", color = NA), # plot
    )


  # Adjustment for Fill Scales ----------------------------------------------

  # Specific adjustments for continuous fill types apart from PoV and PoA (for
  # these adjustments are done subsequently)
  if(is.numeric(x[[fill]]) && !(fill %in% c("PoV","PoA"))){
    # If fill is numeric, use a gradient scale
    g <- g +
      ggplot2::scale_fill_gradient(low = "#00BFC4", high = "#F8766D")
  }

  # Fixed color scale for specific fill types so that cropping in visualization
  # does not change colors if specific type or data_type is missed during
  # cropping or scrolling.
  if((fill %in% c("data_type", "type")) && !no_fill) {

    # Fixed color scale
    if(!is.null(scale_fill_values)) {
      # Take user supplied scale_fill_values
      master_type_to_col_map <- scale_fill_values
    } else {
      # Otherwise use the default master map

      # Fixed color scale for specific fill types for all:
      # core_cell_recognized_format() (Here col means color)
      master_type_to_col_map <- c(
        logical = "#FA837A", categorical = "#F78981", date = "#FF6E63",
        time = "#FC5B4E", character = "#F8766D", numeric = "#00BFC4",
        # Added for plot.cells_analysis - type cases
        attr = "#F8766D", data = "#00BFC4",
        blank = "#A3A3A3A3")
    }

    # Filter the master map to only include the types present in the data. But
    # this is not required !
    #
    # this_type_to_col_map <-  master_type_to_col_map[unique(x[[fill]])]
    #
    # Use the master map directly
    g <- g +
      ggplot2::scale_fill_manual(
        values = master_type_to_col_map,
        na.value = "transparent"
      )
  }

  # Similar Adjustments for PoV and PoA

  if((fill == "PoA") && !no_fill) {

    # Fixed color scale for specific fill types
    g <- g +
      ggplot2::scale_fill_gradient(
        low = "#00BFC4",
        high = "#F8766D",
        limits = c(0, 1),      # <-- fixes the domain
        na.value = "transparent"
      )
  }

  if((fill == "PoV") && !no_fill) {

    # Fixed color scale for specific fill types
    g <- g +
      ggplot2::scale_fill_gradient(
        # Reverse the colors for PoV to match the PoA
        low = "#F8766D",
        high = "#00BFC4",
        limits = c(0, 1),      # <-- fixes the domain
        na.value = "transparent"
      )
  }

  # Add the full grid to ensure all cells are represented

  g <- g +
    ggplot2::geom_tile(
      data = full_grid,
      ggplot2::aes(x = .data$col, y = -.data$row),
      fill = NA, color = "#00000015", alpha = 0.1, width = 1, height = 1, inherit.aes = FALSE
    )

  if(add_cell_address) {
    # Add cell addresses as text labels
    g <- g +
      ggplot2::geom_text(
        data = full_grid,
        ggplot2::aes(x = .data$col-0.2, y = -.data$row, label = paste0(.data$row, ", ", .data$col)),
        size = 2, alpha = 0.3, color = "#00000099", na.rm = TRUE,
        inherit.aes = FALSE
      )
  }

  # If boundaries are provided, add them to the plot
  if(!is.null(boundaries) && is.data.frame(boundaries)) {

    # Check if boundaries have the a grouping var
    group_var <- setdiff(
      colnames(boundaries),
      c("c_min", "c_max", "r_min", "r_max"))

    # if no grouping var, then use create a dummy one
    if(length(group_var) == 0) {
      boundaries$gid <- ""
      group_var <- "gid"
    }

    # In case multiple grouping vars are present, then use the first one
    group_var <- group_var[1]

    g <- g +
      ggplot2::geom_rect(
        data = boundaries,
        ggplot2::aes(
          xmin = .data$c_min-0.5, xmax = .data$c_max+0.5, ymin = -.data$r_min+0.5, ymax = -.data$r_max-0.5,
          group = .data[[group_var]]
        ),
        color = "red", inherit.aes = FALSE, alpha = 0.1,
        #size = 1.5,
        lty = 2,
        linewidth = 0.7,
        fill = "#8a5f5c",
        na.rm = TRUE
      )

    # Add text labels for boundaries
    if(!no_txt){
      g <- g +
        ggplot2::geom_text(
          data = boundaries,
          ggplot2::aes(
            x = (.data$c_min + .data$c_max) / 2, y = -(.data$r_min + .data$r_max) / 2,
            label = .data[[group_var]]
          ),
          inherit.aes = FALSE, color = "#4f1e1a", size = 5, na.rm = TRUE
        )
    }
  }

  if (!no_txt) {
    g <- g +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$value,
                     angle = .data$text_angle),
        size = txt_size, alpha = txt_alpha, na.rm = TRUE)
  }

  if(no_legend) {
    g <- g + ggplot2::theme(legend.position = "none")
  }

  # Add the L-shaped arrows from cell_connection_map
  if (NROW(cell_connection_map) > 0) {

    if(connection_line_overlay_text_color && !no_txt){

      x_this <- x %>%
        dplyr::inner_join(
          cell_connection_map,
          by = c("row"="row_to", "col"="col_to"),
          suffix = c("","_2")) %>%
        dplyr::select(c("row","col","value","color","text_angle"))


      g <- g +
        ggplot2::geom_text(
          data = x_this,
          ggplot2::aes(
            x = .data$col, y = -.data$row,
            label = .data$value,
            angle = .data$text_angle,
            color = .data$color),
          inherit.aes = FALSE,
          size = txt_size, alpha = 1, na.rm = TRUE)

    }

    # Special case of vertical only lines

    cell_connection_map <- cell_connection_map %>%
      dplyr::mutate(vertical_only = .data$col_from==.data$col_to)


    if(connection_line_add_jitter){
      NR <- NROW(cell_connection_map)
      rng <- c(-0.15,0.15)
      # Time dependent set seed to mimic non-controlled random nature
      set.seed(Sys.time())
      cell_connection_map$row_from <- cell_connection_map$row_from + stats::runif(NR, rng[1],rng[2])
      cell_connection_map$row_to <- cell_connection_map$row_to + stats::runif(NR, rng[1],rng[2])
      cell_connection_map$col_from <- cell_connection_map$col_from + stats::runif(NR, rng[1],rng[2])
      cell_connection_map$col_to <- cell_connection_map$col_to + stats::runif(NR, rng[1],rng[2])
    }


    # You have to use like:
    #
    # x %>% plot( cell_connection_map = data.frame( row_from = c(5,4), col_from
    # = 4, row_to = c(2,7), col_to = 2, color = c("red","blue") ))

    # Check if color column is present in cell_connection_map. If not, add a default color.
    if (!utils::hasName(cell_connection_map, "color")) {
      cell_connection_map$color <- "#fa5b52"  # Default color for connections
    }

    # Define col_padding based on col_to > col_from
    cell_connection_map$col_padding <- ifelse(
      cell_connection_map$col_to > cell_connection_map$col_from, 0.3, -0.3
    )

    # Draw arrow based on connection_line_type
    connection_line_type <- match.arg(connection_line_type)


    # Here scale_color_identity() uses the values in your color column as actual
    # colors instead of mapping them to a palette.
    g <- g +
      ggplot2::scale_color_identity()


    # Add starting point circles : This is common for both connection types
    g <- g + ggplot2::geom_point(
      data = cell_connection_map,
      mapping = ggplot2::aes(
        x = .data$col_from + .data$col_padding, y = -.data$row_from, color = .data$color),
      inherit.aes = FALSE,
      size = 2,
      alpha = connection_line_alpha
    )


    if (connection_line_type == "Curved") {
      # For curved lines, we can use geom_curve
      cell_connection_map_1 <- cell_connection_map %>%
        dplyr::slice_head(n = max(round(NROW(cell_connection_map)/2),1))
      cell_connection_map_2 <- cell_connection_map %>%
        dplyr::slice(-(1:NROW(cell_connection_map_1)))

      g <- g + ggplot2::geom_curve(
        data = cell_connection_map_1,
        mapping = ggplot2::aes(
          x = .data$col_from + .data$col_padding,
          y = -.data$row_from,
          xend = .data$col_to - .data$col_padding,
          yend = -.data$row_to,
          color = .data$color
        ),
        curvature = -0.3,
        inherit.aes = FALSE,
        linewidth = 1,
        alpha = connection_line_alpha,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "open")
      )

      if(NROW(cell_connection_map_2)>0){
        g <- g + ggplot2::geom_curve(
          data = cell_connection_map_2,
          mapping = ggplot2::aes(
            x = .data$col_from + .data$col_padding,
            y = -.data$row_from,
            xend = .data$col_to - .data$col_padding,
            yend = -.data$row_to,
            color = .data$color
          ),
          curvature = +0.3,
          inherit.aes = FALSE,
          linewidth = 1,
          alpha = connection_line_alpha,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "open")
        )
      }

      # Add curved lines for each connection:
      #
      # This is as curvature is an aesthetic (aes), we need to loop through each
      # row. Kept for reference.
      #
      # for(i in seq_len(nrow(cell_connection_map))) {
      #   row <- cell_connection_map[i, ]
      #   g <- g + ggplot2::geom_curve(
      #     data = row,
      #     mapping = ggplot2::aes(
      #       x = .data$col_from + .data$col_padding,
      #       y = -.data$row_from,
      #       xend = .data$col_to - .data$col_padding,
      #       yend = -.data$row_to,
      #       color = .data$color
      #     ),
      #     curvature = row$col_padding,
      #     inherit.aes = FALSE,
      #     linewidth = 1,
      #     alpha = connection_line_alpha,
      #     arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "open")
      #   )
      # }

    }

    if (connection_line_type == "L-shaped") {

      # Add vertical segments only (with arrow) for vertical_only cases
      if(any(cell_connection_map$vertical_only)){
        g <- g + ggplot2::geom_segment(
          data = cell_connection_map %>% dplyr::filter(.data$vertical_only),
          mapping = ggplot2::aes(
            x = .data$col_from + .data$col_padding, y = -.data$row_from,
            xend = .data$col_from + .data$col_padding, yend = -.data$row_to,
            color = .data$color
          ),
          inherit.aes = FALSE,
          linewidth = 1,
          alpha = connection_line_alpha,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "open")
        )
      }

      # Add vertical and horizontal segments for non-vertical_only cases

      if(any(!cell_connection_map$vertical_only)){
        # Add vertical segments
        g <- g + ggplot2::geom_segment(
          data = cell_connection_map %>% dplyr::filter(!.data$vertical_only),
          mapping = ggplot2::aes(
            x = .data$col_from + .data$col_padding, y = -.data$row_from,
            xend = .data$col_from + .data$col_padding, yend = -.data$row_to,
            color = .data$color
          ),
          inherit.aes = FALSE,
          linewidth = 1,
          alpha = connection_line_alpha
        )

        # Add horizontal segments with arrow heads
        g <- g + ggplot2::geom_segment(
          data = cell_connection_map %>% dplyr::filter(!.data$vertical_only),
          mapping = ggplot2::aes(
            x = .data$col_from + .data$col_padding, y = -.data$row_to,
            xend = .data$col_to - .data$col_padding, yend = -.data$row_to,
            color = .data$color
          ),
          inherit.aes = FALSE,
          linewidth = 1,
          alpha = connection_line_alpha,
          arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "open")
        )
      }

    }

  }


  if (!no_plot) {
    plot(g, ...)
  }

  return(invisible(g))
}





#' Visualize Cell Data with Combined Attribute Information
#'
#' This function creates a visualization of cell data, combining attribute
#' columns and displaying them alongside data blocks.
#'
#' @param x A `cells_analysis`-object
#' @param ... Additional arguments passed to plot.cells
#' @param attr_cols Column names from attr_data_map to combine with ":"
#'   separator
#' @param max_txt_len Maximum length of text displayed in each cell before
#'   truncation. Default is 18.
#' @param txt_size Numeric. Text size for cell labels. Default is 2.5.
#' @param focus_on_data_blocks Character vector. If provided, filters the
#'   analysis to only include specified data blocks (by their `data_gid`).
#'   Default is `NULL`, which means all data blocks are included.
#' @param color_attrs_separately Logical. If `TRUE`, colors attributes
#'   separately based on their hierarchical rank. If `FALSE`, all attributes
#'   gets same color. Default is `FALSE`.
#' @param show_values_in_cells Logical. If `TRUE`, displays the actual values in
#'   cells instead of the cells-analysis info. Default is `FALSE`.
#'
#'
#' @return Invisibly returns the \code{ggplot} object for further customization
#'   or display.
#'
#' @keywords internal
#' @export
plot.cells_analysis <- function(
    x, ...,
    attr_cols = c("data_gid", "nice_header_name", "header_orientation_tag"),
    max_txt_len = 18, txt_size = 2.5,
    focus_on_data_blocks = NULL,
    color_attrs_separately = FALSE,
    show_values_in_cells = FALSE,
    declutter = TRUE) {

  # Step 1: Convert cells analysis data for plotting
  conv_d <-  util_convert_cells_analysis_for_plot(
    x,
    attr_cols = attr_cols,
    focus_on_data_blocks = focus_on_data_blocks,
    color_attrs_separately = color_attrs_separately,
    color_attrs_on_unified_rc = declutter,
    show_values_in_cells = show_values_in_cells)

  # Step 2: Generate the cell plot

  # Adjustment to supplied dot arguments
  dots <- list(...)
  ignored <- c("allow_rc_df", "fill", "max_txt_len",
               "txt_size","scale_fill_values", "declutter_based_on_value")
  dots <- dots[!(names(dots) %in% ignored)]

  # Display the combined data with the plot.cells function
  do.call(
    plot.cells,
    c(list(
      conv_d$combined_data,
      allow_rc_df = TRUE,
      fill = "type",
      max_txt_len = max_txt_len,
      txt_size = txt_size,
      scale_fill_values = conv_d$scale_fill_map,
      declutter_based_on_value = declutter
    ),
    dots
    )
  )
}
