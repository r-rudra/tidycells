

# Here shiny server components for the Shiny modules of this package are
# defined. These functions are used in the main shiny application defined in
# shiny_parts_server.R.
#
# Here sps: shiny parts server


# Dev Note : For brushing to work, always return the ggplot object directly in
# renderPlot. Never wrap a ggplot in plot() inside Shiny.

shiny_sps_part_row_col_selection <- function(
    input, output, session,
    d_cells, ca_react = NULL, plot_config) {

  if(!is.null(ca_react)) {
    # Note if ca_react is not NULL then plot_config (plot_handle) is required

    # If cells analysis is provided, use it to derive the cells data

    # Derive the current data from the cells analysis
    d_cells <- shiny::reactiveVal(
      tibble::tibble(row = integer(),
                     col = integer(),
                     value = character(),
                     data_type = character()))
    shiny::observe({
      ph <- plot_config()
      # If CA mode, use the cells analysis data to retrieve the induced cells
      dh <- util_convert_cells_analysis_for_plot(
        ca_react(),
        focus_on_data_blocks = ph$plot_params$focus_on_data_blocks,
        color_attrs_separately = ph$plot_params$color_attrs_separately,
        show_values_in_cells = ph$plot_params$show_values_in_cells)
      d_cells(dh$combined_data)
    })
  }

  # Filtered cells (based on row and column selection)
  cells_for_plot <- shiny::reactive({

    shiny::req(input$selected_rows, input$selected_cols)

    d0 <- d_cells()

    d0 %>%
      dplyr::filter(
        .data$row >= input$selected_rows[1] &
          .data$row <= input$selected_rows[2] &
          .data$col >= input$selected_cols[1] &
          .data$col <= input$selected_cols[2])
  })

  # Checking if plot is OK (row and column selection is within limits)
  cells_plot_ok <- shiny::reactive({
    shiny::req(input$selected_rows, input$selected_cols)
    (input$selected_cols[2] - input$selected_cols[1]) <= 30 &&
      (input$selected_rows[2] - input$selected_rows[1]) <= 100 &&
      NROW(cells_for_plot()) > 0
  })

  # Action of SPS : adjusting the row and column selection sliders if they
  # exceed the limits
  shiny::observe({
    shiny::req(input$selected_rows, input$selected_cols)

    if ((input$selected_rows[2] - input$selected_rows[1]) > 100) {
      shiny::updateSliderInput(
        session,
        "selected_rows",
        value = c(input$selected_rows[1], input$selected_rows[1] + 100 - 1)
      )
      shiny::showNotification(
        "Maximum 100 rows can be selected at a time.",
        type = "warning",
        duration  = 1
      )
    }

    if ((input$selected_cols[2] - input$selected_cols[1]) > 30) {
      shiny::updateSliderInput(
        session,
        "selected_cols",
        value = c(input$selected_cols[1], input$selected_cols[1] + 30 - 1)
      )
      shiny::showNotification(
        "Maximum 30 columns can be selected at a time.",
        type = "warning",
        duration  = 1
      )
    }
  })


  # Update mini plot based on row and column selection. It plots all cells but
  # with a boundary defined by the selected rows and columns.
  output$plot_rc_select <- shiny::renderPlot({

    if (cells_plot_ok()) {
      plot.cells(
        d_cells(),
        fill = "data_type",
        fill_alpha = 1,
        no_fill = TRUE,
        no_txt = TRUE,
        no_legend = TRUE,
        boundaries = data.frame(
          r_min = input$selected_rows[1], r_max = input$selected_rows[2],
          c_min = input$selected_cols[1], c_max = input$selected_cols[2]
        ),
        # For Speed
        ignore_validation =  TRUE
      )
    }
  }, bg = "transparent")

  # Return as list
  return(list(
    cells_for_plot = cells_for_plot,
    cells_plot_ok = cells_plot_ok
  ))
}

shiny_sps_part_plot_tune <- function(
    input, output, session,
    d_now, ca_now = NULL) {


  # The main plot handle for the plot tuning to be returned
  plot_handle <- shiny::reactive({

    shiny::req(input$fill)

    lst_info <- list()

    # Capture all plot parameters too
    lst_info$plot_params <- list(
      fill = input$fill,
      fill_alpha = input$fill_alpha,
      txt_size = input$txt_size,
      txt_alpha = input$txt_alpha,
      no_txt = input$no_txt,
      max_txt_len = input$max_txt_len,
      add_cell_address = input$add_cell_address,
      no_legend = input$no_legend,
      auto_round_values = input$auto_round_values,
      color_attrs_separately = input$color_attrs_separately,
      show_values_in_cells = input$show_values_in_cells
    )

    lst_info
  })


  # Row and column selection for the plot tuning
  rc_sel <- shiny_sps_part_row_col_selection(
    input, output, session,
    d_cells = d_now, ca_react = ca_now, plot_config = plot_handle)

  # Render the plot based on the selected parameters
  output$plot_tune <- shiny::renderPlot({
    shiny::req(input$fill_alpha)
    # Check the size of the selection and then plot the cells
    if(rc_sel$cells_plot_ok()) {
      # If row and column selection is within limits, plot the cells

      ph <- plot_handle()
      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel,
        ca_now = ca_now
      )
    }
  }, execOnResize = TRUE)


  # Return the plot handle
  return(plot_handle)
}

shiny_sps_part_crop  <- function(
    input, output, session,
    d_now, d_orig, plot_handle) {

  # The main reactiveVal to store current data
  current <- shiny::reactiveVal()
  shiny::observe({
    # Initialize current data with the present
    current(d_now())
  })

  # Row and column selection for the plot tuning
  rc_sel <- shiny_sps_part_row_col_selection(
    input, output, session,
    d_cells = current)

  # Get selected points based on brush input
  selected_points <- shiny::reactive({
    # Dev Note: This can not be done here shiny::req(input$brush_crop)
    shiny::brushedPoints(
      rc_sel$cells_for_plot() %>%
        dplyr::mutate(x = .data$col, y = -.data$row),
      input$brush_crop,
      xvar = "x", yvar = "y"
    ) %>%
      # Remove the temporary columns used for brushing
      dplyr::select(c("row", "col"))

  })

  # Update the cells plot
  output$plot_crop <- shiny::renderPlot({
    # If row and column selection is within limits, plot the cells with selected
    # points highlighted
    if(rc_sel$cells_plot_ok()) {
      ph <- plot_handle()
      # Base plot for selected cells (based on row/col selection)
      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel
      ) +
        # Section for highlighting the selected points
        ggplot2::geom_tile(
          mapping = ggplot2::aes(.data$col, -.data$row),
          # If no points are selected, it will return an empty data frame and
          # the plot will work
          data = selected_points(),
          color = "#F07973", fill = "#949684A8", inherit.aes = FALSE,
          alpha = 0.5, na.rm = TRUE,
          width = 1, height = 1
        )
    }
  })

  ### Action to crop the data based on selected points ----

  # Delete Option
  shiny::observeEvent(input$data_del, {
    pts <- selected_points()
    if (NROW(pts) > 0) {
      # Update the current data by removing the selected points
      current(current() %>% dplyr::anti_join(pts, by = c("row", "col")))
      # Reset the brush
      session$resetBrush(input$brush_crop$brushId)
    }
  })

  # Crop Option
  shiny::observeEvent(input$data_crop, {
    pts <- selected_points()
    if (NROW(pts) > 0) {
      # Update the current data by keeping only the selected points
      current(
        current() %>%
          dplyr::inner_join(pts[c("row","col")], by = c("row", "col")))
      # Reset the brush
      session$resetBrush(input$brush_crop$brushId)
    }
  })

  # Reset Option
  shiny::observeEvent(input$data_reset, {
    current(d_orig())
  })

  # Return the current data
  return(current)
}

shiny_sps_part_va_classify <- function(
    input, output, session,
    d_now, d_orig, plot_handle) {


  # The main reactiveVal to store current data
  current <- shiny::reactiveVal()
  shiny::observe({
    # Initialize current data with the working copy of cells data
    current(d_now())
  })

  # Row and column selection for the plot tuning
  rc_sel <- shiny_sps_part_row_col_selection(
    input, output, session,
    d_cells = current)

  # Get selected points based on brush input
  selected_points <- shiny::reactive({
    shiny::brushedPoints(
      rc_sel$cells_for_plot() %>%
        dplyr::mutate(x = .data$col, y = -.data$row),
      input$brush_va_classify,
      xvar = "x", yvar = "y"
    ) %>%
      # Remove the temporary columns used for brushing
      dplyr::select(c("row", "col"))

  })

  # Plot the cells with selected points highlighted
  output$plot_va_classify <- shiny::renderPlot({
    # If row and column selection is within limits, plot the cells with selected
    # points highlighted
    if(rc_sel$cells_plot_ok()) {
      ph <- plot_handle()
      # Set the fill parameter for the plot to PoA (Probability of Attribute)
      ph$plot_params$fill <- "PoA"
      # Base plot for selected cells (based on row/col selection)
      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel
      ) +
        # Section for highlighting the selected points
        ggplot2::geom_tile(
          mapping = ggplot2::aes(.data$col, -.data$row),
          data = selected_points(),
          color = "#F07973", fill = "#949684A8", inherit.aes = FALSE,
          alpha = 0.5, na.rm = TRUE,
          width = 1, height = 1
        )
    }
  }, execOnResize = TRUE)


  # Action to classify the data based on selected points

  # Since structure of action in both value and attribute classification is
  # similar here we take help of additional variable (reactive).

  VA_probs <- shiny::reactiveValues(
    VA_trigger = FALSE, # Default initial value - "Trigger of VA classification"
    PoV = 0, # Default initial value for PoV
    PoA = 0  # Default initial value for PoA
  )

  # Value Option
  shiny::observeEvent(input$make_value_va_classify, {
    VA_probs$VA_trigger <- TRUE # Trigger the VA classification
    VA_probs$PoA <- 0 # Sets probability of attribute to 0
    VA_probs$PoV <- 1 # Sets probability of value to 1
  })

  # Attribute Option
  shiny::observeEvent(input$make_attr_va_classify, {
    VA_probs$VA_trigger <- TRUE # Trigger the VA classification
    VA_probs$PoA <- 1 # Sets probability of attribute to 1
    VA_probs$PoV <- 0 # Sets probability of value to 0
  })

  # VA Action through VA_probs

  shiny::observeEvent(VA_probs$VA_trigger, {
    pts <- selected_points()
    if (NROW(pts) > 0) {
      cells_now <- current()

      # PoA and PoV columns are added to the current data for value cells
      pts$PoV <- VA_probs$PoV
      pts$PoA <- VA_probs$PoA

      # Update PoA and PoV columns in the current data based on the selected
      # points
      cells_updated  <- cells_now %>% dplyr::left_join(
        pts, by = c("row", "col"), suffix = c("", "_new")
      ) %>%
        dplyr::mutate(
          # Update the PoV and PoA column with the new value from the brush
          # value = ifelse(is.na(.data$value_new), .data$value, .data$value_new)
          PoV = ifelse(is.na(.data$PoV_new), .data$PoV, .data$PoV_new),
          PoA = ifelse(is.na(.data$PoA_new), .data$PoA, .data$PoA_new)
        ) %>%
        dplyr::select(-c("PoV_new", "PoA_new"))

      # Update the current data by removing the selected points
      current(cells_updated)
      # Reset the brush
      session$resetBrush(input$brush_crop$brushId)
      # Reset the trigger on action completion
      VA_probs$VA_trigger <- FALSE
    } else {
      # Reset the trigger if no points are selected
      VA_probs$VA_trigger <- FALSE
    }
  })

  # Reset Option
  shiny::observeEvent(input$reset_va_classify, {
    VA_probs$VA_trigger <- FALSE
    current(d_orig())
  })


  return(current)
}

shiny_sps_part_orientation_modification <- function(
    input, output, session,
    ca_now, ca_orig, plot_handle) {
  # NS is required as dynamic UI is used
  ns <- session$ns  # Get the namespace function

  # The main reactiveVal to store current data
  current <- shiny::reactiveVal()
  current_rcdf <- shiny::reactiveVal()
  # The working copy of plot_handle
  current_plot_handle <- shiny::reactiveVal()

  # For data_block focus
  current_focus_on_data_blocks <- shiny::reactiveVal(NULL)
  # Variable to store current state
  selection_1 <- shiny::reactiveVal(NULL)
  selection_2 <- shiny::reactiveVal(NULL)

  click_count <- shiny::reactiveVal(0)

  action_to_perform <- shiny::reactiveVal(NULL)

  # Initialize these state variables
  shiny::observe({
    # Initialize current data with the original data
    current(ca_now())
  })

  shiny::observe({
    # This to depend on changes in current
    current()
    # Generate the initial reactive cells induced by cells_analysis
    ph <- plot_handle()
    ph$plot_params$focus_on_data_blocks <- current_focus_on_data_blocks()

    # Update the current plot handle for focus
    current_plot_handle(ph)

    conv_d <- util_convert_cells_analysis_for_plot(
      current(),
      focus_on_data_blocks = current_focus_on_data_blocks(),
      color_attrs_separately = ph$plot_params$color_attrs_separately,
      # Note intentionally not using `ph$plot_params$show_values_in_cells`. As
      # values in cells only for display but cell info is crucial for other
      # operations.
      show_values_in_cells = FALSE)
    current_rcdf(conv_d$combined_data)
  })


  # Row and column selection for the plot tuning
  rc_sel <- shiny_sps_part_row_col_selection(
    input, output, session,
    ca_react = current, plot_config = current_plot_handle)


  # Render the plot based on the selected parameters
  output$plot_omod <- shiny::renderPlot({
    # Check the size of the selection and then plot the cells
    if(rc_sel$cells_plot_ok()) {
      # If row and column selection is within limits, plot the cells

      ph <- plot_handle()

      # declutter mode only if value is not shown
      declutter_possibility <- FALSE
      declutter_possibility_check <- rc_sel$cells_for_plot()$value |>
        setdiff(util_convert_cells_analysis_for_plot(ca_now())$combined_data$value)
      if(length(declutter_possibility_check)==0) declutter_possibility <- TRUE

      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel,
        ca_now = current,
        declutter_ca = isTRUE(input$declutter_ca_plot) && declutter_possibility) +
        # Section for highlighting the selected points (selection 1 and 2)
        ggplot2::geom_tile(
          mapping = ggplot2::aes(.data$col, -.data$row),
          # If no points are selected, it will return an empty data frame and
          # the plot will work
          data = shiny_util_get_full_block_from_cells_analysis(
            selection = selection_1()$which,
            ca_cells = rc_sel$cells_for_plot()
          ),
          color = "#F07973", fill = "#94F484", inherit.aes = FALSE,
          alpha = 0.2, na.rm = TRUE, lwd = 0.7, lty = 5,
          width = 1, height = 1) +
        # Section for highlighting the selected points (selection 1 and 2)
        ggplot2::geom_tile(
          mapping = ggplot2::aes(.data$col, -.data$row),
          # If no points are selected, it will return an empty data frame and
          # the plot will work
          data = shiny_util_get_full_block_from_cells_analysis(
            selection = selection_2()$which,
            ca_cells = rc_sel$cells_for_plot()
          ),
          color = "#F073D9", fill = "#848F06", inherit.aes = FALSE,
          alpha = 0.2, na.rm = TRUE, lwd = 0.7, lty = 4,
          width = 1, height = 1
        )
    }
  }, execOnResize = TRUE)


  # Update Selection 1 and Selection 2 based on user input
  shiny::observeEvent(input$click_omod, {

    click_count(click_count()+1)

    # Capture the clicked cell
    d_sel <- shiny_util_clicked_tile(current_rcdf(), input$click_omod)

    # Selection 1: Click on the data block or attribute block
    if(NROW(d_sel) > 0 && is.null(selection_1())) {

      # Either data block or attribute block is selected

      if(d_sel$type[1] == "data") {
        # Data block selected
        selection_1(list(type="data", which = d_sel))

        # Change the focus on data blocks to selected data block
        if(input$dynamic_focus){
          current_focus_on_data_blocks(d_sel$value)
        }
      } else {
        # Attribute block selected
        selection_1(list(type="attr", which = d_sel))

        # Shift the focus on data blocks to the attribute block only if
        # Navigation-Only mode enabled.
        if (input$dynamic_focus) {
          # Retrieve the data blocks connected to this attribute block
          ad_map_attr_wise <- current()$attr_data_map
          connected_data_blocks <- ad_map_attr_wise %>%
            dplyr::inner_join(d_sel[c("row", "col")],
                              by = c("row", "col"))
          connected_data_blocks <- unique(connected_data_blocks$data_gid)

          # Change the focus on data blocks to the connected data blocks
          current_focus_on_data_blocks(connected_data_blocks)
        }

      }
    } else if(NROW(d_sel) == 0) {
      # No selection made, reset the selections
      current_focus_on_data_blocks(NULL)
      selection_1(NULL)
      selection_2(NULL)
      click_count(0)
    }

    # Selection 2: Click or Brush on the data block or attribute block after
    # selection 1

    # Proceed only if selection_1 is made and selection_2 is not set (tracked
    # via click_count)
    if(!is.null(selection_1()) && click_count()>1 && NROW(d_sel) > 0){

      # Check if clicked cell is a data block or attribute block
      if(d_sel$type[1] == "data") {
        # Data block selected
        selection_2(list(type="data", which = d_sel))
      } else {
        # Attribute block selected
        selection_2(list(type="attr", which = d_sel))
      }
    }

  })


  # Reset Option
  shiny::observeEvent(input$reset_omod, {
    current(ca_orig())
  })


  # Dynamic UI Section ----

  output$ui_omod_control <- shiny::renderUI({
    # Start Case when no click detected/ Selection 1 is not made
    if(is.null(selection_1())) {
      return(shiny::tagList(
        shiny::h5(
          "Click on the cells to select data-block/attribute-block to proceed."
        ),
        shiny::h6(
          ifelse(
            !input$dynamic_focus,
            paste0(
              "Tip: You can turn on Dynamic Focus to automatically focus on ",
              "the first selected data-block, or on the data-block(s) ",
              "connected to the first selected attribute-block. This is useful ",
              "when performing actions on a single data-block, such as ",
              "detaching an attribute or changing the header orientation tag."
            ),
            paste0(
              "Tip: You can turn off Dynamic Focus to allow selection of ",
              "multiple data-blocks or attribute-blocks without automatically ",
              "focusing on the selected block."
            )
          )
        )
      ))
    } else if(is.null(selection_2())) {
      # Clicked Case / Selection 1 is made
      return(shiny::tagList(
        shiny::h5(
          paste0(
            tools::toTitleCase(selection_1()$type), "-Block Selected:",
            selection_1()$which$value[1]
          )
        ),
        shiny::div(style = "height:1px;"),
        shiny::h5(
          paste0(
            "\nNow click on another cells to select ",
            "second data-block/attribute-block to proceed."
          )
        )
      ))
    } else {

      common_top <- shiny::tagList(
        shiny::h6(
          "First ",
          shiny::strong(tools::toTitleCase(selection_1()$type)),
          "-Block Selected:",
          shiny::strong(selection_1()$which$value[1]), " Then "

        ),
        shiny::div(style = "margin-top: 1px;"),
        shiny::h6(
          shiny::strong(tools::toTitleCase(selection_2()$type)),
          "-Block Selected:",
          shiny::strong(selection_2()$which$value[1])
        )
      )

      actions_available <- infer_util_cells_analysis_modification(
        current(),
        point_1 = selection_1(),
        point_2 = selection_2(),
        return_actions = TRUE)


      # For "detach" & "change_HOT" situations
      if("detach" %in% actions_available) {
        return(shiny::tagList(
          common_top,
          shiny::h5(
            "These actions can be done:"
          ),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::actionButton(
                ns("omod_detach"),
                "Unlink/Detach",
                title = paste0(
                  "Unlink/Detach ", selection_1()$type, ":- ", selection_1()$which$value[1],
                  " from ", selection_2()$type, ":- ", selection_2()$which$value[1]
                ),
                icon = shiny::icon("unlink")
              )
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(
                ns("omod_change_HOT"),
                "Modify - HOT",
                title = "Modify - Header Orientation Tag",
                icon = shiny::icon("edit")
              )
            )
          ),

          shiny::selectInput(
            ns("omod_HOT_to"),
            "Choose Header Orientation Tag (HOT)",
            choices = unique(unlist(infer_get_valid_header_orientation_tags()))
          )
        ))
      }

      # For "join" case
      if("join" %in% actions_available) {
        return(shiny::tagList(
          common_top,
          shiny::h5(
            "This action can be done:"
          ),
          shiny::actionButton(
            ns("omod_join"),
            "Join Them",
            title = paste0(
              "Join ", selection_1()$type, ":- ", selection_1()$which$value[1],
              " and ", selection_2()$type, ":- ", selection_2()$which$value[1]
            ),
            icon = shiny::icon("link")
          ),
          shiny::div(style = "height:3px;")
        ))
      }

      # For "connect" case
      if("connect" %in% actions_available) {
        return(shiny::tagList(
          common_top,
          shiny::h5(
            "This action can be done:"
          ),
          shiny::actionButton(
            ns("omod_connect"),
            "Connect Them",
            title = paste0(
              "Connect ", selection_1()$type, ":- ", selection_1()$which$value[1],
              " and ", selection_2()$type, ":- ", selection_2()$which$value[1]
            ),
            icon = shiny::icon("plug")
          ),
          shiny::selectInput(
            ns("omod_HOT_to"),
            "Choose Header Orientation Tag (HOT)",
            choices = unique(unlist(infer_get_valid_header_orientation_tags()))
          )
        ))
      }

      # Conflict Cases
      if(any(stringr::str_detect(actions_available,"conflict"))) {
        if ("conflict_multiple_selection" %in% actions_available) {
          return(shiny::tagList(
            common_top,
            shiny::h6(
              paste0(
                "No action can be performed because multiple selections were detected in a single cell.",
                " Please select only one data-block first, then select another data-block or attribute-block.",
                " If cells of this type are present, they have to first be detached from multiple connected data-blocks for further actions."
              )
            )
          ))
        }


        if ("conflict_same_selection" %in% actions_available) {
          return(shiny::tagList(
            common_top,
            shiny::h6(
              paste0(
                "No action can be performed because the same block was selected twice.",
                " Please select two different blocks to proceed."
              )
            )
          ))
        }

      }


    }


    # Default case return empty UI
    return(shiny::tagList())

  })


  # Dynamic UI-based Actions ----

  # Put all action to perform in variable "action_to_perform" and then from
  # there update current()
  shiny::observeEvent(input$omod_detach,{
    action_to_perform(
      list(action = "detach")
    )
  })

  shiny::observeEvent(input$omod_change_HOT,{
    action_to_perform(
      list(action = "change_HOT",
           HOT_to = input$omod_HOT_to)
    )
  })

  shiny::observeEvent(input$omod_join,{
    action_to_perform(
      list(action = "join")
    )
  })

  shiny::observeEvent(input$omod_connect,{
    action_to_perform(
      list(action = "connect",
           HOT_to = input$omod_HOT_to)
    )
  })

  # Finally perform it
  shiny::observeEvent(action_to_perform(),{
    if(!is.null(selection_1()) &&
       !is.null(selection_2()) &&
       !is.null(action_to_perform()$action)){

      ca_mod <- infer_util_cells_analysis_modification(
        ca = current(),
        point_1 = selection_1(), point_2 = selection_2(),
        return_actions = FALSE,
        do_action = action_to_perform()$action,
        HOT_to = action_to_perform()$HOT_to)

      current(ca_mod)

      # If no other blocks are connected, the entire block will be removed from
      # the plot. In such cases, the code will identify which block is affected
      # and reset the selection accordingly.
      conv_d <- util_convert_cells_analysis_for_plot(ca_mod)
      cells_now <- conv_d$combined_data

      chk1 <- selection_1()$which[c("row","col")] %>%
        dplyr::inner_join(cells_now, by = c("row","col"))

      if (NROW(chk1) == 0) selection_1(NULL)

      chk2 <- selection_2()$which[c("row","col")] %>%
        dplyr::inner_join(cells_now, by = c("row","col"))

      if (NROW(chk2) == 0) selection_2(NULL)

      # Reset the action_to_perform
      action_to_perform(NULL)

    }
  })


  # Return
  return(current)
}




shiny_sps_part_traceback <- function(
    input, output, session,
    ca_now, plot_handle) {
  # NS is required for DT
  ns <- session$ns  # Get the namespace function

  # Variable to store present traceback render
  traceback_render_now <- shiny::reactiveVal(NULL)
  data_for_DT <- shiny::reactiveVal(tibble::tibble())

  # The main reactiveVal to store current whole data (without data-block focus)
  whole_rcdf <- shiny::reactiveVal()

  # The main reactiveVal to store current data
  current_rcdf <- shiny::reactiveVal()
  # The working copy of plot_handle
  current_plot_handle <- shiny::reactiveVal()

  # For data_block focus
  current_focus_on_data_blocks <- shiny::reactiveVal(NULL)
  # Variable to store current state
  selection_from_plot <- shiny::reactiveVal(NULL)
  selection_from_DT <- shiny::reactiveVal(NULL)
  # Variable to fuse combined selection
  selection_combined <- shiny::reactiveVal(NULL)

  # DT Color and Arrow map for traceback
  DT_traceback_meta <- shiny::reactiveVal(
    tibble::tibble())

  return_handle <- shiny::reactiveVal(NULL)



  # Initialize these state variables

  shiny::observe({
    # Generate the initial reactive cells induced by cells_analysis
    ph <- plot_handle()
    ph$plot_params$focus_on_data_blocks <- current_focus_on_data_blocks()

    # Update the current plot handle for focus
    current_plot_handle(ph)

    conv_d <- util_convert_cells_analysis_for_plot(
      ca_now(),
      focus_on_data_blocks = current_focus_on_data_blocks(),
      color_attrs_separately = ph$plot_params$color_attrs_separately,
      # Note intentionally not using `ph$plot_params$show_values_in_cells`. As
      # values in cells only for display but cell info is crucial for other
      # operations.
      show_values_in_cells = FALSE)
    current_rcdf(conv_d$combined_data)

    # Take all data_blocks for furtehr use
    conv_d_all_data_blocks <- util_convert_cells_analysis_for_plot(
      ca_now(),
      # Since it is mainly required for data_block identification only data_gid
      # is taken
      attr_cols = "data_gid",
      show_values_in_cells = FALSE)

    whole_rcdf(conv_d_all_data_blocks$combined_data)
  })

  # Initiate and Link traceback_render_now to ca_now to trigger re-render on
  # ca_now change
  shiny::observeEvent(ca_now(), {
    tr_rndr <- util_get_traceback_composition(ca_now())
    traceback_render_now(tr_rndr)
  })

  # Prepare data_for_DT based on the selected comp_type
  shiny::observe({
    shiny::req(traceback_render_now())

    collated_options <- c(
      "Collated Columns","Info Columns",
      "Uncollated Columns","Cell Address")

    composed_options <- c(
      "High Priority Columns","Mid Priority Columns",
      "Low Priority Columns","Cell Address")

    ok_state <- FALSE

    if(input$comp_type == "collated" &&
       any(input$show_cols %in% collated_options[-4])) {
      dt0 <- traceback_render_now()$comp_orig_collate
      chk <- intersect(input$show_cols_fine_sel, setdiff(colnames(dt0), c("col","row","data_gid","value")))
      if(length(chk)==0){
        shiny::updateSelectizeInput(
          inputId = "show_cols_fine_sel",
          label = shiny::span(
            "Columns Filter (Further Selection)",
            title = shiny_util_msg()$fine_col_sel),
          choices = colnames(dt0),
          selected =colnames(dt0)
        )
      }
      ok_state <- TRUE
    }

    if(input$comp_type != "collated" &&
       any(input$show_cols %in% composed_options[-4])) {

      dt0 <- traceback_render_now()$comp_orig
      chk <- intersect(input$show_cols_fine_sel, setdiff(colnames(dt0), c("col","row","data_gid","value")))
      if(length(chk)==0){
        shiny::updateSelectizeInput(
          inputId = "show_cols_fine_sel",
          label = shiny::span(
            "Columns Filter (Further Selection)",
            title = shiny_util_msg()$fine_col_sel),
          choices = colnames(dt0),
          selected =colnames(dt0)
        )
      }

      ok_state <- TRUE
    }

    if(ok_state) {

      if(input$comp_type == "collated") {
        # Use output of collate_column
        d_this <- traceback_render_now()$comp_orig_collate

        if(input$discard_const_cols){
          cols_to_keep <- c("col", "row", "data_gid", "value")
          d_this <- d_this %>%
            dplyr::select(
              dplyr::any_of(cols_to_keep),
              dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 1)
            )
        }

        cnames_this <- colnames(d_this)

        if(!("Uncollated Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^uncollated_")])
        }

        if(!("Info Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^info_")])
        }

        cn_try <- intersect(input$show_cols_fine_sel, cnames_this)
        if(length(setdiff(cn_try,c("value","row","col","data_gid")))>0){
          cnames_this <- union(cn_try, c("value","row","col","data_gid"))
        }

        d_this <- d_this[cnames_this]

      } else {
        # Use output of compose
        d_this <- traceback_render_now()$comp_orig

        if(input$discard_const_cols){
          cols_to_keep <- c("col", "row", "data_gid", "value")
          d_this <- d_this %>%
            dplyr::select(
              dplyr::any_of(cols_to_keep),
              dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) > 1)
            )
        }

        cnames_this <- colnames(d_this)

        if(!("Mid Priority Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^mid_")])
        }

        if(!("Low Priority Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^low_")])
        }

        cn_try <- intersect(input$show_cols_fine_sel, cnames_this)
        if(length(setdiff(cn_try,c("value","row","col","data_gid")))>0){
          cnames_this <- union(cn_try, c("value","row","col","data_gid"))
        }

        d_this <- d_this[cnames_this]
      }

      # Add row_id for DT
      d_this <- dplyr::mutate(d_this, row_id = seq_len(dplyr::n()))

      # Reorder columns
      d_this <- d_this[c(
        # Safe option is this but d_thiswill always have these columns:
        # intersect(colnames(d_this),c("data_gid","row","col")),
        c("data_gid","row","col"),
        setdiff(colnames(d_this),c("data_gid","row","col","value","row_id")),
        "value",
        "row_id"
      )]

      # Update the data_for_DT to be used in DT section
      data_for_DT(d_this)
    }

  })

  # Dev Note : Note that input$show_cols is used in different mode - This means
  # for some values of input$show_cols it is directly removed before DT and some
  # are hidden at DT

  # Update the check-box group based on the selected comparison type
  shiny::observeEvent(input$comp_type,{

    if(input$comp_type == "collated") {

      collated_options <- c(
        "Collated Columns","Info Columns",
        "Uncollated Columns","Cell Address")

      # If none of the collated options are selected, update the checkbox group
      if(!any(input$show_cols %in% collated_options[-4])){
        shiny::updateCheckboxGroupInput(
          inputId = "show_cols",
          label = "Show Columns",
          choices = collated_options,
          selected = collated_options[1]
        )
      }

    } else {

      composed_options <- c(
        "High Priority Columns","Mid Priority Columns",
        "Low Priority Columns","Cell Address")

      # If none of the composed options are selected, update the checkbox group
      if(!any(input$show_cols %in% composed_options[-4])){
        shiny::updateCheckboxGroupInput(
          inputId = "show_cols",
          label = "Show Columns",
          choices = composed_options,
          selected = composed_options[c(1,2)]
        )
      }
    }

  })


  # Row and column selection for the plot tuning
  rc_sel <- shiny_sps_part_row_col_selection(
    input, output, session,
    ca_react = ca_now, plot_config = current_plot_handle)


  # Render the plot based on the selected parameters
  output$plot_traceback <- shiny::renderPlot({
    # Check the size of the selection and then plot the cells
    if(rc_sel$cells_plot_ok()) {
      # If row and column selection is within limits, plot the cells

      # Color and arrow mapping
      arrow_and_color_map <- DT_traceback_meta()
      if(NROW(arrow_and_color_map)>0){
        sel_pt <- arrow_and_color_map %>% dplyr::slice(1)
      } else {
        sel_pt <- tibble::tibble(row_from = numeric(0), col_from = numeric(0))
      }

      ph <- plot_handle()

      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel,
        ca_now = ca_now,
        declutter_ca = FALSE,
        # These arguments are for traceback
        cell_connection_map = arrow_and_color_map,
        connection_line_type = input$connection_line_type,
        connection_line_alpha = input$connection_line_alpha,
        connection_line_add_jitter = input$connection_line_add_jitter
      ) +
        # Section for highlighting the selected point
        ggplot2::geom_tile(
          mapping = ggplot2::aes(.data$col_from, -.data$row_from),
          data = sel_pt,
          color = "#F07973", fill = "#94F484", inherit.aes = FALSE,
          alpha = 0.2, na.rm = TRUE, lwd = 0.7, lty = 5,
          width = 1, height = 1)
    }
  }, execOnResize = TRUE)


  output$dt_trace <- DT::renderDT({
    shiny::req(data_for_DT())

    # Color mapping
    col_name_to_color_map <- DT_traceback_meta()

    if(NROW(col_name_to_color_map)>0){
      # If value column is missing, add it
      if(!("value" %in% col_name_to_color_map$colname)){
        col_name_to_color_map <- col_name_to_color_map %>%
          dplyr::bind_rows(
            data.frame(colname = "value", color = "black")
          )
      }
    }

    # Specify columns to hide
    hide_these_cols <- character()
    if(!("Cell Address" %in% input$show_cols)){
      hide_these_cols <- c("data_gid","row","col")
    }

    # Check if any columns present apart from display requested columns
    cnames_this <- colnames(data_for_DT())
    rem_cols <- setdiff(cnames_this, c(hide_these_cols,"row_id"))
    if(length(rem_cols)<3){
      shiny::showNotification(
        paste0(
          "Too few columns are visible! ",
          "Consider enabling more column types in the controls."),
        duration = 3)
    }

    # Render the DT table with the selected row and column highlighted
    out_DT <- shiny_util_DT(
      # The final data to display in DT as linked with data_for_DT
      data = data_for_DT(),
      target_row = selection_combined()$row[1],
      target_col = selection_combined()$col[1],
      col_colors = col_name_to_color_map,
      hide_these_cols = hide_these_cols,
      DT_animate_scroll = isTRUE(input$animate_DT_scroll)
    )

    out_DT

  }, server = TRUE)



  # Update Selections base on plot or DT ----

  # Section from plot
  shiny::observeEvent(input$click_traceback, {

    # Capture the clicked cell
    d_sel <- shiny_util_clicked_tile(current_rcdf(), input$click_traceback)

    # Selection from Plot: Click on the data block in the plot
    if(NROW(d_sel) > 0) {

      # Consider the case of only data block selection

      if(d_sel$type[1] == "data") {
        # Data block selected
        selection_from_plot(d_sel)
        selection_from_DT(NULL) # Reset selection from DT
      }
      # Note here attribute block selection is not considered as traceback
      # focuses on data blocks only
    } else if(NROW(d_sel) == 0) {
      # No selection made, reset the selections
      selection_from_plot(NULL)
      selection_combined(NULL)
      current_focus_on_data_blocks(NULL)
      proxy <- DT::dataTableProxy("dt_trace")
      # Deselect all rows
      DT::selectRows(proxy, NULL)
      selection_from_DT(NULL)
    }

  })


  # Selection from DT
  shiny::observeEvent(input$dt_trace_rows_selected, {

    d_this <- traceback_render_now()$comp_orig
    # Update the selection_from_DT based on selected rows in DT
    selection_from_DT(
      d_this[input$dt_trace_rows_selected,c("row","col")]
    )
    selection_from_plot(NULL) # Reset selection from plot
  })

  # Update selection_combined based on selection from plot or DT
  shiny::observe({
    if(!is.null(selection_from_plot())){
      # Update from plot
      selection_combined(
        selection_from_plot()[1,c("row","col")]
      )
    }

    if(!is.null(selection_from_DT())){
      # Update from DT
      selection_combined(
        selection_from_DT()[1,c("row","col")]
      )
    }

  })

  # Action based on selection (selection_combined) ----

  shiny::observe({

    if(!is.null(selection_combined())){

      # Update the focus on data blocks to selected data block
      if(input$dynamic_focus_traceback){
        d_sel <- selection_combined()
        d_sel <- d_sel %>%
          dplyr::inner_join(
            whole_rcdf() %>%
              dplyr::select(c("row","col","value","type")),
            by = c("row","col")
          )
        if(NROW(d_sel) > 0 && d_sel$type[1] == "data") {
          current_focus_on_data_blocks(d_sel$value[1])
        }
      } else {
        current_focus_on_data_blocks(NULL)
      }

      # Traceback operation - meta update
      dtr <- util_traceback(
        cell_row = selection_combined()$row[1],
        cell_col = selection_combined()$col[1],
        traceback_render = traceback_render_now(),
        do_collated = (input$comp_type=="collated"))

      if(input$comp_type=="collated") {
        dtr_this <- dtr$connected_collated_cells
      } else {
        dtr_this <- dtr$connected_cells
      }

      dtr_this <- stats::na.omit(dtr_this)

      if(input$show_displayed_cols_only){
        cnames_now <- colnames(data_for_DT())
        dtr_this <- dtr_this %>%
          dplyr::filter(.data$colname %in% cnames_now)
      }


      if(NROW(dtr_this)>0){

        # 1. Define your base colors
        base_cols <- c(
          "#FF0000", "#FF7F00", "#007FFF", "#E84C20",
          "#008B5A", "#0000FF", "#A1330F", "#8B00FF"
        )

        # 2. Generate the palette function
        pal <- shiny_util_create_separated_palette(
          colors = base_cols, seed = 1, return_palette = TRUE)

        dtr_this$color <- pal(NROW(dtr_this))

        dtr_this <- dtr_this %>%
          dplyr::mutate(
            row_from = selection_combined()$row[1],
            col_from = selection_combined()$col[1],
            row_to = .data$row,
            col_to = .data$col)

        dtr_this <- dtr_this %>%
          dplyr::select(c("colname", "row_from", "col_from",
                          "row_to", "col_to", "color"))




        DT_traceback_meta(dtr_this)

      }

    } else {
      DT_traceback_meta(NULL)
    }
  })



  shiny::observe({
    if(input$comp_type=="collated"){
      return_handle(traceback_render_now()$comp_orig_collate)
    } else {
      return_handle(traceback_render_now()$comp_orig)
    }
  })

  # Return
  return(return_handle)


}
