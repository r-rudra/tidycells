
# This function ensures brush can be added while retaining drag-ability


shiny_util_ui_absolute_panel <- function(
    ...,
    ns_id,
    such_panel_seq = 1,
    btn_id  = paste0("toggle_abs_panel", such_panel_seq),
    content_id = paste0("collapsible_content", such_panel_seq),
    top = NULL,
    left = NULL,
    right = NULL,
    bottom = NULL,
    width = NULL,
    height = NULL,
    opacity = 0.8,
    panel_id = paste0("custom_abs_panel", such_panel_seq)
) {
  ns <- shiny::NS(ns_id)
  btn_id_ns <- ns(btn_id)
  content_id_ns <- ns(content_id)
  panel_id_ns <- ns(panel_id)

  js_code <- sprintf(
    "(function() {
  var panel = document.getElementById('%s');
  var btn = document.getElementById('%s');
  var offset = [0,0];
  var isDown = false;
  var dragStart = [0,0];
  var dragged = false;
  var dragThreshold = 5;

  btn.addEventListener('mousedown', function(e) {
    isDown = true;
    dragged = false;
    dragStart = [e.clientX, e.clientY];
    offset = [
      panel.offsetLeft - e.clientX,
      panel.offsetTop - e.clientY
    ];
    document.body.style.userSelect = 'none';
    e.preventDefault();
  });

  document.addEventListener('mouseup', function(e) {
    if (isDown) {
      var dx = Math.abs(e.clientX - dragStart[0]);
      var dy = Math.abs(e.clientY - dragStart[1]);
      if (dx < dragThreshold && dy < dragThreshold) {
        var x = document.getElementById('%s');
        if (x.style.display === 'none') {
          x.style.display = 'block';
          btn.innerHTML = '-';
        } else {
          x.style.display = 'none';
          btn.innerHTML = '+';
        }
      }
    }
    isDown = false;
    document.body.style.userSelect = '';
  });

  document.addEventListener('mousemove', function(e) {
    if (isDown) {
      var dx = Math.abs(e.clientX - dragStart[0]);
      var dy = Math.abs(e.clientY - dragStart[1]);
      if (dx > dragThreshold || dy > dragThreshold) {
        dragged = true;
      }
      if (dragged) {
        panel.style.left = (e.clientX + offset[0]) + 'px';
        panel.style.top  = (e.clientY + offset[1]) + 'px';
      }
    }
  });
})();",panel_id_ns, btn_id_ns, content_id_ns)

  btn_style <- "
    margin-bottom: 2px;
    width: 20px;
    height: 20px;
    border: none;
    background: rgba(200, 200, 200, 0.4);
    color: #222;
    border-radius: 50%;
    font-size: 1em;
    font-weight: bold;
    cursor: move;
    transition: background 0.2s;
    outline: none;
    box-shadow: 0 2px 8px rgba(0,0,0,0.10);
    line-height: 20px;
    text-align: center;
    padding: 0;"

  shiny::absolutePanel(
    id = panel_id_ns,
    top = top, left = left, right = right, bottom = bottom,
    width = width, height = height,
    # Draggability handled via JS
    shiny::tags$button(
      id = btn_id_ns,
      "-", # Start as expanded
      style = btn_style,
      "aria-label" = "Toggle panel and drag",
      onmouseover = "this.style.background='rgba(160,160,160,0.7)'",
      onmouseout = "this.style.background='rgba(200,200,200,0.4)'"
    ),
    shiny::div(
      id = content_id_ns,
      ...,
      style = paste0("opacity: ", opacity, ";")
    ),
    shiny::tags$script(shiny::HTML(js_code))
  )
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


  # To store ready state for DT
  DT_ready <- shiny::reactiveVal(FALSE)

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
      ok_state <- TRUE
    }

    if(input$comp_type != "collated" &&
       any(input$show_cols %in% composed_options[-4])) {
      ok_state <- TRUE
    }

    if(ok_state) {

      if(input$comp_type == "collated") {
        # Use output of collate_column
        d_this <- traceback_render_now()$comp_orig_collate

        cnames_this <- colnames(d_this)

        if(!("Uncollated Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^uncollated_")])
        }

        if(!("Info Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^info_")])
        }

        d_this <- d_this[cnames_this]

      } else {
        # Use output of compose
        d_this <- traceback_render_now()$comp_orig

        cnames_this <- colnames(d_this)

        if(!("Mid Priority Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^mid_")])
        }

        if(!("Low Priority Columns" %in% input$show_cols)){
          cnames_this <- cnames_this %>%
            setdiff(cnames_this[stringr::str_detect(cnames_this,"^low_")])
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

      ph <- plot_handle()
      shiny_util_cells_plot_it(
        ui_params = ph$plot_params,
        rc_sel = rc_sel,
        ca_now = ca_now)
    }
  }, execOnResize = TRUE)


  output$dt_trace <- DT::renderDT({
    shiny::req(traceback_render_now())

    # Color mapping
    col_name_to_color_map <- data.frame(
      colname = c("data_gid", "row", "col", "value", "high_long", "mid_long_1", "high_short"),
      color = c("#853194", "#1e6627", "#FF5733", "#0066CC", "#E91E63", "#009688", "#673AB7"),
      stringsAsFactors = FALSE
    )

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
      target_row = selection_combined()$row[1], target_col = selection_combined()$col[1],
      #target_row = 65, target_col = 5,
      col_colors = col_name_to_color_map,
      hide_these_cols = hide_these_cols
    )

    DT_ready(TRUE) # Set DT ready state to TRUE

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

      # Set DT ready state to FALSE to indicate that DT is required to re-render
      DT_ready(FALSE)
    }

    if(!is.null(selection_from_DT())){
      # Update from DT
      selection_combined(
        selection_from_DT()[1,c("row","col")]
      )

      # Set DT ready state to FALSE to indicate that DT is required to re-render
      DT_ready(FALSE)
    }

  })


  # Special case of scrolling DT to selected point in the plot

  output$dt_scroll_js <- shiny::renderUI({

    # Render JavaScript that executes immediately when button is clicked
    if(!is.null(selection_from_plot()) && DT_ready() && FALSE) {

      cat("hit\n")

      this_d <- data_for_DT()
      sel <- selection_from_plot()

      rowNum <- this_d$row_id[
        this_d$row == sel$row[1] & this_d$col == sel$col[1]]

      rowNum <- min(max(rowNum,min(this_d$row_id)),max(this_d$row_id))

      # DT uses 0-based indexing for rows, so subtract 1 and we need target row
      # in middle that's why another -3 (as default page length is 10)
      targetRow <- min(max(rowNum - 1 - 3, 0), max(this_d$row_id) - 1)

      # Note this is passed as argument for shiny_util_DT like shiny_util_DT(
      # .... DT_api_js_var_name = ....) here default is being used.
      DT_api_js_var_name <- "this_dt_api_access"

      shiny::tags$script(
        shiny::HTML(
          sprintf(
            paste(
              sep = "\n",
              "(function() {",
              "function scrollToRowIfRequired() {",
              "var pinf = window.%s.page.info();",
              "var mid = (pinf.start + pinf.end) / 2;",
              "var targetRow = %d;",
              "if (Math.abs(mid - targetRow) > 5) {",
              "window.%s.scroller().scrollToRow(targetRow, true);",
              "}}",
              "try {",
              "scrollToRowIfRequired();",
              "} catch (e) {",
              "setTimeout(scrollToRowIfRequired, 100);",
              "}})();"), DT_api_js_var_name, targetRow, DT_api_js_var_name)
        ))



      shiny::tags$script(
        shiny::HTML(sprintf(
          paste(
            sep = "\n",
            "(function() {",
            "  var targetRow = %d;",
            "  var apiVar = '%s';",
            "  var interval = setInterval(function() {",
            "    if (window[apiVar] && window[apiVar].page && window[apiVar].scroller) {",
            "      try {",
            "        var pinf = window[apiVar].page.info();",
            "        var mid = (pinf.start + pinf.end) / 2;",
            "        if (Math.abs(mid - targetRow) > 5) {",
            "          window[apiVar].scroller().scrollToRow(targetRow, true);",
            "        }",
            "      } catch (e) { /* swallow */ }",
            "      clearInterval(interval);",
            "    }",
            "  }, 500);",
            "})();"
          ),
          targetRow, DT_api_js_var_name
        ))
      )


    } else {
      # Empty UI
      shiny::tagList()
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
    }
  })






  # TODO Return
  return(NULL)


}



ok_function <- function(){

  library(shiny)
  library(DT)

  big_iris <- do.call(rbind, replicate(50, iris, simplify = FALSE))
  big_iris$row_id <- seq_len(nrow(big_iris))

  ui <- fluidPage(
    h3("Scroller + Pagination + page.info() to R"),
    DTOutput("tbl"),
    verbatimTextOutput("info")
  )

  server <- function(input, output, session) {
    output$tbl <- renderDT({
      datatable(
        big_iris,
        extensions = "Scroller",
        options = list(
          pageLength = 10,
          scrollY = 300,
          scroller = TRUE,
          deferRender = TRUE,
          paging = TRUE,
          initComplete =  DT::JS(
            "function(settings, json){",
            paste0("window.this_dt = this.api();"),

            "
          function sendPageInfo() {
          if(window.this_dt.page) {
            Shiny.onInputChange('dt_info', window.this_dt.page.info());
          }
          }

          setTimeout(sendPageInfo, 100);

        window.this_dt.on('draw', sendPageInfo);
          window.this_dt.on('scroller:scroll', sendPageInfo);

          ",

            "}")
        )
      )
    })

    output$info <- renderPrint({
      input$dt_info
    })



  }

  shinyApp(ui, server)


}
