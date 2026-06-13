
# This file contains the UI components for the Shiny modules of this package.

shiny_ui_part_plot_rc_selection <- function(
    id, d, top = 2, left = 2) {


  row_range <- range(d$row, na.rm = TRUE)
  col_range <- range(d$col, na.rm = TRUE)

  row_sel_range <- c(row_range[1], min(row_range[2],100)) # Limit to 100 for UI
  col_sel_range <- c(col_range[1], min(col_range[2],30)) # Limit to 30 for UI

  ns <- shiny::NS(id)

  shiny_util_ui_absolute_panel(

    shiny::div(
      shiny::plotOutput(
        ns("plot_rc_select"), height = "200px"),

      shiny::sliderInput(
        ns("selected_rows"), label = "Rows to Display",
        min = row_range[1], max = row_range[2],
        value = c(row_sel_range[1], row_sel_range[2]),
        step = 1),
      shiny::sliderInput(
        ns("selected_cols"), label = "Columns to Display",
        min = col_range[1], max = col_range[2],
        value = c(col_sel_range[1], col_sel_range[2]),
        step = 1),
      style = "margin-top:2px; margin-bottom:2px; margin-left:5px; margin-right:5px; width: 200px;"
    ),

    ns_id = id, such_panel_seq = 1,
    top = top, left = left, width = 200, height = 200
  )
}

shiny_ui_part_plot_tune <- function(
    id = "ui_plot_tune", d, ca = NULL,
    init_show_values_in_cells =  FALSE) {

  # Cells Analysis mode if `ca` is provided
  CA_mode <- !is.null(ca)

  if(CA_mode) {
    d <- ca$original_sheet
  }

  ns <- shiny::NS(id)

  cols <- colnames(d) %>% setdiff(c("row", "col", "value"))
  cols <- unique(c("none", cols))

  miniUI::miniTabPanel(
    "Parameters",
    icon = shiny::icon("sliders-h"), # Corrected icon name
    miniUI::miniContentPanel(
      padding = 0,
      # Main Plot Output
      shiny::plotOutput(ns("plot_tune"), height = "530px"),
      # Row Col Selection Panel
      shiny_ui_part_plot_rc_selection(
        id = id, d = d
      )
    ),
    miniUI::miniButtonBlock(
      shiny::fluidRow(
        # Group 1: Fill Parameters
        shiny::column(
          4,
          shiny::h6("Fill Options:"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                ns("fill"),
                label = "Fill Cells Using:",
                choices = cols,
                selected = "data_type",
                multiple = FALSE
              ),
              shiny::conditionalPanel(
                condition = ifelse(CA_mode,"ture","false"), ns = ns,
                shiny::h6("Note: This (fill) will not have any impact in this module!")
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.fill != 'none'", ns = ns,
                shiny::sliderInput(
                  ns("fill_alpha"),
                  label = "Fill Transparency",
                  min = 0, max = 1, value = 1, step = 0.05)
              )
            )
          )
        ),
        # Group 2: Text Parameters
        shiny::column(
          4,
          shiny::h6("Text Options:"),
          shiny::fluidRow(
            shiny::column(
              5,
              shiny::checkboxInput(
                ns("no_txt"), label = "Disable Texts", value = FALSE)
            ),
            shiny::column(
              7,
              shiny::conditionalPanel(
                condition = "!input.no_txt", ns = ns,
                shiny::sliderInput(
                  ns("txt_size"), label = "Text Size",
                  min = 1, max = 10, value = 4, step = 0.5)
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "!input.no_txt", ns = ns,
                shiny::sliderInput(
                  ns("txt_alpha"), label = "Text Transparency",
                  min = 0, max = 1, value = 1, step = 0.05)
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "!input.no_txt", ns = ns,
                shiny::sliderInput(
                  ns("max_txt_len"), label = "Max-Text Length",
                  min = 1, max = 30, value = 20, step = 1)
              )
            )
          )
        ),
        # Group 3: General Parameters
        shiny::column(
          4,
          shiny::h6("Other Options:"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("add_cell_address"),
                label = "Add Cell Address", value = FALSE)
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("no_legend"),
                label = "Disable Legend", value = TRUE)
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("auto_round_values"),
                label = "Auto Round Values in Cells", value = TRUE)
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = ifelse(CA_mode,"ture","false"), ns = ns,
                shiny::checkboxInput(
                  ns("color_attrs_separately"),
                  label = "Color Attributes Separately", value = TRUE)
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = ifelse(CA_mode,"ture","false"), ns = ns,
                shiny::checkboxInput(
                  ns("show_values_in_cells"),
                  label = "Show Values in Cells",
                  value = init_show_values_in_cells)
              )
            )
          )
        )
      )
    )
  )
}

shiny_ui_part_crop <- function(id = "ui_crop", d) {
  ns <- shiny::NS(id)

  miniUI::miniTabPanel(
    "Crop",
    icon = shiny::icon("crop-alt"),
    miniUI::miniContentPanel(
      padding = 0,
      # Crop Plot Output
      shiny::plotOutput(
        ns("plot_crop"), height = "100%",
        brush = shiny::brushOpts(ns("brush_crop"))),
      # Row Col Selection Panel
      shiny_ui_part_plot_rc_selection(
        id = id, d = d
      )
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton(ns("data_crop"), "Crop"),
      shiny::actionButton(ns("data_del"), "Delete"),
      shiny::actionButton(
        ns("data_reset"), "Reset",
        title = "Load original data", icon = shiny::icon("undo")
      )
    )
  )
}

shiny_ui_part_va_classify <- function(id = "ui_va_classify", d) {
  ns <- shiny::NS(id)

  miniUI::miniTabPanel(
    "Classify",
    icon = shiny::icon("table"),
    miniUI::miniContentPanel(
      padding = 0,
      # Classification Plot Output
      shiny::plotOutput(
        ns("plot_va_classify"),
        height = "100%",
        brush = shiny::brushOpts(ns("brush_va_classify"))
      ),
      # Row Col Selection Panel
      shiny_ui_part_plot_rc_selection(
        id = id, d = d
      )
    ),
    miniUI::miniButtonBlock(
      shiny::h4("Make selection as:"),
      shiny::actionButton(ns("make_value_va_classify"), "Values"),
      shiny::actionButton(ns("make_attr_va_classify"), "Attribute"),
      shiny::actionButton(
        ns("reset_va_classify"),
        "Reset All",
        # tooltip
        title = "Load original data with basic classification",
        icon = shiny::icon("undo")
      )
    )
  )
}

# omod: orientation_modification
shiny_ui_part_orientation_modification <- function(id = "ui_omod", ca) {

  ns <- shiny::NS(id)

  d <- ca$original_sheet

  miniUI::miniTabPanel(
    "Cell Analysis Refinement",
    icon = shiny::icon("arrows"),
    miniUI::miniContentPanel(
      padding = 0,
      # Cells Plot Output
      shiny::plotOutput(
        ns("plot_omod"),
        height = "100%",
        # Click and Brush Options not working together inside miniUI - Possible
        # bug in shiny or miniUI
        #
        # brush = shiny::brushOpts(ns("brush_omod")), - KFL
        click = shiny::clickOpts(ns("click_omod"))
      ),
      # Row Col Selection Panel
      shiny_ui_part_plot_rc_selection(
        id = id, d = d, top = 260
      ),
      # Inject JS that will collapse Row Col Selection Panel after 2 sec
      shiny::tags$script(shiny::HTML(
        paste0(
          ' setTimeout(function() {\n',
          '$("#',ns("toggle_abs_panel1"),'").click();\n',
          '}, 2000);'
        )
      )),

      # This is used instead of miniUI::miniButtonBlock
      shiny_util_ui_absolute_panel(
        shiny::div(
          shiny::div(style = "height:1px;"),
          shiny::h4("Modification Controls"),
          shiny::uiOutput(ns("ui_omod_control")),
          shiny::div(style = "height:1px;"),
          shiny::actionButton(
            ns("reset_omod"),
            "Reset All",
            title = "Load original 'Cells Analysis'",
            icon = shiny::icon("undo")
          ),
          shiny::div(style = "height:2px;"),
          shiny::checkboxInput(
            ns("dynamic_focus"),
            # Add a tooltip
            label = shiny::span("Dynamic Focus", title = "Enable or disable dynamic focus"),
            value = TRUE
          ),
          shiny::div(style = "height:2px;"),
          style = "margin-top:5px; margin-bottom:5px; margin-left:5px; margin-right:5px; width: 300px;"
        ),
        ns_id = id, such_panel_seq = 2,
        top = 2, left = 2, width = 300, height = 200, opacity = 0.95)

    )
  )
}

shiny_ui_part_traceback <- function(id = "ui_traceback", ca) {
  # The package {DT} is required for this which is ensured by parent functions
  # and hence not checking here.

  ns <- shiny::NS(id)

  js_to_disable_first_option <- function(target_id, trigger_id) {
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "
      // Define a function to disable the first checkbox
      function disableFirstCheckbox(inputId) {
        // Small delay to ensure DOM is updated
        setTimeout(function() {
          // Get all checkboxes in the group and disable the first one
          var checkboxes = $('#' + inputId + ' input[type=\"checkbox\"]');
          if (checkboxes.length > 0) {
            $(checkboxes[0]).prop('disabled', true);
            $(checkboxes[0]).parent().css('opacity', '0.7');
            $(checkboxes[0]).parent().css('cursor', 'not-allowed');
          }
        }, 100);
      }

      // Run initially
      $(document).ready(function() {
        disableFirstCheckbox('%s');
      });

      // Set up an observer for Shiny input changes
      $(document).on('shiny:inputchanged', function(event) {
        if(event.name === '%s') {
          // When comparison type changes, the checkbox group will update
          // So we need to disable the first checkbox again
          disableFirstCheckbox('%s');
        }
      });
    ", ns(target_id), ns(trigger_id), ns(target_id)
        )
      ))
  }

  d <- ca$original_sheet

  miniUI::miniTabPanel(
    "Traceback",
    icon = shiny::icon("route"),

    miniUI::miniContentPanel(
      padding = 0,

      shiny::plotOutput(
        ns("plot_traceback"),
        height = "100%",
        click = shiny::clickOpts(ns("click_traceback"))
      ),
      # Row Col Selection Panel
      shiny_ui_part_plot_rc_selection(
        id = id, d = d, top = 420
      ),
      # Inject JS that will collapse Row Col Selection Panel after 2 sec
      shiny::tags$script(shiny::HTML(
        paste0(
          ' setTimeout(function() {\n',
          '$("#',ns("toggle_abs_panel1"),'").click();\n',
          '}, 2000);'
        )
      )),
      # This is used instead of miniUI::miniButtonBlock
      shiny_util_ui_absolute_panel(
        shiny::div(
          shiny::div(style = "height:10px;"),
          DT::DTOutput(ns("dt_trace"), height = "100%"),
          shiny::div(style = "height:10px;"),
          style = "font-size:70%; margin-top:5px; margin-bottom:5px; margin-left:8px; margin-right:8px; width: 600px;"
        ),
        ns_id = id, such_panel_seq = 2,
        top = 2, left = 2, width = 600, height = 300, opacity = 0.95),

      # Control Panel
      shiny_util_ui_absolute_panel(
        shiny::div(
          shiny::div(style = "height:1px;"),
          shiny::h4("Controls"),
          shiny::div(style = "height:1px;"),

          shiny::fluidRow(
            shiny::column(
              6,
              # This is not required kept only for reference
              #
              # shiny::actionButton(
              #   ns("recompose_traceback"),
              #   "Recompose",
              #   title = "Compose Again Based on Updated 'Cells Analysis'",
              #   icon = shiny::icon("sync")
              # ),
              shiny::div(style = "height:8px;"),
              shiny::radioButtons(
                ns("comp_type"),
                "Render Type:",
                choices = c("collated","composed only"),
                selected = "collated"),
              shiny::checkboxInput(
                ns("dynamic_focus_traceback"),
                # Add a tooltip
                label = shiny::span("Dynamic Focus", title = "Enable or disable dynamic focus to data-block"),
                value = TRUE
              ),
              shiny::checkboxInput(
                ns("discard_const_cols"),
                # Add a tooltip
                label = shiny::span("Discard Constant Columns", title = "Discard/Disable Constant Columns (which has only one Non-NA value)."),
                value = TRUE
              )
            ),

            shiny::column(
              6,
              shiny::checkboxGroupInput(
                ns("show_cols"),
                label = "Show Columns",
                choices = c("Collated Columns","Info Columns","Uncollated Columns","Cell Address"),
                selected = "Collated Columns"
              ),
              shiny::checkboxInput(
                ns("animate_DT_scroll"),
                # Add a tooltip
                label = shiny::span("Animate Scroll", title = "Adds animation to DT scrolling from top."),
                value = FALSE
              )
            )
          ),


          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("show_displayed_cols_only"),
                # Add a tooltip
                label = shiny::span(
                  "Connections: Show Displayed Columns",
                  title = "Show Only Currently Displayed Columns in the Connection. If disabled, all other columns in the connection will be shown, including those currently hidden from the display. To view these columns, you may need to adjust column visibility from the control panel."),
                value = TRUE
              ),
              shiny::radioButtons(
                ns("connection_line_type"),
                "Connection Line Type:",
                choices = c("L-shaped","Curved"),
                selected = "L-shaped")
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("connection_line_add_jitter"),
                # Add a tooltip
                label = shiny::span(
                  "Connections: Add Jitter",
                  title = "Add Random Jitter to Lines"),
                value = TRUE
              ),
              shiny::sliderInput(
                ns("connection_line_alpha"),
                "Connection Line Alpha:",min = 0.1, max = 1, value = 0.7)
            )
          ),


          shiny::fluidRow(
            shiny::column(
              12,
              shiny_util_attempted_collapsible_panel(
                id = ns("collapse_fine_sel"),
                title = "Advanced: Columns (Further Selection)",
                style = "primary",

                # The content to be collapsed (or shown directly)
                shiny::selectizeInput(
                  ns("show_cols_fine_sel"),
                  label = shiny::span(
                    "Select Specific Columns:",
                    title = shiny_util_msg()$fine_col_sel
                  ),
                  choices = NULL,
                  multiple = TRUE
                )
              )
            )
          ),

          js_to_disable_first_option(target_id = "show_cols", trigger_id = "comp_type"),
          shiny::div(style = "height:2px;"),
          style = "margin-top:5px; margin-bottom:5px; margin-left:5px; margin-right:5px; width: 350px;"
        ),
        ns_id = id, such_panel_seq = 3,
        top = 2, left = 810, width = 350, height = 200, opacity = 0.95),
      # Inject JS that will collapse Row Col Selection Panel after 2 sec
      shiny::tags$script(shiny::HTML(
        paste0(
          ' setTimeout(function() {\n',
          '$("#',ns("toggle_abs_panel3"),'").click();\n',
          '}, 5000);'
        )
      ))
    )
  )
}
