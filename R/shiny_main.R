
# The Shiny Application for the package is defined here. Note that shiny based
# features are optional and the package can be used without them.
#
# The shiny application is defined into modular manner.
#  - 1) shiny_utils.R contains utility functions for the shiny applications. All other may use these functions.
#  - 2) shiny_parts_ui.R contains the UI components for the Shiny modules.
#  - 3) shiny_parts_server_components.R contains the server components for the Shiny modules.
#  - 4) shiny_parts_server.R contains the server logic for the Shiny modules. It uses functions from shiny_parts_server_components.R
#  - 5) shiny_main.R contains the main shiny application. It uses functions from shiny_parts_server.R and shiny_parts_ui.R


# Shiny App for Value-Attribute Classification
shiny_app_va_classify <- function(d) {

  shiny_util_check_and_init()

  ########## Pre-Processing ##########

  # If the input data is not gone through value-attribute classification,
  # perform a simple heuristic classification.
  if (!utils::hasName(d, "PoA") || !utils::hasName(d, "PoV")) {
    d <- d %>%
      value_attribute_classify(method = "simple_heuristic")
  }


  ########## UI ##########
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Value-Attribute Classifier"),
    miniUI::miniTabstripPanel(
      shiny_ui_part_va_classify(d = d),
      shiny_ui_part_crop(d = d),
      shiny_ui_part_plot_tune(d = d),
      id = "tab_main"
    )
  )


  ########## Server ##########
  server <- shiny_server_va_classify(d)

  shiny_util_runApp(
    ui = ui,
    server = server,
    title = "TidyCells: Value/Attribute Classifier"
  )
}

# Shiny App for Cell Analysis Orientation Modification
# This app allows users to modify the orientation (and others) of cell analysis results.
shiny_app_orientation_modification <- function(ca) {
  # This function expects a cell analysis object (ca) (a result of
  # analyze_cells())

  shiny_util_check_and_init()

  ########## Pre-Processing ##########

  ########## UI ##########
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Cell Analysis Orientation Modification"),
    miniUI::miniTabstripPanel(
      shiny_ui_part_orientation_modification(ca = ca),
      shiny_ui_part_plot_tune(ca = ca),
      id = "tab_main"
    )
  )

  ########## Server ##########
  server <- shiny_server_orientation_modification(ca)

  shiny_util_runApp(
    ui = ui,
    server = server,
    title = "TidyCells: Value/Attribute Classifier"
  )


}

# Shiny App for Cell Analysis Traceback
# This app allows users to trace back the cell analysis results to the original cells.
# It is useful for debugging and understanding the analysis process.
shiny_app_traceback <- function(ca) {
  # This function expects a cell analysis object (ca) (a result of
  # analyze_cells())

  # Check {DT} is available or not
  if (!pkg_is_available("DT")) {
    rlang::abort(
      message = cli_bold_red(
        "The {DT} package is required for this functionality."
      ),
      call = NULL
    )
  }

  shiny_util_check_and_init()

  ########## Pre-Processing ##########

  ########## UI ##########
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Cell Analysis Traceback"),
    miniUI::miniTabstripPanel(
      shiny_ui_part_traceback(ca = ca),
      shiny_ui_part_orientation_modification(ca = ca),
      shiny_ui_part_plot_tune(ca = ca, init_show_values_in_cells = TRUE),
      id = "tab_main"
    )
  )

  ########## Server ##########
  server <- shiny_server_traceback(ca)

  shiny_util_runApp(
    ui = ui,
    server = server,
    title = "TidyCells: Cell Analysis Traceback"
  )
}
