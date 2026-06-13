

shiny_server_va_classify <- function(d) {

  # Dev Notes:
  #
  # Following is done in parent function : shiny_app_va_classify
  # - Ensure input cells is gone through value-attribute classification
  #
  # If the input data is not gone through value-attribute classification,
  # perform a simple heuristic classification.
  #
  # if (!utils::hasName(d, "PoA") || !utils::hasName(d, "PoV")) {
  #   d <- d %>%
  #     value_attribute_classify(method = "simple_heuristic")
  # }

  function(input, output, session) {
    # d_now is working copy of d
    d_now <- shiny::reactiveVal(d)
    # d_orig is the original copy of d (reactive version)
    d_orig <- shiny::reactiveVal(d)

    # Plot parameters
    plot_handle <- shiny::callModule(
      shiny_sps_part_plot_tune, id = "ui_plot_tune",
      d_now = d_now)

    # Cropped data (cells)
    d_cropped <- shiny::callModule(
      shiny_sps_part_crop, id = "ui_crop",
      d_now = d_now, d_orig = d_orig, plot_handle = plot_handle)

    # The "value-attribute classification" performed cells
    d_now_va <- shiny::callModule(
      shiny_sps_part_va_classify, id = "ui_va_classify",
      d_now = d_now, d_orig = d_orig, plot_handle = plot_handle)

    # Loop-back d_cropped to d_now
    shiny::observeEvent(d_cropped(), {
      if (!identical(d_cropped(), d_now())) {
        d_now(d_cropped())
      }
    })

    # Loop-back d_now_va to d_now
    shiny::observeEvent(d_now_va(), {
      if (!identical(d_now_va(), d_now())) {
        d_now(d_now_va())
      }
    })

    # Handle cancel and done events
    shiny::observeEvent(input$cancel, {
      shiny::stopApp({
        cat(cli_red("Value-Attribute Classification Canceled\n"))
      })
    })

    # Complete the app and return the classified data if Done is clicked
    shiny::observeEvent(input$done, {
      shiny::stopApp(d_now_va())
    })
  }
}

shiny_server_orientation_modification <- function(ca) {
  function(input, output, session) {

    # ca_now is working copy of ca
    ca_now <- shiny::reactiveVal(ca)

    # ca_orig is the original copy of ca (reactive version)
    ca_orig <- shiny::reactiveVal(ca)

    # Plot parameters
    plot_handle <- shiny::callModule(
      shiny_sps_part_plot_tune, id = "ui_plot_tune",
      ca_now = ca_now)

    ca_mod <- shiny::callModule(
      shiny_sps_part_orientation_modification, id = "ui_omod",
      ca_now = ca_now, ca_orig = ca_orig, plot_handle = plot_handle)


    # Loop-back ca_mod to ca_now
    shiny::observeEvent(ca_mod(), {
      if (!identical(ca_mod(), ca_now())) {
        ca_now(ca_mod())
      }
    })

    # Handle cancel and done events
    shiny::observeEvent(input$cancel, {
      shiny::stopApp({
        cat(cli_red("Orientation Modification Canceled\n"))
      })
    })

    # Complete the app and return the classified data if Done is clicked
    shiny::observeEvent(input$done, {
      shiny::stopApp(
        # Fix for attr_data_map for nice_header_name which need to be unique per
        # attribute block within a data-block connected scope.
        infer_util_cells_analysis_fix_nice_header_name(
          ca_mod()))
    })


  }
}

shiny_server_traceback <- function(ca) {
  function(input, output, session) {

    # ca_now is working copy of ca
    ca_now <- shiny::reactiveVal(ca)

    # ca_orig is the original copy of ca (reactive version)
    ca_orig <- shiny::reactiveVal(ca)

    # Plot parameters
    plot_handle <- shiny::callModule(
      shiny_sps_part_plot_tune, id = "ui_plot_tune",
      ca_now = ca_now)

    ca_mod <- shiny::callModule(
      shiny_sps_part_orientation_modification, id = "ui_omod",
      ca_now = ca_now, ca_orig = ca_orig, plot_handle = plot_handle)

    rendered_df <- shiny::callModule(
      shiny_sps_part_traceback, id = "ui_traceback",
      ca_now = ca_now, plot_handle = plot_handle)


    # Loop-back ca_mod to ca_now
    shiny::observeEvent(ca_mod(), {
      if (!identical(ca_mod(), ca_now())) {
        ca_now(ca_mod())
      }
    })

    # Handle cancel and done events
    shiny::observeEvent(input$cancel, {
      shiny::stopApp({
        cat(cli_red("Trackback Operation Canceled\n"))
      })
    })

    # Complete the app and return the classified data if Done is clicked
    shiny::observeEvent(input$done, {
      shiny::stopApp(rendered_df())
    })

  }
}
