
# attach like
# server <- server_va_select(d)
server_va_select <- function(d) {
  if (!hasName(d, "type")) {
    d <- basic_classifier(d)
  }

  function(input, output, session) {
    d_now <- reactiveVal()
    d_now(d)

    d_va_now <- reactiveVal()
    d_va_now(d)

    plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)

    callModule(sps_part_plotly, "ui_visualize", plot_now = plot_now)

    dat_new_va_classify <- callModule(sps_part_va_classify, "ui_va_classify", plot_now = plot_now, d_now = d_now, d_orig = d)

    dat_new_crop <- callModule(sps_part_crop, "ui_crop", plot_now = plot_now, d_now = d_now, d_orig = d_va_now)

    observeEvent(dat_new_crop(), {
      if (!identical(dat_new_crop(), d_now())) {
        d_now(dat_new_crop())
      }
    })

    observeEvent(dat_new_va_classify(), {
      if (!identical(dat_new_va_classify(), d_now())) {
        d_now(dat_new_va_classify())
        d_va_now(dat_new_va_classify())
      }
    })

    observeEvent(input$cancel, {
      stopApp(abort("Value-Attribute Classification Canceled"))
    })

    observeEvent(input$done, {
      stopApp(d_now() %>% select(row, col, data_type, value, type))
    })
  }
}

server_crop <- function(d) {
  if (!hasName(d, "type")) {
    d <- basic_classifier(d)
  }


  function(input, output, session) {
    d_now <- reactiveVal()
    d_now(d)

    plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)

    callModule(sps_part_plotly, "ui_visualize", plot_now = plot_now)

    dat_new_crop <- callModule(sps_part_crop, "ui_crop", plot_now = plot_now, d_now = d_now, d_orig = d)

    observeEvent(dat_new_crop(), {
      if (!identical(dat_new_crop(), d_now())) {
        d_now(dat_new_crop())
      }
    })

    observeEvent(input$cancel, {
      stopApp(abort("Crop Data Canceled"))
    })

    observeEvent(input$done, {
      stopApp(d_now() %>% select(row, col, data_type, value, type))
    })
  }
}

server_data_block <- function(x) {
  d <- x$cell_df

  function(input, output, session) {
    d_now <- reactiveVal()
    d_now(d)

    plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)

    info_dblock <- callModule(sps_part_data_block, "ui_data_block", plot_now = plot_now, ca = x)

    callModule(sps_part_plotly, "ui_visualize", plot_now = info_dblock$plot)

    observeEvent(input$cancel, {
      stopApp()
    })

    observeEvent(input$done, {
      stopApp()
    })
  }
}

server_orientation_modification <- function(x) {
  d <- x$cell_df

  function(input, output, session) {
    d_now <- reactiveVal()
    d_now(d)

    ca_current <- reactiveVal()
    ca_current(x)

    gids_self <- reactiveVal()
    gids_self("All")

    plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)

    info_dblock <- callModule(sps_part_data_block, "ui_data_block",
      plot_now = plot_now, ca = ca_current, now_gids = gids_self
    )

    callModule(sps_part_plotly, "ui_visualize", plot_now = info_dblock$plot)

    ca_this <- callModule(sps_part_orientation_modification, "ui_orientation_modification",
      plot_now = info_dblock$plot, ca = ca_current, gid_now = info_dblock$gids
    )


    observeEvent(ca_this(), {
      if (!identical(ca_this(), ca_current())) {
        ca_current(ca_this())
        gids_self(info_dblock$gids())
      }
    })

    observeEvent(input$cancel, {
      stopApp(abort("Orientation Modification Canceled"))
    })

    observeEvent(input$done, {
      stopApp(ca_this())
    })
  }
}

server_traceback <- function(x, dcomp) {
  d <- x$cell_df

  dcomp <- attach_trace_info(x, dcomp)

  dcomp <- dcomp %>%
    mutate(RN = seq(value))

  function(input, output, session) {
    d_now <- reactiveVal()
    d_now(d)

    plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)

    gids_self <- reactiveVal()
    gids_self("All")

    info_dblock <- callModule(sps_part_data_block, "ui_data_block",
      plot_now = plot_now, ca = x,
      now_gids = gids_self
    )

    callModule(sps_part_plotly, "ui_visualize", plot_now = info_dblock$plot)

    gid_sel <- callModule(sps_part_traceback, "ui_traceback", dcomp = dcomp, ca = x, prior_ca_plot = info_dblock$plot)

    observeEvent(gid_sel(), {
      if (!identical(gid_sel(), info_dblock$gids())) {
        gids_self(gid_sel())
      }
    })

    observeEvent(input$cancel, {
      stopApp(abort("Traceback Canceled"))
    })

    observeEvent(input$done, {
      stopApp(invisible(dcomp))
    })
  }
}
