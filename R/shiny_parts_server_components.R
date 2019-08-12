
# sps: shiny parts server

# attach this module in shiny server body like this:
# plot_now <- callModule(sps_part_plot_now, "ui_plot_tune", d_now = d_now)
sps_part_plot_now <- function(input, output, session, d_now) {
  if (!is.reactive(d_now)) {
    .d <- d_now
    d_now <- function() {
      .d
    }
  }

  plot_now <- reactive({
    if (is.null(input$fill)) {
      graphics::plot(d_now(), no_plot = TRUE)
    } else {
      graphics::plot(
        d_now(),
        fill = input$fill, adaptive_txt_size = input$adaptive_txt_size,
        txt_size = input$txt_size, txt_alpha = input$txt_alpha,
        no_txt = input$no_txt,
        no_plot = TRUE
      )
    }
  })

  output$plot_tune <- renderPlot({
    graphics::plot(plot_now())
  })

  return(plot_now)
}


# helper function

# kept for a known limitation / issue in plotly
# ref :
# https://github.com/ropensci/plotly/issues/1562
# https://stackoverflow.com/questions/46604893/issue-when-trying-to-plot-geom-tile-using-ggplotly/56888579
patch_plot_for_plotly <- function(g) {
  aes_old <- g$mapping %>% map_chr(rlang::as_label)

  if (is.na(aes_old["fill"])) {
    # re-attach the fill asthetic manually
    glsel <- g$layers %>% map_lgl(~ "GeomTile" %in% class(.x$geom))

    # in this case only fist Tile will not have a fill asthetic
    glsel <- glsel %>% which()

    if (length(glsel) != 2) {
      abort("not sure whether things are fine!")
    }

    glsel1 <- glsel[1]
    glsel2 <- glsel[2]



    gl1 <- g$layers[[glsel1]]
    gl2 <- g$layers[[glsel2]]

    gl1$mapping <- gl2$mapping
    gl1$mapping$x <- NULL
    gl1$mapping$y <- NULL
    gl1$mapping$fill <- rlang::as_quosure("z_dummy")

    gl1 -> g$layers[[glsel1]]

    plt <- plotly::ggplotly(g)

    for (i in seq_along(plt$x$data)) {
      if (identical(plt$x$data[[i]]$legendgroup, "z_dummy")) {
        plt$x$data[[i]]$showlegend <- FALSE
      }
    }
  } else {
    plt <- plotly::ggplotly(g)
  }

  plt
}

# optional server logic
sps_part_plotly_raw <- function(input, output, session, plot_now) {
  plot_this <- reactiveVal()

  observe({
    if (is.reactive(plot_now)) {
      plot_this(plot_now())
    } else {
      plot_this(plot_now)
    }
  })

  noti_id <- reactiveVal()
  noti_id(NULL)

  output$plot_plotly <- plotly::renderPlotly({
    withProgress(message = "Generating plotly", value = 0, {
      incProgress(0.1, detail = paste("Please wait (don't change tabs)"))

      ggp <- plot_this() %>%
        patch_plot_for_plotly()

      incProgress(0.1, detail = paste("Almost Done"))

      ggp <- ggp %>%
        plotly::style(hovertemplate = paste(c(
          "<i>Row</i>:%{y:.0f}",
          "<i>Col</i>:-%{x:.0f}",
          "<b>%{text}</b>"
        ), collapse = "<br>"))

      incProgress(0.8, detail = paste("Done"))
    })

    id_ <- showNotification("Kindly wait for few seconds yet, to load the plotly!", duration = 0)

    noti_id(id_)

    ggp
  })

  observeEvent(noti_id(), {
    if (!is.null(noti_id())) {
      removeNotification(noti_id())
      noti_id(NULL)
    }
  })
}

# attach like
# callModule(sps_part_plotly, "ui_visualize", plot_now = plot_now)
sps_part_plotly <- function(input, output, session, plot_now) {
  if (plotly_present()) {
    sps_part_plotly_raw(input, output, session, plot_now)
  }
}


# attach like
# dat_new_va_classify <- callModule(sps_part_va_classify, "ui_va_classify", plot_now = plot_now, d_now = d_now, d_orig = d)
sps_part_va_classify <- function(input, output, session, plot_now, d_now, d_orig) {
  if (!is.reactive(d_now)) {
    .d <- d_now
    d_now <- function() {
      .d
    }
  }

  if (!is.reactive(d_orig)) {
    .d_orig <- d_orig
    d_orig <- function() {
      .d_orig
    }
  }


  if (!is.reactive(plot_now)) {
    .p <- plot_now
    plot_now <- function() {
      .p
    }
  }


  selected_points <- reactive({
    brushedPoints(d_now() %>%
      mutate(x = col, y = -row),
    input$brush_va_classify,
    xvar = "x", yvar = "y"
    )
  })

  output$plot_va_classify <- renderPlot({
    plot_now() +
      # show selected points
      ggplot2::geom_tile(
        mapping = ggplot2::aes(col, -row),
        data = selected_points(),
        color = "#F07973", fill = "#949684A8", inherit.aes = FALSE,
        alpha = 0.5, na.rm = TRUE,
        width = 1, height = 1
      )
  })


  new_dv <- reactiveVal()
  new_dv(NULL)

  observeEvent(input$make_value_va_classify, {
    selected_cells <- selected_points()
    if (nrow(selected_cells) > 0) {
      dat <- d_now()
      d_rest <- dat %>% anti_join(selected_cells, by = c("row", "col"))
      dat_new <- selected_cells %>%
        mutate(type = "value") %>%
        bind_rows(d_rest)
      session$resetBrush(input$brush_va_classify$brushId)
    } else {
      dat_new <- d_now()
    }
    new_dv(dat_new)
  })

  new_da <- reactiveVal()
  new_da(NULL)

  observeEvent(input$make_attr_va_classify, {
    selected_cells <- selected_points()
    if (nrow(selected_cells) > 0) {
      dat <- d_now()
      d_rest <- dat %>% anti_join(selected_cells, by = c("row", "col"))
      dat_new <- selected_cells %>%
        mutate(type = "attribute") %>%
        bind_rows(d_rest)
      session$resetBrush(input$brush_va_classify$brushId)
    } else {
      dat_new <- d_now()
    }
    new_da(dat_new)
  })

  is_reset <- reactiveVal()
  is_reset(FALSE)


  observeEvent(input$reset_va_classify, {
    is_reset(TRUE)
  })



  new_d <- reactive({
    input$make_attr_va_classify
    input$make_value_va_classify
    input$reset_va_classify

    nd <- NULL
    isolate({
      if (!is_reset()) {
        if (!is.null(new_dv())) {
          nd <- new_dv()
          new_dv(NULL)
        }
        if (!is.null(new_da())) {
          nd <- new_da()
          new_da(NULL)
        }
      } else {
        is_reset(FALSE)
        nd <- d_orig()
        new_dv(NULL)
        new_da(NULL)
      }

      if (is.null(nd)) {
        nd <- d_now()
      } else {
        session$resetBrush(input$brush_crop$brushId)
        new_dv(NULL)
        new_da(NULL)
      }
    })
    nd
  })

  return(invisible(new_d))
}


# attach like
# dat_new_crop <- callModule(sps_part_crop, "ui_crop", plot_now = plot_now, d_now = d_now, d_orig = d)
sps_part_crop <- function(input, output, session, plot_now, d_now, d_orig) {
  if (!is.reactive(d_now)) {
    .d <- d_now
    d_now <- function() {
      .d
    }
  }

  if (!is.reactive(d_orig)) {
    .d_orig <- d_orig
    d_orig <- function() {
      .d_orig
    }
  }


  if (!is.reactive(plot_now)) {
    .p <- plot_now
    plot_now <- function() {
      .p
    }
  }

  selected_points <- reactive({
    brushedPoints(d_now() %>%
      mutate(x = col, y = -row),
    input$brush_crop,
    xvar = "x", yvar = "y"
    )
  })

  output$plot_crop <- renderPlot({
    plot_now() +
      # show selected points
      ggplot2::geom_tile(
        mapping = ggplot2::aes(col, -row),
        data = selected_points(),
        color = "#F07973", fill = "#949684A8", inherit.aes = FALSE,
        alpha = 0.5, na.rm = TRUE,
        width = 1, height = 1
      )
  })

  new_dd <- reactiveVal()
  new_dd(NULL)

  observeEvent(input$data_del, {
    selected_cells <- selected_points()
    if (nrow(selected_cells) > 0) {
      dat <- d_now()
      d_rest <- dat %>% anti_join(selected_cells, by = c("row", "col"))
      dat_new <- d_rest
      session$resetBrush(input$brush_crop$brushId)
    } else {
      dat_new <- d_now()
    }
    new_dd(dat_new)
  })

  new_dc <- reactiveVal()
  new_dc(NULL)

  observeEvent(input$data_crop, {
    selected_cells <- selected_points()
    if (nrow(selected_cells) > 0) {
      dat_new <- selected_cells
      session$resetBrush(input$brush_crop$brushId)
    } else {
      dat_new <- d_now()
    }
    new_dc(dat_new)
  })

  is_reset <- reactiveVal()
  is_reset(FALSE)


  observeEvent(input$data_reset, {
    is_reset(TRUE)
  })

  new_d <- reactive({
    input$data_crop
    input$data_del
    input$data_reset

    nd <- NULL
    isolate({
      if (!is_reset()) {
        if (!is.null(new_dc())) {
          nd <- new_dc()
          new_dc(NULL)
        } else {
          if (!is.null(new_dd())) {
            nd <- new_dd()
            new_dd(NULL)
          }
        }
      } else {
        is_reset(FALSE)
        nd <- d_orig()
      }


      if (is.null(nd)) {
        nd <- d_now()
      } else {
        session$resetBrush(input$brush_crop$brushId)
      }
    })
    nd
  })

  return(invisible(new_d))
}

# attach like
# info_dblock <- callModule(sps_part_data_block, "ui_data_block", plot_now = plot_now, ca = x)
sps_part_data_block <- function(input, output, session, plot_now, ca, now_gids) {
  if (!is.reactive(plot_now)) {
    .p <- plot_now
    plot_now <- function() {
      .p
    }
  }

  arg_there <- FALSE
  if (!missing(now_gids)) {
    if (is.reactive(now_gids)) {
      arg_there <- TRUE
    }
  }


  now_gids_this <- reactiveVal()

  observe({
    if (arg_there) {
      now_gids_this(now_gids())
    } else {
      now_gids_this("All")
    }
  })


  # ca : cell analysis
  if (!is.reactive(ca)) {
    .ca <- ca
    ca <- function() {
      .ca
    }
  }

  observe({
    updateSelectizeInput(session,
      inputId = "gids",
      label = "Select Data Block ids",
      choices = c("All", ca()$cells$natural_gid %>% unique()),
      selected = now_gids_this(),
      options = list(
        placeholder = "Please select any group id...",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })


  gids_this <- reactive({
    gids0 <- input$gids

    if (any(gids0 == "All") & length(gids0) > 1) {
      gids0 <- setdiff(gids0, "All")
    }

    if (is.null(gids0)) {
      gids0 <- "All"
    }

    if (any(gids0 == "All")) {
      gids0 <- ca()$cells$natural_gid %>% unique()
    } else {
      gids0 <- as.numeric(gids0)
    }
    gids0
  })

  plot_this <- reactive({
    graphics::plot(ca(),
      prior_plot = plot_now(),
      gids = gids_this(),
      zoom_selected_gids = input$zoom_selected_gids,
      plot_cell_base_color = FALSE,
      block_boundary = input$block_boundary,
      direction_text = input$direction_text,
      direction_text_on_all = input$direction_text_on_all,
      dat_att_boundary = input$dat_att_boundary,
      plot_issues = input$plot_issues,
      no_plot = TRUE
    )
  })

  output$plot_data_block <- renderPlot({
    graphics::plot(plot_this())
  })

  return(list(plot = plot_this, gids = gids_this))
}

# attach like
# callModule(sps_part_orientation_modification, "ui_orientation_modification", plot_now = info_dblock$plot, ca = x, gid_now = info_dblock$gids)
sps_part_orientation_modification <- function(input, output, session, plot_now, ca, gid_now) {
  if (!is.reactive(plot_now)) {
    .p <- plot_now
    plot_now <- function() {
      .p
    }
  }

  # ca : cell analysis
  if (!is.reactive(ca)) {
    .ca <- ca
    ca <- function() {
      .ca
    }
  }

  click_pt <- reactiveVal()
  click_pt(NULL)

  observeEvent(input$click_omod, {
    if (!is.null(input$click_omod)) {
      if (!identical(input$click_omod, click_pt())) {
        click_pt(input$click_omod)
      }
    }
  })

  observeEvent(input$dbclick_omod, {
    click_pt(NULL)
    session$resetBrush(input$brush_omod$brushId)
  })

  selected_points <- reactive({
    d0 <- ca()$cell_df

    d00 <- d0 %>%
      mutate(x = col, y = -row)

    if (!is.null(input$brush_omod)) {
      init_sel <- brushedPoints(d00,
        input$brush_omod,
        xvar = "x", yvar = "y"
      )
    } else {
      init_sel <- nearPoints(d00, click_pt(), xvar = "x", yvar = "y", threshold = 80, addDist = TRUE)
      if (nrow(init_sel) > 0) {
        init_sel <- init_sel %>% filter(dist_ == min(dist_))
      }
    }

    final_sel <- init_sel

    isolate({
      if (nrow(init_sel) > 0) {
        gid0 <- gid_now()
        dam0 <- ca()$details$data_attr_map_raw
        dam0_this <- dam0 %>% filter(natural_gid %in% gid0)
        dam0_this_a <- init_sel %>%
          select(row_a = row, col_a = col) %>%
          inner_join(dam0_this, by = c("row_a", "col_a"))

        attr_gid0 <- dam0_this_a %>%
          distinct(attr_gid) %>%
          pull(1)
        attr_gid_split0 <- dam0_this_a %>%
          distinct(attr_gid_split) %>%
          pull(1)

        final_sel <- dam0_this %>%
          filter(attr_gid %in% attr_gid0) %>%
          filter(attr_gid_split %in% attr_gid_split0) %>%
          select(row = row_a, col = col_a) %>%
          inner_join(d0, by = c("row", "col")) %>%
          full_join(init_sel, by = c("row", "col", "data_type", "type")) %>%
          distinct(row, col, data_type, type)
      }
    })

    final_sel
  })

  output$plot_omod <- renderPlot({
    plot_now() +
      # show selected points
      ggplot2::geom_tile(
        mapping = ggplot2::aes(col, -row),
        data = selected_points(),
        color = "#F07973", fill = "#949684A8", inherit.aes = FALSE,
        alpha = 0.5, na.rm = TRUE,
        width = 1, height = 1
      )
  })

  # reactiveness not utilized
  info_this <- reactiveVal()
  info_this(list())

  # reactiveness not utilized
  doable <- reactiveVal()
  doable(FALSE)

  ca_now <- reactiveVal()
  observe(ca_now(ca()))


  # update new_direction using updateSelectizeInput
  observe({
    input$allow_all_dirs

    sel_pts <- selected_points()

    reset_this <- FALSE

    isolate({
      if (nrow(sel_pts) > 0) {
        if ("attribute" %in% sel_pts$type) {
          gid0 <- gid_now()
          gid_map <- ca()$cells

          if (length(gid0) == 0) {
            this_gids <- gid_map %>%
              inner_join(sel_pts, by = c("row", "col")) %>%
              pull(gid) %>%
              unique()
          } else {
            this_gids <- gid_map %>%
              filter(natural_gid %in% gid0) %>%
              pull(gid) %>%
              unique()
          }


          dam_old <- ca()$details$data_attr_map_raw
          dam_old <- dam_old %>% mutate(id = seq(gid))

          this_dam <- dam_old %>%
            filter(gid %in% this_gids) %>%
            inner_join(sel_pts %>%
              filter(type == "attribute") %>%
              select(row_a = row, col_a = col),
            by = c("row_a", "col_a")
            )
          n_dg <- this_dam$direction_group %>%
            unique() %>%
            length()



          if (n_dg == 1) {
            all_dirs <- get_unpivotr_direction_names()

            all_dirs_mains <- all_dirs[c("N", "E", "W", "S")]

            max_dir <- this_dam %>%
              group_by(direction) %>%
              summarise(n = n())


            valid_main_dirs <- all_dirs_mains %>%
              map_lgl(~ any(max_dir$direction %in% .x)) %>%
              which() %>%
              names()

            if (length(intersect(valid_main_dirs, c("E", "W"))) == 2 |
              length(intersect(valid_main_dirs, c("N", "S"))) == 2) {
              updateSelectizeInput(session,
                inputId = "new_direction",
                label = paste0(
                  "Select Direction\n",
                  "(selected cells possibly in different data block or simply in opposite direction)"
                ),
                choices = "Not Applicable",
                options = list(
                  placeholder = "Select a specific data block / attribute block ...",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            } else {
              valid_dirs <- all_dirs %>%
                map_lgl(~ any(max_dir$direction %in% .x)) %>%
                all_dirs[.] %>%
                unlist() %>%
                unique()

              max_dir <- max_dir %>%
                summarise(direction[which.max(n)]) %>%
                pull(1)

              if (length(setdiff(all_dirs, valid_dirs)) == 0) {
                warn("all_dirs and valid_dirs same which is not expected")
              }

              if (input$allow_all_dirs) {
                give_dirs <- all_dirs
              } else {
                give_dirs <- valid_dirs
              }

              doable(TRUE)

              lst <- list(dam = dam_old, this = this_dam)
              info_this(lst)

              updateSelectizeInput(session,
                inputId = "new_direction",
                label = "Select Direction",
                choices = give_dirs,
                selected = max_dir,
                options = list(
                  placeholder = "Select direction to apply ...",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            }
          } else {
            updateSelectizeInput(session,
              inputId = "new_direction",
              label = "Select Direction (selected cells in different direction groups)",
              choices = "Not Applicable",
              options = list(
                placeholder = "Select cells in single direction ...",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          }
        } else {
          reset_this <- TRUE
        }
      } else {
        reset_this <- TRUE
      }

      if (reset_this) {
        updateSelectizeInput(session,
          inputId = "new_direction",
          label = "Select Direction",
          choices = "Not Applicable",
          options = list(
            placeholder = "Please Select Cells ...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      }
    })
  })

  observeEvent(input$apply_direction, {
    if (doable() & !is.null(input$new_direction)) {
      dam_this <- info_this()$dam
      part_this <- info_this()$this
      dir_this <- input$new_direction[1]
      all_dirs <- get_unpivotr_direction_names() %>% unlist()
      if (dir_this %in% all_dirs) {
        if (length(setdiff(part_this$direction, dir_this))) {
          part_this$direction <- dir_this
          dam_this_ex <- dam_this %>% anti_join(part_this, by = "id")
          dam_new <- dam_this_ex %>%
            bind_rows(part_this) %>%
            select(-id)
          ca_new <- ca()
          if (identical(dim(dam_new), dim(ca_new$details$data_attr_map_raw))) {
            ca_new$details$data_attr_map_raw <- dam_new
            ca_now(ca_new)
          } else {
            warn("failed to create new CA")
          }
        }
      }
    }
  })

  return(invisible(ca_now))
}


# helper function

# optional server logic
sps_part_traceback_raw <- function(input, output, session, dcomp, ca, prior_ca_plot) {
  if (!is.reactive(dcomp)) {
    .dcomp <- dcomp
    dcomp <- function() {
      .dcomp
    }
  }

  # ca : cell analysis
  if (!is.reactive(ca)) {
    .ca <- ca
    ca <- function() {
      .ca
    }
  }

  if (!is.reactive(prior_ca_plot)) {
    .prior_ca_plot <- prior_ca_plot
    prior_ca_plot <- function() {
      .prior_ca_plot
    }
  }

  rev_sel_row <- reactiveVal()
  rev_sel_row(NULL)

  proxy <- DT::dataTableProxy("dt_trace", session = session)

  this_dc <- reactiveVal()
  this_dc(NULL)

  output$dt_trace <- DT::renderDT({
    dc0 <- dcomp()

    showcols <- colnames(dc0) %>%
      stringr::str_detect("[major|minor|collated]_[0-9]+$") %>%
      colnames(dc0)[.]

    std_opts <- list(
      pageLength = 5,
      keys = TRUE,
      sDom = '<"top">lrt<"bottom">ipB',
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      ordering = FALSE,
      # basic : buttons = I("colvis")
      buttons = list(
        list(
          extend = "colvis",
          text = as.character(tags$a("Columns", style = "font-size:70%"))
        )
      )
    )

    if (is.null(rev_sel_row())) {
      opts <- std_opts
      dc0_disp <- dc0[c("RN", "value", "data_block", showcols)]
    } else {

      # kept for later
      # below option is not working properly for all rows
      # hence using custom solution
      #
      # # this is the ideal way but not always working
      # # specifically is the selected row is beyond some number
      # # this number is not fixed it's sometime 30 in viewer
      # # ref :
      # # https://stackoverflow.com/questions/47911673/r-shiny-scrolling-to-a-given-row-of-datatable-with-javascript-callback
      # sel_opt <- list(
      #   initComplete  = JS(paste("function() {",
      #                            # this is  not working (kept for reference):# paste0("this.api().table().row(",rev_sel_row()-1,").node().scrollIntoView();"),
      #                            # below and this are same : paste0("this.api().table().row(",rev_sel_row()-1,").scrollTo();"),
      #                            paste0("this.api().row( ",rev_sel_row()-1," ).scrollTo();"),
      #                            # this is  not working (kept for reference):# paste0("this.api().table().scroller.toPosition(",rev_sel_row()-1,")"),
      #                            "}",
      #                            sep = "\n"))
      # )
      #
      # opts <- c(std_opts, sel_opt)

      # instead of above I simply rearranged
      opts <- std_opts

      rw <- rev_sel_row()

      dc0 <- dc0 %>%
        mutate(dummy_order = (RN - rw)^2)

      dc0_disp <- dc0 %>% arrange(dummy_order, RN)

      dc0_disp <- dc0_disp[c("RN", "value", "data_block", showcols)]


      proxy %>% DT::selectRows(1)
    }

    this_dc(dc0_disp)


    DT::datatable(dc0_disp %>% select(-data_block),
      selection = "single",
      escape = FALSE,
      rownames = FALSE,
      style = "bootstrap",
      class = "cell-border stripe",
      extensions = c("KeyTable", "Scroller", "Buttons"),
      options = opts
    )
  },
  server = TRUE
  )

  selected_row <- reactiveVal()
  selected_row(NULL)

  selected_gid <- reactiveVal()
  selected_gid(NULL)


  output$plot_traceback <- renderPlot({
    cell_trace_plot(dcomp(), trace_row = selected_row(), ca = ca(), prior_ca_plot = prior_ca_plot())
  })


  click_pt <- reactiveVal()
  click_pt(NULL)

  noti_id <- reactiveVal()
  noti_id(NULL)

  observe({
    if (is.null(selected_row())) {
      id_ <- showNotification(paste("Bi-directional selection is possible.",
        "Either select a row from table or <double click> on plot to select a cell.",
        "Select a row to start",
        sep = " "
      ),
      duration = 0
      )
      noti_id(id_)
    }
  })

  observeEvent(input$click_traceback, {
    if (!is.null(input$click_traceback)) {
      click_pt(input$click_traceback)
    }
  })

  observeEvent(input$dt_trace_rows_selected, {
    dc0 <- this_dc()
    this_rn <- dc0$RN[input$dt_trace_rows_selected]
    if (!identical(selected_row(), this_rn)) {
      if (!is.null(noti_id())) {
        removeNotification(noti_id())
        noti_id(NULL)
      }
      selected_row(this_rn)
      selected_gid(dc0$data_block[input$dt_trace_rows_selected])
      # this is not required as otherwise theselection will be removed : rev_sel_row(NULL)
    }
  })


  observe({
    click_pt()

    isolate({
      d00 <- dcomp() %>%
        mutate(x = col, y = -row, r_num = seq(value))

      this_sel <- nearPoints(d00, click_pt(), xvar = "x", yvar = "y", threshold = 80, addDist = TRUE)
      if (nrow(this_sel) > 0) {
        this_sel <- this_sel %>% filter(dist_ == min(dist_))
      }

      if (nrow(this_sel) > 0) {
        if (!identical(this_sel$r_num, input$dt_trace_rows_selected)) {
          rev_sel_row(as.numeric(this_sel$r_num))
          click_pt(NULL)
        }
      }
    })
  })


  return(selected_gid)
}

# attach like
# gid_sel <- callModule(sps_part_traceback, "ui_traceback", dcomp = dcomp, ca = x, prior_ca_plot = info_dblock$plot)
sps_part_traceback <- function(input, output, session, dcomp, ca, prior_ca_plot) {
  if (DT_present()) {
    sps_part_traceback_raw(input, output, session, dcomp, ca, prior_ca_plot)
  }
}
