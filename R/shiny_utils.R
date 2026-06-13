# This file contains utility functions for working with Shiny applications.

# Function to check for the availability of the 'shiny' and 'miniUI'
shiny_util_check_and_init <- function(interactive_check = TRUE) {
  # This function checks whether the 'shiny' and 'miniUI' packages are
  # available, and initializes each shiny module in this package. Specifically,
  # it captures the state of attached packages prior to calling any shiny
  # module, so that this state can be restored afterwards via
  # 'shiny_util_auto_detach'. This helps ensure the search path remains clean
  # and prevents unintended auto-attachment of 'shiny'. Additionally, if
  # 'interactive_check' is TRUE, the function verifies that the session is
  # interactive before proceeding.
  #
  # If shiny_util_runApp is used to run the app, it will automatically
  # handle the session end and call shiny_util_auto_detach().

  # Check interactive session if 'interactive_check' is TRUE.
  if (interactive_check) {
    if (!interactive()) {
      rlang::abort(
        message = cli_bold_red(
          "This functionality requires an interactive R session.\n"
        ),
        call = NULL
      )
    }
  }

  # Check if 'shiny', 'miniUI', and 'fontawesome' packages are available.
  required_pkgs <- c("shiny", "miniUI", "ggplot2", "fontawesome")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, pkg_is_available, logical(1))]

  if (length(missing_pkgs)) {
    rlang::abort(
      message = cli_bold_red(
        sprintf("The following packages are required for this functionality: %s.",
                paste0("'", missing_pkgs, "'", collapse = ", "))
      ),
      call = NULL
    )
  }

  # Check and cache the attached packages before initializing 'shiny' and
  # 'miniUI'. this is useful for optionally detaching them later as
  # shiny::runApp directly attaches them to the active session's search path.

  attached_pkgs <- util_attached_packages()

  # There are mechanisms to store and revert to the prior state before this
  # package was loaded, which is especially useful for handling the first-run
  # scenario. However, users may still choose to explicitly attach 'shiny' in
  # these cases.
  #
  # For reliability, it is better to perform this check every time, allowing us
  # to consistently revert to the state prior to each shiny module call.
  #
  # The code below is retained for reference only.
  #
  # if(!isTRUE(util_pkg_cache( "shiny_initialized",
  #   pkg_cache_head_name = "shiny"))){
  #   # If not, initialize it.
  #   util_pkg_cache(
  #     shiny_initialized = TRUE,
  #     attached_pkgs_before_shiny_init = attached_pkgs ,
  #     pkg_cache_head_name = "shiny")
  # }

  util_pkg_cache(
    attached_pkgs_before_shiny_init = attached_pkgs,
    pkg_cache_head_name = "shiny")

  # If shiny is not already attached, attach it now. This will suppress
  # "loading required packages - shiny" messages (as it's loaded silently
  # here).

  if(!("shiny" %in% attached_pkgs)) {
    es <- loadNamespace("shiny")
    tryCatch(
      attachNamespace(es),
      error = function(e) NULL, warnig = function(w) NULL)
  }

  invisible(0)
}

# Function to Auto Detach 'shiny' and 'miniUI' Packages
shiny_util_auto_detach <- function() {
  # This function auto detaches the 'shiny' and 'miniUI' packages if they were
  # initialized by any shiny module in this package. It checks whether the user
  # has set the 'auto_detach_shiny' option; if not, it defaults to TRUE,
  # enabling automatic detachment of these packages.
  #
  # For this mechanism to work, each shiny module in this package should begin
  # by calling shiny_util_check_and_init(), and should call
  # shiny_util_auto_detach() after completing its task.
  #
  # Additionally, within the server component, you should include:
  #
  # session$onSessionEnded(function() {
  #   shiny_util_auto_detach()
  # })
  #
  # This ensures that 'shiny_util_auto_detach()' is executed on any exit,
  # including when the app is closed using the X button.
  #
  # If shiny_util_runApp is used to run the app, it will automatically
  # handle the session end and call shiny_util_auto_detach().

  auto_detach_shiny <- core_opt_get("auto_detach_shiny", default = TRUE)

  if (auto_detach_shiny) {

    # Check attached packages before shiny init.
    attached_pkgs_before_shiny_init <-
      util_pkg_cache("attached_pkgs_before_shiny_init",
                     pkg_cache_head_name = "shiny")

    # Check attached packages now.
    attached_pkgs_now <- util_attached_packages()

    new_attached_pkgs <-
      setdiff(attached_pkgs_now, attached_pkgs_before_shiny_init)

    # If 'shiny' and 'miniUI' are attached now but were not before shiny init,
    # then detach them. First miniUI and then shiny.
    if ("miniUI" %in% new_attached_pkgs) {
      tryCatch(
        util_detach_pkg_if_attached("miniUI"),
        error = function(e) NULL, warning = function(w) NULL
      )
    }

    if ("shiny" %in% new_attached_pkgs) {
      tryCatch(
        util_detach_pkg_if_attached("shiny"),
        error = function(e) NULL, warning = function(w) NULL
      )
    }
  }
}


# Dev Note: for debugging and development purposes, this can be added somewhere
# in the package R sources then the app will run in viewer pane.
#
# .onLoad <- function(libname, pkgname) {
#   util_pkg_cache(debug = TRUE)
# }
#
# This will ensure that the Shiny app runs with the provided UI and server and
# adheres to shiny_util_auto_detach mechanisms.
shiny_util_runApp <- function(
    ui, server,
    title = "Prompt Window", width = 1200, height = 800,
    # Below is a internal debug flag to run the app in debug mode. Enable it by
    # running util_pkg_cache(debug =TRUE)
    debug = isTRUE(util_pkg_cache("debug")),
    debug_in_viewer = isTRUE(util_pkg_cache("debug_in_viewer"))) {

  # This may be required to called before running the app.
  # shiny_util_check_and_init()

  ui_mod <- shiny::tagList(
    # Add this line to explicitly load the Font Awesome icon library
    fontawesome::fa_html_dependency(),
    ui
  )

  # Modify the server function to ensure it uses the provided
  # server function and includes the auto-detach mechanism.
  server_mod <- function(input, output, session) {

    # Actual Server Function (user supplied)
    server(input, output, session)

    # Ensure auto-detach on session end
    session$onSessionEnded(function() {
      shiny_util_auto_detach()
    })
  }

  if(debug){
    # Early exit in debug mode to avoid running the app in silent mode.
    return(
      shiny::runApp(
        shiny::shinyApp(
          ui = ui_mod,
          server = server_mod),
        # If debug_in_viewer is set to TRUE use Viewer, otherwise launch in
        # Web-Browser
        launch.browser = ifelse(debug_in_viewer, shiny::paneViewer(), TRUE),
        quiet = TRUE
      )
    )
  }

  # Create a temporary file to capture output
  tmpfile <- tempfile()
  sink(tmpfile)
  # Ensure that the temporary file is removed after the app is closed
  on.exit({
    sink()
    unlink(tmpfile)
  })

  # Now run (safely) the Shiny app with the provided UI and server functions.
  tryCatch(
    shiny::runApp(
      shiny::shinyApp(
        ui = ui_mod,
        server = server_mod),
      launch.browser = shiny::dialogViewer(
        dialogName = title,
        width = width,
        height = height
      ),
      quiet = TRUE
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )
}


# Utility functions for user interaction and object selection in Shiny or RStudio contexts

shiny_util_global_objects <- function(cls = c("cells")) {
  # This function lists all objects in the global environment that inherit from
  # a specified class (default is "cells", but can be "cells_analysis" or
  # "sheets" or any other class). It returns a character vector of object names
  # that match the class.

  cls <- as.character(cls[1])  # Ensure cls is a character vector of length 1

  # List all objects in the global environment
  objs <- ls(envir = globalenv())

  # For each object, check if it inherits from the specified class
  # Keep only the names of objects that match the class
  cobjs <- objs[
    vapply(
      objs,
      function(obj_name) inherits(get(obj_name, envir = globalenv()), cls),
      logical(1)
    )
  ]

  # Return the vector of matching object names
  cobjs
}



shiny_util_select_one_of <- function(
    g_vars,
    title = "Object Selection for Visual Functions",
    msg = "Select one of the objects to proceed:") {
  # Standard session start-up for shiny modules (for auto-detach mechanism)
  shiny_util_check_and_init()

  n <- length(g_vars)

  # --- Simplified width and height calculation ---

  # 1. Truncate labels and create color vector
  button_labels <- purrr::map_chr(
    g_vars, function(label) {
      if (nchar(label) > 20) {
        paste0(substr(label, 1, 17), "...")
      } else {
        label
      }
    })
  btn_col <- rep_len(c("#d5eced", "#e5ede4"), length.out = n)

  # 2. Set layout parameters
  buttons_per_line <- min(n, 3)
  num_rows <- ceiling(n / buttons_per_line)

  # 3. Calculate width based on the number of buttons per line
  # (150px button + 8px gap) * buttons + 30px padding
  width <- buttons_per_line * 158 + 30
  width <- min(max(width, 200), 1000) # Clamp width

  # 4. Calculate height based on the number of rows
  # (30px button + 8px gap) * rows + 50px header + 30px padding
  height <- num_rows * 38 + 80
  height <- min(max(height, 150), 1000) # Clamp height

  # --- UI and Server ---

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .button-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
          gap: 8px;
        }
      "))
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4(msg),
        shiny::uiOutput("button_group")
      )
    )
  )

  server <- function(input, output, session) {
    output$button_group <- shiny::renderUI({
      buttons <- lapply(seq_along(button_labels), function(i) {
        shiny::actionButton(
          inputId = paste0("var_btn_", i),
          label = button_labels[i],
          style = paste0("background-color: ", btn_col[i], ";")
        )
      })
      shiny::div(class = "button-grid", buttons)
    })

    # Observe all button presses
    lapply(seq_along(g_vars), function(i) {
      shiny::observeEvent(input[[paste0("var_btn_", i)]], {
        shiny::stopApp(g_vars[i])
      })
    })
  }

  # Run the app and return the selected value
  shiny_util_runApp(
    ui, server,
    title = title, width = width, height = height
  )
}


shiny_util_global_object_picker <- function(what = c("cells")) {
  # Pick a global object of the specified class from the user's global
  # environment.
  #  - If only one object is found, return it.
  #  - If none are found, do nothing. (This is not an error.)
  #  - If multiple, prompt the user to select one by name.

  what <- as.character(what[1]) # Ensure 'what' is a character vector of length 1

  objs <- shiny_util_global_objects(cls = what)

  # If only one object is found, return it directly.
  if (length(objs) == 1) {
    return(get(objs, envir = globalenv()))
  }

  # If no objects are found, print a message and interrupt the process.
  if (length(objs) < 1) {
    cli_blue(paste0(
      "No <", cli_bold_red(what),
      "> - Object found in the global environment!\n"))
    rlang::interrupt()
  }

  # If multiple objects are found, prompt the user to select one.
  if (length(objs) > 1) {
    sel <- shiny_util_select_one_of(
      g_vars = objs,
      title = paste0("Select a ", what, " - object"),
      msg = paste0(
        "Multiple ", what, " - objects found in the global environment. ",
        "Please select one to proceed:"))
    return(get(sel, envir = globalenv()))
  }

}




# This function creates an drag-able absolute panel in a Shiny app that can be
# toggled
shiny_util_ui_absolute_panel <- function(
    ...,
    ns_id,
    such_panel_seq = 1,
    btn_id  = paste0("toggle_abs_panel", such_panel_seq),
    content_wrap_id = paste0("content_wrap", such_panel_seq),
    content_id = paste0("collapsible_content", such_panel_seq),
    top = NULL,
    left = NULL,
    right = NULL,
    bottom = NULL,
    width = NULL,
    height = NULL,
    draggable = TRUE,
    opacity = 0.9) {

  # Create a namespace function for module compatibility
  ns <- shiny::NS(ns_id)
  btn_id_ns <- ns(btn_id)                     # Button's namespaced ID
  content_wrap_id_ns <- ns(content_wrap_id)   # Wrapper div's namespaced ID
  content_id_ns <- ns(content_id)             # Content div's namespaced ID

  # JavaScript code for toggling content wrapper visibility
  # This hides/shows the entire wrapper, actually freeing up space when hidden
  js_code <- sprintf(
    "
    document.getElementById('%s').onclick = function() {
      var wrap = document.getElementById('%s');
      var btn = document.getElementById('%s');
      if (wrap.style.display === 'none') {
        wrap.style.display = 'block';  // Show content
        btn.innerHTML = '-';           // Change button label to minus
      } else {
        wrap.style.display = 'none';   // Hide content (frees space)
        btn.innerHTML = '+';           // Change button label to plus
      }
    }
    ",
    btn_id_ns, content_wrap_id_ns, btn_id_ns
  )

  # Inline CSS style for the toggle button
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
  cursor: pointer;
  transition: background 0.2s;
  outline: none;
  box-shadow: 0 2px 8px rgba(0,0,0,0.10);
  line-height: 20px;
  text-align: center;
  padding: 0;"

  # Create the absolute panel container
  shiny::absolutePanel(
    top = top, left = left, right = right, bottom = bottom,
    draggable = draggable,

    # Toggle button (always visible even when content is hidden)
    shiny::tags$button(
      id = btn_id_ns,
      "-", # Initial label shows minus (open state)
      style = btn_style,
      onmouseover = "this.style.background='rgba(160,160,160,0.7)'",
      onmouseout = "this.style.background='rgba(200,200,200,0.4)'"
    ),

    # Content wrapper div (collapsible)
    shiny::div(
      id = content_wrap_id_ns,
      shiny::div(
        id = content_id_ns,
        ...,
        # Style for the inner content area
        style = paste0(
          "opacity: ", opacity, ";",
          "background: rgba(255, 255, 255, ", opacity, ");",
          "box-shadow: 0 2px 8px rgba(0,0,0,", round(opacity/3, 2), ");",
          "border-radius: 8px;"
        )
      )
    ),

    # Add the toggle JavaScript into the document
    shiny::tags$script(shiny::HTML(js_code))
  )
}



# A convenience function to plotting cells inside shiny modules. It's parameters
# can be reactive and this is meant for reactive context.
shiny_util_cells_plot_it <- function(
    ui_params,
    # ROW/COL selection context (reactive)
    rc_sel,
    # For Cells Analysis Plot
    ca_now = NULL,
    # Additional parameters to pass to plot.cells
    ...) {
  # Dynamic plot to be used inside shiny modules.

  # Here instead of using plot.cells_analysis, we use plot.cells directly.

  # If ca_now is provided, we assume it is intended for a cells_analysis object.
  if(!is.null(ca_now)) {
    # Convert the cells analysis for plotting. Only conv_d$scale_fill_map is
    # required as conv_d$combined_data will be flowing through
    # rc_sel$cells_for_plot() which will be taken care by this function
    conv_d <-  util_convert_cells_analysis_for_plot(
      ca_now(),
      color_attrs_separately = ui_params$color_attrs_separately,
      show_values_in_cells = ui_params$show_values_in_cells)

    # Specific modification for fill parameter as required for
    # plot.cells_analysis
    ui_params$fill <- "type"

    scale_fill_values_this <- conv_d$scale_fill_map

  } else {
    scale_fill_values_this <- NULL
  }

  # Dev Note: as `plot.cells` returns invisible
  # ggplot, it is required to store it is gg object and then return it. (to
  # avoid invisible return). Otherwise renderPlot might not work properly.
  gg <- plot.cells(
    rc_sel$cells_for_plot(),
    # Fill parameters
    fill = ui_params$fill,
    fill_alpha = ui_params$fill_alpha,
    no_fill = ui_params$fill == "none",
    # Text parameters
    txt_size = ui_params$txt_size,
    txt_alpha = ui_params$txt_alpha,
    no_txt = ui_params$no_txt,
    max_txt_len = ui_params$max_txt_len,
    # Others
    add_cell_address = ui_params$add_cell_address,
    no_legend = ui_params$no_legend,
    auto_round_values = ui_params$auto_round_values,
    # Non-user parameters
    no_plot = TRUE,
    omit_NSE = TRUE,
    # For Speed
    ignore_validation = TRUE,
    # For Cells Analysis Plot
    allow_rc_df = TRUE,
    # For Cells Analysis
    color_attrs_separately = ui_params$color_attrs_separately,
    show_values_in_cells = ui_params$show_values_in_cells,
    # Below is required for color labeling custom attributes (for
    # plot.cells_analysis)
    scale_fill_values = scale_fill_values_this,
    ...
  )
  return(gg)
}


shiny_util_clicked_tile <- function(df, click) {
  if(is.null(click)) return(NULL)
  row <- round(-click$y)
  col <- round(click$x)
  df[df$row == row & df$col == col, ]
}


shiny_util_get_full_block_from_cells_analysis <- function(
    selection, ca_cells){
  if(NROW(selection) == 0) {
    return(tibble::tibble(row = integer(0), col = integer(0)))
  }
  ca_cells %>%
    dplyr::filter(.data$value %in% unique(selection$value)) %>%
    dplyr::select(c("row","col"))
}


# This function is helper for rendering DT for this package's Shiny apps
shiny_util_DT <- function(
    data, # The data to show in DT
    target_row = NULL, # Optional row value to identify row_index
    target_col = NULL, # Optional column value to identify row_index
    row_index = NULL, # Optional row index to highlight a specific row
    bg_color = "#fcbddc50", # Background color for highlighting the row
    col_colors = data.frame(
      colname = character(),
      color = character()
    ),
    hide_these_cols = character(0),
    DT_api_js_var_name = "this_dt_api",
    DT_animate_scroll = FALSE,
    truncate_length = 20, # Feature: configurable visible cell text length
    tooltip_length = 200  # Feature: configurable hover tooltip text length
) {



  # Add a temporary unique identifier column (row_id) if not already present
  if(!utils::hasName(data, "row_id")) {
    data <- dplyr::mutate(data, row_id = seq_len(dplyr::n()))
  }

  # If target_row and target_col are provided, find the corresponding row_index
  if(!is.null(target_row) && !is.null(target_col)) {
    row_index <- data %>%
      dplyr::filter(
        .data$row == target_row,
        .data$col == target_col
      ) %>%
      dplyr::pull(.data$row_id)
    # If multiple matches, take the first one
    row_index <- row_index[1]
  }


  # Add row_id in hide_these_cols
  hide_these_cols <- c(hide_these_cols, "row_id")

  # Find indices of columns to hide (both specified columns and created
  # "row_id" column)
  hide_indices <- c(
    # Column indices to hide including row_id column index
    which(colnames(data) %in% hide_these_cols)
  )

  row_highlight <- FALSE
  if(!is.null(row_index) && (row_index %in% data$row_id)){
    row_highlight <- TRUE
  }



  # Prefixed DT options
  DT_opts <- list(
    # To style the header of the table
    headerCallback = DT::JS(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css({'background-color': '#ABE7F953', 'color': '#333'});",
      "}"
    ),
    keys = TRUE,
    dom = '<"top">lrt<"bottom">i',
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = 300,
    scroller = list(
      displayBuffer = 1.2  # Reduce buffer from default 9 to just 2
      # Using 1 is possible but may cause choppy scrolling
    ),
    scrollCollapse = FALSE,
    ordering = FALSE,
    # To show info based on visible rows (otherise it shows to and from same)
    infoCallback = DT::JS(
      "function(settings, start, end, max, total, pre){",
      "  // Calculate visible rows based on scroll position",
      "  var api = this.api();",
      "  var pageInfo = api.page.info();",
      "  var visibleRows = api.rows({page:'current'}).nodes().length;",
      "  var firstVisible = pageInfo.start + 1;",
      "  var lastVisible = Math.min(firstVisible + visibleRows - 1, total);",
      "  return 'Showing ' + firstVisible + ' to ' + lastVisible + ' of ' + total + ' rows';",
      "}"
    ),
    # For accessing this DT API
    initComplete =  DT::JS(
      "function(settings, json){",
      # Registers the API for the table globally
      paste0("window.",DT_api_js_var_name," = this.api();"),
      ifelse(
        row_highlight,
        paste0(
          "this.api().table().scroller.toPosition(",
          min(max(row_index - 5,0), max(data$row_id))
          ,
          ifelse(DT_animate_scroll, "",",false")
          ,");"),""),
      "}")

  )

  # Create datatable with hidden columns
  dt <- DT::datatable(
    data,
    options = c(
      DT_opts, # Prefixed options
      list(
        columnDefs = list(
          # To hide specified columns (0 based index)
          list(visible = FALSE, targets = hide_indices - 1),

          # Feature: dynamically configurable long cells truncation logic via paste0 injection
          list(
            targets = "_all",
            render = DT::JS(
              paste0(
                "function(data, type, row, meta) {",
                "  if (type === 'display' && typeof data === 'string') {",
                "    if (data.length <= ", truncate_length, ") return data;",
                "    var displayStr = data.substr(0, ", truncate_length, ") + '...';",
                "    var tooltipStr = data.substr(0, ", tooltip_length, ");",
                "    if (data.length > ", tooltip_length, ") tooltipStr += '...';",
                "    return '<span title=\"' + tooltipStr + '\">' + displayStr + '</span>';",
                "  }",
                "  return data;",
                "}"
              )
            )
          )
        )
      )),
    selection  = "single",
    escape     = FALSE,
    rownames   = FALSE,
    style      = "bootstrap",
    class      = "cell-border stripe",
    extensions = c("KeyTable", "Scroller")
  )

  # If row_index is provided and valid, highlight the specified row
  if(row_highlight) {

    # Apply background color to specific row only
    visible_cols <- setdiff(1:NCOL(data), hide_indices)
    dt <- DT::formatStyle(
      dt,
      columns = visible_cols,
      target = "row",
      backgroundColor = DT::styleEqual(row_index, bg_color),
      valueColumns = NCOL(data)
    )

    # If col_colors is provided and valid (has at least 1 row in
    # relevant_colors), apply text colors to specific columns in highlighted row
    # from col_colors.

    # First filter to only relevant columns
    relevant_colors <- dplyr::filter(
      col_colors,
      .data$colname %in% colnames(data),
      !(.data$colname %in% hide_these_cols))

    if(NROW(relevant_colors)>0) {
      # Apply text colors to specific columns

      color_specs <- purrr::pmap(
        relevant_colors,
        function(colname, color, ...) {
          col_idx <- which(colnames(data) == colname)
          list(col = col_idx, color = color)
        })

      # Use reduce to apply all the formatting operations
      dt <- purrr::reduce(
        color_specs,
        function(dt_acc, format_spec) {
          DT::formatStyle(
            dt_acc,
            columns = format_spec$col,
            target = "cell",
            color = DT::styleEqual(row_index, format_spec$color),
            fontWeight = DT::styleEqual(row_index, "bold"),
            fontSize = DT::styleEqual(row_index, "11px"),
            valueColumns = NCOL(data)
          )
        },
        .init = dt
      )
    }

  }

  return(dt)

}



shiny_util_attempted_collapsible_panel <- function(..., title, id = NULL, style = "default") {

  # Check if shinyBS is installed
  if (pkg_is_available("shinyBS")) {

    # Return the collapsible panel
    shinyBS::bsCollapse(
      id = id,
      shinyBS::bsCollapsePanel(
        title = title,
        style = style,
        ...
      )
    )

  } else {

    # Fallback: Return a styled div with the contents always visible
    shiny::div(
      style = "margin-top: 10px; margin-bottom: 10px; padding: 10px; border: 1px solid #e3e3e3; border-radius: 4px;",
      shiny::tags$strong(title, style = "display: block; margin-bottom: 10px; color: #333;"),
      ...
    )

  }
}

shiny_util_create_separated_palette <- function(colors, randomize = TRUE, seed = NULL, return_palette = TRUE) {

  # Set seed if provided for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- length(colors)

  # Edge case: If 2 or fewer colors, interleaving isn't necessary
  if (n <= 2) {
    if (return_palette) return(grDevices::colorRampPalette(colors))
    return(colors)
  }

  # 1. Convert to HSV and extract the Hue row ("h")
  hues <- grDevices::rgb2hsv(grDevices::col2rgb(colors))["h", ]

  # 2. Sort the colors by Hue
  sorted_cols <- colors[order(hues)]

  # 3. Interleave the first half with the second half
  half <- ceiling(n / 2)
  idx <- c(rbind(1:half, (half + 1):n))
  idx <- idx[idx <= n] # Safety catch for odd-length vectors
  well_spaced_cols <- sorted_cols[idx]

  # 4. Introduce random shift
  if (randomize) {
    shift <- sample(1:n, 1)
    if (shift > 1) {
      well_spaced_cols <- well_spaced_cols[c(shift:n, 1:(shift - 1))]
    }
  }

  # 5. Return result based on user preference
  if (return_palette) {
    return(grDevices::colorRampPalette(well_spaced_cols))
  } else {
    return(well_spaced_cols)
  }
}

# This is not fully implemented. Later it may be used for all msgs.
shiny_util_msg <- function(){
  list(
    fine_col_sel = "Show only columns common to both selections above. Note: If no columns are selected here, this filter is ignored. To reset, reselect or change the render type."
  )
}
