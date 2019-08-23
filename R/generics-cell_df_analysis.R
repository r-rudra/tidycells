
#' @export
print.cell_analysis <- function(x, ...) {
  nblocks <- x$cells %>%
    summarise(n_distinct(gid)) %>%
    pull(1)
  dachek_msg <- x$details$definiteness_checks %>%
    map(~ attr(.x, "msg")) %>%
    unlist() %>%
    paste0(collapse = "\n")
  if (nchar(dachek_msg) == 0) {
    dachek_msg <- NULL
  } else {
    dachek_msg <- paste0(cli_bs(), cli_br("Potential Issues:"), "\n", cli_b(dachek_msg))
  }
  msg <- paste0(
    cli_bb("Cell DF Analysis:"),
    "\n", paste0(cli_b("Total blocks: "), nblocks),
    "\n", dachek_msg,
    "\n"
  )
  cat(msg)
}


#' @export
plot.cell_analysis <- function(x, ...,
                               # parameters of plo2t.cell_df
                               adaptive_txt_size = TRUE, txt_size = 2, no_txt = FALSE,
                               # or directly prior plot can be passed
                               prior_plot,
                               # parameters of this function
                               gids,
                               zoom_selected_gids = FALSE,
                               plot_cell_base_color = FALSE,
                               block_boundary = TRUE,
                               direction_text = TRUE,
                               direction_text_on_all = FALSE,
                               dat_att_boundary = FALSE,
                               plot_issues = FALSE,
                               no_plot = FALSE) {
  if (missing(prior_plot)) {
    g_base <- plot.cell_df(x$cell_df,
      no_fill = TRUE,
      adaptive_txt_size = adaptive_txt_size,
      txt_size = txt_size,
      no_txt = no_txt,
      txt_alpha = 0.08,
      no_plot = TRUE
    )
  } else {
    g_base <- prior_plot
  }

  all_gids <- unique(x$cells$natural_gid)
  if (missing(gids)) {
    gids <- all_gids
  }


  if (!plot_cell_base_color) {
    aes_old <- g_base$mapping %>% map_chr(rlang::as_label)

    if (!is.null(g_base$labels$fill) | !is.na(aes_old["fill"])) {
      # remove the fill asthetic manually
      glsel <- g_base$layers %>% map_lgl(~ "GeomTile" %in% class(.x$geom))
      g_base$layers <- g_base$layers[!glsel]
      g_base$mapping <- g_base$mapping[-which(names(aes_old) == "fill")]
      g_base <- g_base +
        ggplot2::geom_tile(color = "#00000046", alpha = 0.1, na.rm = TRUE, width = 1, height = 1)
    }
  }

  g <- g_base +
    ggplot2::geom_tile(
      data = x$cells %>% filter(natural_gid %in% gids),
      ggplot2::aes(col, -row, fill = cell_group_type),
      inherit.aes = FALSE, alpha = 0.7, na.rm = TRUE,
      width = 1, height = 1
    ) +
    ggplot2::labs(fill = "Cell Type")


  if (block_boundary) {
    dat_att_boundary <- FALSE
    g <- g +
      ggplot2::geom_rect(
        data = x$sections %>% filter(natural_gid %in% gids),
        ggplot2::aes(
          xmin = c_min, xmax = c_max, ymin = -r_min, ymax = -r_max,
          group = gid
        ),
        color = "red", inherit.aes = FALSE, alpha = 0.1, size = 1.5, lty = 2,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = x$sections %>% filter(natural_gid %in% gids),
        ggplot2::aes(
          x = (c_min + c_max) / 2, y = -(r_min + r_max) / 2,
          label = natural_gid
        ),
        inherit.aes = FALSE, color = "red", size = 8, na.rm = TRUE
      )
  }




  if (dat_att_boundary) {
    g <- g +
      ggplot2::geom_rect(
        data = x$details$data_details$group_id_boundary,
        ggplot2::aes(
          xmin = c_min, xmax = c_max, ymin = -r_min, ymax = -r_max,
          group = gid
        ),
        color = "red", inherit.aes = FALSE, alpha = 0.1, size = 1.5, lty = 2,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = x$details$data_details$group_id_boundary,
        ggplot2::aes(x = (c_min + c_max) / 2, y = -(r_min + r_max) / 2, label = gid %>% as.factor() %>% as.numeric()),
        inherit.aes = FALSE, color = "red", size = 4,
        na.rm = TRUE
      ) +
      ggplot2::geom_rect(
        data = x$details$attr_details$group_id_boundary,
        ggplot2::aes(
          xmin = c_min, xmax = c_max, ymin = -r_min, ymax = -r_max,
          group = gid
        ),
        color = "blue", inherit.aes = FALSE, alpha = 0.1, size = 1.5, lty = 2,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = x$details$attr_details$group_id_boundary,
        ggplot2::aes(x = (c_min + c_max) / 2, y = -(r_min + r_max) / 2, label = gid %>% as.factor() %>% as.numeric()),
        inherit.aes = FALSE, color = "blue", size = 4, na.rm = TRUE
      )
  }


  if (direction_text) {
    d_gids <- gids

    if (direction_text_on_all) {
      d_gids <- all_gids
    }

    g <- g +
      ggplot2::geom_text(
        data = x$details$data_attr_map_raw %>%
          filter(natural_gid %in% d_gids) %>%
          distinct(direction, row = row_a, col = col_a),
        ggplot2::aes(col, -row,
          label = direction
        ),
        inherit.aes = FALSE, color = "black", size = 3, na.rm = TRUE
      )
  }

  if (zoom_selected_gids) {
    lims <- x$sections %>%
      filter(natural_gid %in% gids) %>%
      summarise(r_min = min(r_min), r_max = max(r_max), c_min = min(c_min), c_max = max(c_max))

    g <- g +
      ggplot2::scale_x_continuous(limits = c(lims$c_min - 0.5, lims$c_max + 0.5)) +
      ggplot2::scale_y_continuous(limits = c(-lims$r_max - 0.5, -lims$r_min + 0.5))
  }

  if (plot_issues) {
    issue_details <- get_definiteness_details(
      x$details$data_attr_map_raw,
      x$details$attr_details$group_id_boundary$gid %>%
        setdiff(x$details$attr_details$missed_blocks$gid)
    )
    if (nrow(issue_details$gid_att) > 0) {
      sel_cells <- x$details$attr_details$group_id_map %>%
        filter(gid %in% issue_details$gid_att$attr_gid) %>%
        distinct(row, col, gid) %>%
        mutate(gid = gid %>% as.factor() %>% as.numeric())

      sel_cells_mid <- sel_cells %>%
        group_by(gid) %>%
        summarise(row = mean(row), col = mean(col)) %>%
        mutate(txt = paste0("A", gid))

      g <- g +
        ggplot2::geom_tile(
          data = sel_cells, ggplot2::aes(col, -row),
          color = "red", fill = "red", alpha = 0.7,
          inherit.aes = FALSE, na.rm = TRUE,
          width = 1, height = 1
        ) +
        ggplot2::geom_text(
          data = sel_cells_mid, ggplot2::aes(col, -row, label = txt),
          inherit.aes = FALSE, na.rm = TRUE
        )
    }

    if (nrow(issue_details$gid_data) > 0) {
      sel_cells <- x$details$data_details$group_id_map %>%
        filter(gid %in% issue_details$gid_data$data_gid) %>%
        distinct(row, col, gid) %>%
        mutate(gid = gid %>% as.factor() %>% as.numeric())

      sel_cells_mid <- sel_cells %>%
        group_by(gid) %>%
        summarise(row = mean(row), col = mean(col)) %>%
        mutate(txt = paste0("D", gid))

      g <- g +
        ggplot2::geom_tile(
          data = sel_cells, ggplot2::aes(col, -row),
          color = "orange", fill = "orange", alpha = 0.7,
          inherit.aes = FALSE, na.rm = TRUE,
          width = 1, height = 1
        ) +
        ggplot2::geom_text(
          data = sel_cells_mid, ggplot2::aes(col, -row, label = txt),
          inherit.aes = FALSE, na.rm = TRUE
        )
    }

    if (length(issue_details$all_gid_att) > 0) {
      sel_cells <- x$details$attr_details$group_id_map %>%
        filter(gid %in% issue_details$all_gid_att) %>%
        distinct(row, col, gid) %>%
        mutate(gid = gid %>% as.factor() %>% as.numeric())

      sel_cells_mid <- sel_cells %>%
        group_by(gid) %>%
        summarise(row = mean(row), col = mean(col)) %>%
        mutate(txt = paste0("M", gid))

      g <- g +
        ggplot2::geom_tile(
          data = sel_cells, ggplot2::aes(col, -row),
          color = "pink", fill = "pink", alpha = 0.7,
          inherit.aes = FALSE, na.rm = TRUE,
          width = 1, height = 1
        ) +
        ggplot2::geom_text(
          data = sel_cells_mid, ggplot2::aes(col, -row, label = txt),
          inherit.aes = FALSE, na.rm = TRUE
        )
    }
  }

  if (!no_plot) {
    graphics::plot(g, ...)
  }

  return(invisible(g))
}
