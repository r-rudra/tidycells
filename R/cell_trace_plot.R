

#' Display static composition traceback plot
#'
#' @param ca A cell analysis
#' @param dc (optional) a composed tidy data.frame
#' @param trace_row the row number to trace. (Default 1)
#' @keywords internal
#' @export
cell_composition_traceback <- function(ca, dc, trace_row = 1L) {
  cell_trace_plot(dc, trace_row, ca)
}


cell_trace_plot <- function(dc, trace_row, ca, prior_plot, prior_ca_plot) {
  dc <- attach_trace_info(ca, dc)

  if (!missing(trace_row)) {
    if (length(trace_row) > 0) {
      if (missing(prior_plot)) {
        if (missing(prior_ca_plot)) {
          prior_plot <- graphics::plot(ca$cell_df, no_plot = TRUE)
        }
      }

      d0 <- dc[trace_row, ]

      connected_cells <- d0[stringr::str_detect(colnames(d0), "cellAddress")]
      connected_cells <- connected_cells %>%
        map_lgl(~ !is.na(.x)) %>%
        connected_cells[.]
      connected_cells <- connected_cells %>%
        imap_dfr(~ .x %>%
          stringr::str_split(" :: ") %>%
          reduce(c) %>%
          stringr::str_split("_") %>%
          map(~ .x %>%
            as.integer() %>%
            t()) %>%
          reduce(rbind) %>%
          as.data.frame() %>%
          mutate(cn_id_raw = .y))
      connected_cells <- connected_cells %>%
        mutate(
          cn_id = cn_id_raw %>%
            stringr::str_remove("cellAddress_"),
          cn = cn_id %>%
            stringr::str_extract("major|minor")
        )
      colnames(connected_cells)[1:2] <- c("row", "col")

      connected_cells <- connected_cells %>%
        mutate(cn_id_ = cn_id) %>%
        tidyr::separate(cn_id_, into = c(rep(NA, 3), "d1", "d2")) %>%
        mutate(dty = (as.numeric(d1) + as.numeric(d2)) %% 2) %>%
        select(-d1, -d2)

      connected_cells <- connected_cells %>% mutate(r = d0$row, c = d0$col)


      if (missing(prior_ca_plot)) {
        prior_ca_plot <- graphics::plot(ca,
          prior_plot = prior_plot,
          gids = d0$data_block,
          zoom_selected_gids = TRUE,
          block_boundary = FALSE,
          direction_text = FALSE,
          no_plot = TRUE
        )
      }
      this_plot <- prior_ca_plot
      this_plot <- this_plot + ggplot2::guides(fill = FALSE)

      this_plot <- this_plot +
        ggplot2::geom_tile(
          data = d0,
          ggplot2::aes(col, -row),
          color = "red", lwd = 1, inherit.aes = FALSE,
          alpha = 0,
          width = 1, height = 1, na.rm = TRUE
        )

      if (length(unique(connected_cells$dty)) == 1) {
        this_plot <- this_plot +
          ggplot2::geom_curve(
            data = connected_cells,
            ggplot2::aes(
              x = c, y = -r,
              xend = col, yend = -row,
              color = cn
            ),
            inherit.aes = FALSE, curvature = -0.1,
            arrow = ggplot2::arrow(), lwd = 1, na.rm = TRUE
          )
      } else {
        this_plot <- this_plot +
          ggplot2::geom_curve(
            data = connected_cells %>% filter(dty == 1),
            ggplot2::aes(
              x = c, y = -r,
              xend = col, yend = -row,
              color = cn
            ),
            inherit.aes = FALSE, curvature = -0.1,
            arrow = ggplot2::arrow(), lwd = 1, lty = "longdash", na.rm = TRUE
          ) +
          ggplot2::geom_curve(
            data = connected_cells %>% filter(dty != 1),
            ggplot2::aes(
              x = c, y = -r,
              xend = col, yend = -row,
              color = cn
            ),
            inherit.aes = FALSE, curvature = 0.1,
            arrow = ggplot2::arrow(), lwd = 1, na.rm = TRUE
          )
      }


      this_plot <- this_plot +
        ggplot2::guides(color = FALSE) +
        ggplot2::scale_color_manual(values = c(major = "#B5525288", minor = "#4156A888"))


      return(this_plot)
    }
  }
}
