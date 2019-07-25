

#' @export
print.cell_df <- function(x, ...) {
  d <- x

  msg <- c(paste0(
    "A ",
    cli_bb("Cell Data Frame")
  ))

  if (hasName(d, "type")) {
    msg <- c(
      msg,
      paste0(
        "With ",
        cli_b("Value/Attribute Classification")
      )
    )
  }

  msg <- c(
    msg,
    paste0(
      cli_bs, "To see cell stats, call ",
      cli_b("summary()")
    )
  )
  msg <- c(
    msg,
    paste0(
      cli_bs, "To see cell structure, call ",
      cli_b("plot()")
    )
  )

  msg <- c(
    msg,
    paste0(cli_bs, "Content:")
  )

  if (length(list(...))) {
    t_msg <- d %>%
      tibble::as_tibble() %>%
      format(...)
  } else {
    t_msg <- d %>%
      tibble::as_tibble() %>%
      format(n = 4, width = 40L)
  }

  msg <- c(msg, t_msg)

  cat(paste0(paste0(msg, collapse = "\n"), "\n"))
}


#' @export
summary.cell_df <- function(object, ..., no_print = FALSE) {
  d <- object

  ds <- d %>%
    summarise(
      mnr = min(row), mxr = max(row),
      mnc = min(col), mxc = max(col),
      nch = sum(data_type == "character"),
      nnum = sum(data_type == "numeric")
    )
  ds$nrow <- nrow(d)

  msg <- c(paste0(
    "A ", cli_bb("Cell Data Frame"),
    " with ", cli_br(ds$nrow), " values"
  ))

  nr <- (ds$mxr - ds$mnr + 1)
  nc <- (ds$mxc - ds$mnc + 1)

  msg <- c(
    msg,
    paste0(cli_bs, cli_bb("Dimension: "), cli_br(nr), " x ", cli_br(nc))
  )

  msg <- c(
    msg,
    paste0(cli_bs, cli_bb("Content: "), cli_br(ds$nch), " characters and ", cli_br(ds$nnum), " numbers")
  )

  ddensity <- nrow(d) / (nr * nc)

  msg <- c(
    msg,
    paste0(cli_bs, cli_bb("Density: "), cli_br(paste0(round(ddensity * 100, 1), "%")))
  )

  if (hasName(d, "type")) {
    ds2 <- d %>% summarise(
      nval = sum(type == "value"),
      natt = sum(type == "attribute"),
      nemp = sum(type == "empty")
    )

    msg <- c(
      msg,
      paste0(cli_bs, cli_bb("Types: "), cli_br(ds2$nval), " values, ", cli_br(ds2$natt), " attributes and ", cli_br(ds2$nemp), " empty cells")
    )
  }

  if (!no_print) {
    cat(paste0(paste0(msg, collapse = "\n"), "\n"))
  }

  return(invisible(ds))
}

#' @export
plot.cell_df <- function(x, ...,
                         fill,
                         no_fill = FALSE,
                         adaptive_txt_size = TRUE, txt_size = 4, txt_alpha = 1, no_txt = FALSE, no_plot = FALSE) {
  d <- x

  if (missing(fill)) {
    if (hasName(d, "type")) {
      fill <- "type"
    } else {
      fill <- "data_type"
    }
  }

  if (!(fill %in% c("data_type", "type"))) {
    abort("fill should be either of (data_type, type)")
  }

  if (!hasName(d, fill)) {
    abort(paste0(fill, " is not present in supplied cell_df"))
  }



  if (adaptive_txt_size) {
    d <- d %>%
      mutate(nc = nchar(value), txt_size_ = (min(nc) + 10^-10) / (nc + 10^-10) * txt_size) %>%
      select(-nc) %>%
      rename(txt_size = txt_size_)
  }

  if (no_fill) {
    g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, label = value))

    g <- g +
      ggplot2::geom_tile(color = "#00000046", alpha = 0.1, na.rm = TRUE, width = 1, height = 1)
  } else {
    if (fill == "data_type") {
      g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, fill = data_type, label = value)) +
        ggplot2::labs(fill = "Data Type")
    } else {
      g <- ggplot2::ggplot(d, ggplot2::aes(col, -row, fill = type, label = value)) +
        ggplot2::labs(fill = "Type")
    }

    g <- g +
      ggplot2::geom_tile(color = "#00000046", na.rm = TRUE, width = 1, height = 1) +
      # kept for all potential plots
      ggplot2::scale_fill_manual(values = c(
        attribute = "#F8766D", value = "#00BFC4", empty = "#A3A3A3A3",
        character = "#F8766D", numeric = "#00BFC4",
        major_attr = "#F8766D", data = "#00BFC4", minor_attr = "#FACE6EE9"
      ))
  }




  g <- g +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if (!no_txt) {
    if (adaptive_txt_size) {
      g <- g + ggplot2::geom_text(ggplot2::aes(size = txt_size), alpha = txt_alpha, na.rm = TRUE) +
        ggplot2::scale_size_continuous(
          range = d$txt_size %>% range(),
          guide = FALSE
        )
    } else {
      g <- g + ggplot2::geom_text(size = txt_size, alpha = txt_alpha, na.rm = TRUE)
    }
  }

  if (!no_plot) {
    graphics::plot(g, ...)
  }

  return(invisible(g))
}





#' @export
as.matrix.cell_df <- function(x, ...) {
  x <- x %>% select(row, col, data_type, value)
  validate_cells(x)
  d <- x
  d <- unset_cell_df_class(d)
  m <- d %>%
    select(-data_type) %>%
    tidyr::spread(col, value) %>%
    select(-row) %>%
    as.matrix()
  colnames(m) <- NULL
  row.names(m) <- NULL
  m
}

#' @export
as.data.frame.cell_df <- function(x, row.names, optional, ...) {
  d <- x
  do <- d %>% as.matrix.cell_df()
  as.data.frame(do, stringsAsFactors = FALSE)
}
