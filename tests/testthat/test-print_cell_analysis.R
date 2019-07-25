test_that("print cell-analysis works works", {
  x <- head(iris)
  cd <- x %>%
    as_cell_df(take_col_names = TRUE) %>%
    numeric_values_classifier()
  ca <- analyze_cells(cd)

  # following should run without error or warning
  wnow <- options("warn")[[1]]
  options(warn = 2)

  plot(ca)
  plot(ca, adaptive_txt_size = FALSE, no_txt = FALSE)
  plot(ca,
    prior_plot = plot(cd, no_plot = TRUE, adaptive_txt_size = FALSE)
  )
  plot(ca,
    prior_plot = plot(cd, no_plot = TRUE, adaptive_txt_size = FALSE),
    gids = 0
  )
  plot(ca,
    zoom_selected_gids = TRUE,
    dat_att_boundary = TRUE, block_boundary = FALSE
  )
  plot(ca, plot_issues = TRUE)

  # also the traceback plot works
  cell_composition_traceback(ca, trace_row = 1)
  cell_composition_traceback(ca, trace_row = 10)

  options(warn = wnow)

  expect_true(inherits(plot(ca, no_plot = TRUE), "ggplot"))
})
