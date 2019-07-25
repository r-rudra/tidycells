

#' Compose a Cell Analysis to a tidy form
#'
#' @param ca a cell_analysis to process
#' @param post_process logical scalar. If disabled a list will be returned without performing post-processing. (Default `TRUE`)
#' @param attr_sep a character string to separate the attributes. (Default is `<space>::<space>`)
#' @param discard_raw_cols logical scalar. By default `FALSE` if enabled only main processed columns will be returned.
#'
#' @return a data.frame (as tibble) in tidy form.
#' @export
#'
#' @examples
#' cd <- 1:(9) %>%
#'   matrix(nrow = 3) %>%
#'   as_cell_df()
#' cd <- sample_based_classifier(cd, attribute_sample = "1")
#' cd <- cd %>% dplyr::filter(value != "1")
#' ca <- analyze_cells(cd)
#'
#' compose_cells(ca)
compose_cells <- function(ca, post_process = TRUE, attr_sep = " :: ", discard_raw_cols = FALSE) {
  compose_cells_raw(
    ca = ca,
    post_process = post_process,
    attr_sep = attr_sep,
    discard_raw_cols = discard_raw_cols
  )
}

compose_cells_raw <- function(ca, post_process = TRUE, attr_sep = " :: ",
                              discard_raw_cols = FALSE,
                              trace_it_back = FALSE,
                              details = FALSE) {
  if (!inherits(ca, "cell_analysis")) {
    abort("A 'Cell Analysis' expected.")
  }

  dam <- ca$details$data_attr_map_raw

  dam <- dam %>%
    group_by(data_gid, direction_basic, direction_group) %>%
    mutate(dist_order = dist %>% as.factor() %>% as.integer()) %>%
    ungroup()

  dam <- dam %>%
    group_by(data_gid, attr_gid) %>%
    mutate(attr_gid_split_order = attr_gid_split %>% as.factor() %>% as.integer()) %>%
    ungroup()

  fj_this <- function(x, y) {
    fj(x, y,
      join_by = c("row", "col", "value", "data_block"),
      sallow_join = TRUE, sep = attr_sep
    )
  }

  dcomp <- dam %>%
    group_by(data_gid) %>%
    group_split() %>%
    map(~ .x %>%
      group_by(attr_gid, direction, attr_gid_split) %>%
      group_split() %>%
      map(~ stitch_direction(.x, ca$cell_df, trace_it = trace_it_back)) %>%
      reduce(fj_this))

  if (!post_process) {
    return(invisible(dcomp))
  }

  cns <- dcomp %>%
    map(colnames) %>%
    unlist() %>%
    unique()
  cns_trace <- cns[stringr::str_detect(cns, "cellAddress_")]
  cns <- cns %>% setdiff(cns_trace)
  cns_base <- c("row", "col", "data_block", "value")
  cns <- cns %>% setdiff(cns_base)

  cns_d <- tibble(cname = cns, cn = cns) %>%
    tidyr::separate(cn, into = c("ag", "rc", "dir", "ad", "d"))


  cns_d <- cns_d %>%
    # anticlockwise
    mutate(dir_n = recode(dir,
      top = 1,
      topLeft = 2,
      left = 3,
      bottomLeft = 4,
      bottom = 5,
      bottomRight = 6,
      right = 7,
      topRight = 8
    )) %>%
    mutate(rc_n = recode(rc,
      row = 1,
      col = 2,
      corner = 3
    )) %>%
    mutate(cname_ord = paste(rc_n, dir_n, ad, d, sep = "_"))



  dcomp_r <- dcomp %>%
    map(~ refine_cols(.x, cn_df = cns_d, sep = attr_sep)) %>%
    bind_rows()

  #  add rc_df class
  class(dcomp_r) <- c(class(dcomp_r), "rc_df") %>% unique()

  this_cols <- colnames(dcomp_r)
  f_cols <- c("row", "col", "data_block", "value")
  this_cols <- this_cols %>% setdiff(f_cols)
  nm_cols <- this_cols[stringr::str_detect(this_cols, "row|col|corner")]
  m_cols <- this_cols %>% setdiff(nm_cols)

  if (details) {
    lo <- list(raw_data = dcomp_r, must_cols = f_cols, major_col = m_cols, minor_col = nm_cols)
    return(lo)
  }
  if (discard_raw_cols) {
    dcomp_r[c(f_cols, m_cols)]
  } else {
    dcomp_r[c(f_cols, m_cols, nm_cols)]
  }
}
