

#' Compose a Cell Analysis to a tidy form
#'
#' @description After [`analyze_cells`][analyze_cells()] carried out, you may like to use this function in order to
#' stitch the cells together as per the analyzed results, to form a meaningful structural representation (like tidy format).
#'
#' @param ca a cell_analysis to process
#' @param post_process logical scalar. If disabled a list will be returned without performing post-processing. (Default `TRUE`)
#' @param attr_sep a character string to separate the attributes. (Default is `<space>::<space>`)
#' @param discard_raw_cols logical scalar. If enabled only main processed columns will be returned. (Default `FALSE`)
#' @param print_attribute_overview print the overview of the attributes (4 distinct values from each attribute of each block)
#' @param silent whether to suppress warning message on compose failure (Default `FALSE`)
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
compose_cells <- function(ca, post_process = TRUE,
                          attr_sep = " :: ",
                          discard_raw_cols = FALSE,
                          print_attribute_overview = FALSE,
                          silent = FALSE) {
  compose_cells_raw(
    ca = ca,
    post_process = post_process,
    attr_sep = attr_sep,
    discard_raw_cols = discard_raw_cols,
    print_col_info = print_attribute_overview,
    silent = silent
  )
}

compose_cells_raw <- function(ca, post_process = TRUE, attr_sep = " :: ",
                              discard_raw_cols = FALSE,
                              trace_it_back = FALSE,
                              details = FALSE,
                              print_col_info = FALSE,
                              silent = FALSE,
                              ask_user = TRUE) {
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

  dcomp00 <- dam %>%
    group_by(data_gid) %>%
    group_split() %>%
    map(~ .x %>%
      group_by(attr_gid, direction, attr_gid_split) %>%
      group_split())

  dcomp0 <- dcomp00 %>%
    map(~ .x %>%
      # this try should be removed if unpivotr::enhead is internalized
      # or similar behaving fucntions is developed.
      map(~ {
        e <- try(stitch_direction(.x, ca$cell_df, trace_it = trace_it_back), silent = TRUE)
        .ok <- !inherits(e, "try-error")
        .d <- NULL
        if (!.ok) .d <- .x
        list(ok = .ok, out = e, dat = .d)
      }))

  chk0 <- dcomp0 %>%
    map_lgl(~ .x %>%
      map_lgl(~ !.x$ok) %>%
      any()) %>%
    any()

  if (chk0) {
    if (!silent) {
      # Need to show user what has been missed
      warn(paste0(
        "Some attributes (possibly minor only) failed to compose.",
        "\nCheck whether output is as expected.",
        "\nYou can disable this by setting silent = TRUE."
      ))
      if (interactive() & ask_user) {
        user_res <- rstudioapi_ask(
          title = "Some attributes failed to compose.",
          message = "Do you want to return back the failed analysis part? (yes/no)",
          default = "yes",
          ok = "Yes", cancel = "No",
          is_question = TRUE
        )

        if (identical(user_res, TRUE)) {
          user_res <- "yes"
        }

        if (user_res == "yes") {
          # return failed analysis part for observing
          patched_ca <- ca

          dp0 <- dcomp0 %>% map_df(~ .x %>%
            map_lgl(~ !.x$ok) %>%
            .x[.] %>%
            map_df(~ .x$dat))
          patched_ca$details$data_attr_map_raw <- unique(dp0[colnames(patched_ca$details$data_attr_map_raw)])

          warn(paste0(
            "Failed portion of Cell-Analysis is returned",
            "\nIn the plots you should see texts, only in failed attributes."
          ))

          return(patched_ca)
        }
      }
    }
  }

  dcomp0 <- dcomp0 %>% map(~ .x %>%
    map_lgl(~ .x$ok) %>%
    .x[.] %>%
    map(~ .x$out))

  chk1 <- dcomp0 %>%
    map_int(length) %>%
    sum()

  if (chk1 > 0) {
    dcomp <- dcomp0 %>%
      map(~ reduce(.x, fj,
        join_by = c("row", "col", "value", "data_block"),
        sallow_join = TRUE, sep = attr_sep
      ))
  } else {
    abort("Failed to compose")
  }


  if (print_col_info) {
    dlinf <- dcomp %>% map(get_all_col_representative, cut_th = 4, lower_it = FALSE)

    dlinfc <- dlinf %>% map(~ .x %>% purrr::imap_chr(~ paste0("  ", cli_bb(.y), "\n     ", paste0(cli_g(.x), collapse = ", "))))
    names(dlinfc) <- paste0("data_block = ", seq_along(dlinfc))

    xmsg <- dlinfc %>%
      purrr::imap_chr(~ paste0(cli_br(.y), "\n", paste0(.x, collapse = "\n"))) %>%
      paste0(collapse = "\n")

    cat(xmsg)
  }

  if (!post_process) {
    return(invisible(dcomp))
  }

  compose_cells_raw_post_process(dcomp, details = details, discard_raw_cols = discard_raw_cols, attr_sep = attr_sep)
}

compose_cells_raw_post_process <- function(dcomp, details = FALSE, discard_raw_cols = FALSE, attr_sep = " :: ") {
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
  class(dcomp_r) <- c("rc_df", class(dcomp_r))

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
