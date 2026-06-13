

#' Attach header meta-data to data by specified direction
#'
#' @param dat Data frame with data cells (must have 'row', 'col')
#' @param hdr Data frame with header cells (must have 'row' or 'col' as relevant)
#' @param direction Header direction, one of:
#'   "v" (vertical), "h" (horizontal),
#'   "vl", "vr", "vm" (vertical: left, right, mid),
#'   "hu", "hd", "hm" (horizontal: up, down, mid),
#'   "direct" (universal)
#' @return Data frame with attached header columns (with "_hdr" suffix)
#' @keywords internal
attach_header <- function(dat, hdr, direction = c("v","h","vl","vr","vm","hu","hd","hm","direct","nothing")) {
  # If hdr is empty, return dat unchanged
  if (NROW(hdr) == 0) return(dat)

  # If only one header cell, assign its columns to all data cells irrespective
  # of direction (except "nothing")
  if (NROW(hdr) == 1 && direction != "nothing") {
    new_header_cols <- setdiff(colnames(hdr), c("row", "col"))
    dat[new_header_cols] <- hdr[
      rep(1, NROW(dat)),
      new_header_cols, drop = FALSE]
    return(dat)
  }
  # For multi-cell headers, use attach_header_base to handle different
  # directions
  attach_header_base(dat, hdr, direction)
}


# Wrapper function to attach header
attach_header_base <- function(dat, hdr,
                               direction = c("v","h","vl","vr","vm","hu","hd","hm","direct","nothing")) {
  direction <- match.arg(direction)
  switch(direction,
         v  = attach_header_simple(dat, hdr, by = "col"),
         h  = attach_header_simple(dat, hdr, by = "row"),
         vl = attach_header_segment(dat, hdr, axis = "col", cuts_fun = attach_header_util_header_data_dir_cuts, direction = 1),
         vr = attach_header_segment(dat, hdr, axis = "col", cuts_fun = attach_header_util_header_data_dir_cuts, direction = -1),
         vm = attach_header_segment(dat, hdr, axis = "col", cuts_fun = attach_header_util_header_data_dir_cuts_mid),
         hu = attach_header_segment(dat, hdr, axis = "row", cuts_fun = attach_header_util_header_data_dir_cuts, direction = 1),
         hd = attach_header_segment(dat, hdr, axis = "row", cuts_fun = attach_header_util_header_data_dir_cuts, direction = -1),
         hm = attach_header_segment(dat, hdr, axis = "row", cuts_fun = attach_header_util_header_data_dir_cuts_mid),
         direct = attach_header_direct(dat, hdr),
         # Do nothing
         nothing = dat
  )
}

# Simple (1:1) join: vertical or horizontal
attach_header_simple <- function(dat, hdr, by) {
  hdr[[setdiff(c("row", "col"), by)]] <- NULL
  dplyr::inner_join(dat, hdr, by = by, suffix = c("", "_hdr"))
}

# Join by segmenting along axis (row or col) using header cuts
attach_header_segment <- function(dat, hdr, axis, cuts_fun, direction = NULL) {
  # axis: "row" or "col"
  hdr_jk <- hdr[[axis]]
  hdr$jk <- hdr_jk
  hdr$row <- NULL
  hdr$col <- NULL
  # choose x: dat$row or dat$col
  dat <- dat %>%
    dplyr::mutate(jk = if(is.null(direction)) cuts_fun(.data[[axis]], sort(unique(hdr_jk)))
                  else cuts_fun(.data[[axis]], sort(unique(hdr_jk)), direction = direction))
  out <- dplyr::inner_join(dat, hdr, by = "jk", suffix = c("", "_hdr"))
  out$jk <- NULL
  out
}

# Universal direct join: attach all headers to all data
attach_header_direct <- function(dat, hdr) {
  hdr$jk <- 1
  hdr$row <- NULL
  hdr$col <- NULL
  dat <- dplyr::mutate(dat, jk = 1)
  out <- dplyr::inner_join(dat, hdr, by = "jk", suffix = c("", "_hdr"))
  out$jk <- NULL
  out
}

# Find header segment for each value (directional)
attach_header_util_header_data_dir_cuts <- function(x, hdr, direction = 1) {
  # direction: +1 (left/up), -1 (right/down)
  # hdr: sorted increasingly
  if(direction < 0) {
    hdr <- rev(-hdr)
    x <- -x
  }
  fi <- findInterval(x, hdr)
  fi[fi == 0] <- NA
  sign(direction) * hdr[fi]
}

# Find header segment for each value (midpoint splits)
attach_header_util_header_data_dir_cuts_mid <- function(x, hdr) {
  # hdr: sorted increasingly
  hdr_mids <- (hdr[-1] + hdr[-length(hdr)]) / 2
  hdr_mids <- c(-Inf, hdr_mids)
  fi <- findInterval(x, hdr_mids)
  fi[fi == 0] <- NA
  hdr[fi]
}
