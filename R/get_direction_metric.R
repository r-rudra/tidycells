

#' get direction metric
#'
#' @param d1 part of d_part with data_gid
#' @param a1 part of d_part with attr_gid
#' @param direction direction name (compatible with `unpivotr`)
#' should be one of [`get_unpivotr_direction_names`][get_unpivotr_direction_names()]
#'
#' @details Used internally by [`get_direction`][get_direction()] function
#' @keywords internal
#' @return a scaled fraction denoting coverage (1 means full coverage) for the supplied direction.
#'
get_direction_metric <- function(d1, a1, direction) {
  l1 <- try(get_direction_metric_part_raw(d1, a1, direction), silent = TRUE)

  if (inherits(l1, "try-error")) l1 <- 0
  if (length(l1) != 1) l1 <- 0
  if (is.na(l1)) l1 <- 0

  l1 / nrow(d1)
}

get_direction_metric_part_raw <- function(d1, a1, direction) {
  d1 %>%
    enhead(a1, direction) %>%
    filter(!is.na(attr_gid)) %>%
    pull(attr_gid) %>%
    length()
}
