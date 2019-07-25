

#' get unpivotr direction names
#'
#' @details Kept for compatibility. Used internally by get_direction function.
#' @keywords internal
#' @return directions as used in unpivotr package with directional grouping
#'
get_unpivotr_direction_names <- function() {

  # direction order
  # "N" "NNW" "NNE" "ABOVE"
  # "W" "WNW" "WSW" "LEFT"
  # "S" "SSW" "SSE" "BELOW"
  # "E" "ENE" "ESE" "RIGHT"

  list(
    N = c("N", "NNW", "NNE", "ABOVE"),
    W = c("W", "WNW", "WSW", "LEFT"),
    S = c("S", "SSW", "SSE", "BELOW"),
    E = c("E", "ENE", "ESE", "RIGHT"),
    NE = "NNE",
    NW = "NNW",
    SE = "SSE",
    SW = "SSW"
  )
}
