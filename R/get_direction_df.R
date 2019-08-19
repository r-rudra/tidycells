

#' Get Directional Orientation for Attributes corresponding to a given data
#' group
#'
#' @param dp single data group boundary information
#' @param datt attribute data (including attribute group id)
#' @details Used internally by get_data_block_information function
#' @keywords internal
#' @return Directional Orientation for Attributes
#'
get_direction_df <- function(dp, datt, allow_inside = FALSE) {
  directions <- list()

  directions$N <- datt %>%
    filter(
      row < dp$r_min,
      col >= dp$c_min,
      col <= dp$c_max
    ) %>%
    mutate(dist = (dp$r_min - row)) %>%
    mutate(
      direction = "N",
      direction_group = "NS",
      data_gid = dp$gid
    )

  directions$S <- datt %>%
    filter(
      row > dp$r_max,
      col >= dp$c_min,
      col <= dp$c_max
    ) %>%
    mutate(dist = (row - dp$r_max)) %>%
    mutate(
      direction = "S",
      direction_group = "NS",
      data_gid = dp$gid
    )

  directions$W <- datt %>%
    filter(
      col < dp$c_min,
      row >= dp$r_min,
      row <= dp$r_max
    ) %>%
    mutate(dist = (dp$c_min - col)) %>%
    mutate(
      direction = "W",
      direction_group = "WE",
      data_gid = dp$gid
    )

  directions$E <- datt %>%
    filter(
      col > dp$c_max,
      row >= dp$r_min,
      row <= dp$r_max
    ) %>%
    mutate(dist = (col - dp$c_max)) %>%
    mutate(
      direction = "E",
      direction_group = "WE",
      data_gid = dp$gid
    )

  # corner directions

  directions$NW <- datt %>%
    filter(
      row < dp$r_min,
      col < dp$c_min
    ) %>%
    mutate(dist = sqrt((dp$r_min - row)^2 + (dp$c_min - col)^2)) %>%
    mutate(
      direction = "NW",
      direction_group = "corner",
      data_gid = dp$gid
    )

  directions$NE <- datt %>%
    filter(
      row < dp$r_min,
      col > dp$c_max
    ) %>%
    mutate(dist = sqrt((dp$r_min - row)^2 + (dp$c_max - col)^2)) %>%
    mutate(
      direction = "NE",
      direction_group = "corner",
      data_gid = dp$gid
    )

  directions$SE <- datt %>%
    filter(
      row > dp$r_max,
      col > dp$c_max
    ) %>%
    mutate(dist = sqrt((dp$r_max - row)^2 + (dp$c_max - col)^2)) %>%
    mutate(
      direction = "SE",
      direction_group = "corner",
      data_gid = dp$gid
    )

  directions$SW <- datt %>%
    filter(
      row > dp$r_max,
      col < dp$c_min
    ) %>%
    mutate(dist = sqrt((dp$r_max - row)^2 + (dp$c_min - col)^2)) %>%
    mutate(
      direction = "SW",
      direction_group = "corner",
      data_gid = dp$gid
    )

  if (allow_inside) {
    directions$INSIDE <- datt %>%
      filter(
        row >= dp$r_min,
        row <= dp$r_max,
        col >= dp$c_min,
        col <= dp$c_max
      ) %>%
      mutate(dist = 0) %>%
      mutate(
        direction = "INSIDE",
        direction_group = "inside",
        data_gid = dp$gid
      )
  }


  direction_df <- directions %>% bind_rows()

  direction_df
}
