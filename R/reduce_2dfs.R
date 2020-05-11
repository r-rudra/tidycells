

get_connected_cols <- function(col_map_with_dist) {
  col_map_with_dist
  col_map <- tibble()
  repeat({
    if (nrow(col_map_with_dist) == 0) break()
    col_map_this <- col_map_with_dist %>% filter(dist == min(dist))
    if (nrow(col_map_this) == 0) break()
    col_map <- col_map %>% bind_rows(col_map_this)
    col_map_with_dist <- col_map_with_dist %>% filter(!(n1 %in% col_map$n1), !(n2 %in% col_map$n2))
  })
  col_map
}

reduce_2dfs <- function(dc1, dc2, combine_th = 0.6, rest_cols = Inf, retain_other_cols = FALSE) {
  colnames(dc1) <- stringr::str_replace_all(colnames(dc1), "uncollated_", "d1_old_uc_")
  colnames(dc1) <- stringr::str_replace_all(colnames(dc1), "collated_", "d1_old_c_")

  colnames(dc2) <- stringr::str_replace_all(colnames(dc2), "uncollated_", "d2_old_uc_")
  colnames(dc2) <- stringr::str_replace_all(colnames(dc2), "collated_", "d2_old_c_")


  cr1 <- get_all_col_representative(dc1)
  cr2 <- get_all_col_representative(dc2)

  # if either of cr1 or cr2 is empty simply rbind and return
  if (length(cr1) * length(cr2) == 0) {
    dcnew <- dc1 %>% bind_rows(dc2)
    return(dcnew)
  }

  all_maps <- expand.grid(n1 = names(cr1), n2 = names(cr2), stringsAsFactors = FALSE)
  m1 <- seq_len(nrow(all_maps)) %>%
    map(~ similarity_score(cr1[[all_maps$n1[.x]]], cr2[[all_maps$n2[.x]]]) %>% t()) %>%
    reduce(rbind)
  m2 <- as.data.frame(m1) %>%
    map(norm_this) %>%
    as.data.frame()
  # KFL : kept for later
  wts <- rep(1, ncol(m2))
  m3 <- m2 %>% apply(MARGIN = 1, function(x) sum(x * wts) / sum(wts))
  all_maps <- all_maps %>% mutate(dist = m3)

  all_maps_c <- get_connected_cols(all_maps)

  amap_ok <- all_maps_c %>% filter(dist <= combine_th)
  amap_not_ok <- all_maps_c %>% filter(dist > combine_th)

  if (nrow(amap_not_ok) > 0) {
    repeat({
      induced_cols <- amap_not_ok %>%
        mutate(n1 = paste0("n1.", n1), n2 = paste0("n2.", n2)) %>%
        select(n1, n2) %>%
        unlist() %>%
        unique()

      if (length(induced_cols) > rest_cols) {
        amap_not_ok <- amap_not_ok %>% arrange(dist)
        if (rest_cols <= 0) {
          amap_not_ok <- amap_not_ok %>% filter(FALSE)
        } else {
          # pass a single row to amap_ok
          amap_ok <- amap_ok %>% bind_rows(amap_not_ok[1, ])
          amap_not_ok <- amap_not_ok[-1, ]
        }
      } else {
        break()
      }
    })
  }

  cmap <- tibble(new_name = NA_character_, old_name = NA_character_, block = NA_real_) %>% filter(FALSE)

  if (nrow(amap_ok) > 0) {
    amap_ok <- amap_ok %>%
      arrange(dist) %>%
      mutate(new_name = paste0("collated_", seq_along(dist)))

    cmap_this <- amap_ok %>%
      select(-dist) %>%
      tidyr::gather(cr, old_name, -new_name)

    cmap_this <- cmap_this %>%
      mutate(block = recode(cr, n1 = dc1$data_block[1], n2 = dc2$data_block[1])) %>%
      select(-cr)

    cmap <- cmap %>% bind_rows(cmap_this)
  }

  if (nrow(amap_not_ok) > 0) {
    cmap_this <- amap_not_ok %>%
      select(-dist) %>%
      tidyr::gather(cr, old_name) %>%
      mutate(new_name = paste0("uncollated_", seq_along(old_name)))

    cmap_this <- cmap_this %>%
      mutate(block = recode(cr, n1 = dc1$data_block[1], n2 = dc2$data_block[1])) %>%
      select(-cr)

    cmap <- cmap %>% bind_rows(cmap_this)
  }

  cmap1 <- cmap %>% filter(block == dc1$data_block[1])
  cmap2 <- cmap %>% filter(block == dc2$data_block[1])

  for (i in seq_len(nrow(cmap1))) {
    colnames(dc1)[which(colnames(dc1) == cmap1$old_name[[i]])] <- cmap1$new_name[[i]]
  }

  for (i in seq_len(nrow(cmap2))) {
    colnames(dc2)[which(colnames(dc2) == cmap2$old_name[[i]])] <- cmap2$new_name[[i]]
  }

  dcnew <- dc1 %>% bind_rows(dc2)

  if (!retain_other_cols) {
    nc_cols <- colnames(dcnew) %>%
      stringr::str_detect("collated") %>%
      colnames(dcnew)[.]
    dcnew <- dcnew[c(intersect(defcols, colnames(dcnew)), nc_cols)]
  }

  dcnew
}
