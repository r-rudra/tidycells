
test_that("infer_group_connected_blocks checks", {

  # This will be slower but more explicit version of the
  # infer_group_connected_blocks function.
  infer_group_connected_blocks_slow <- function(df){
    groups <- list()
    for(i in seq_len(nrow(df))) {
      this_row <- as.character(df[i,])
      chk <- purrr::map_lgl(groups, ~ length(intersect(.x,this_row))>0)
      if(!any(chk)){
        # If no group found, create a new group
        groups <- append(groups, list(this_row))
      } else {
        # If a group is found, merge this row with that group
        idx <- which(chk)[1]
        groups[[idx]] <- unique(c(groups[[idx]], this_row))
      }
    }
    groups %>% purrr::map(unique)
  }

  # Example 1: Simple chain (single connected component)
  df1 <- data.frame(a = c("A", "B", "C"), b = c("B", "C", "D"))
  res_fast1 <- infer_group_connected_blocks(df1)
  res_slow1 <- infer_group_connected_blocks_slow(df1)
  expect_equal(res_fast1, res_slow1)

  # Example 2: Disconnected pairs (two components)
  df2 <- data.frame(a = c("A", "C"), b = c("B", "D"))
  res_fast2 <- infer_group_connected_blocks(df2)
  res_slow2 <- infer_group_connected_blocks_slow(df2)
  expect_equal(res_fast2, res_slow2)

  # Example 3: Single node (self loop)
  df3 <- data.frame(a = "A", b = "A")
  res_fast3 <- infer_group_connected_blocks(df3)
  res_slow3 <- infer_group_connected_blocks_slow(df3)
  expect_equal(res_fast3, res_slow3)

  # Example 4: Complex graph with merging (two components)
  df4 <- data.frame(a = c("A", "B", "C", "E"), b = c("B", "C", "D", "F"))
  res_fast4 <- infer_group_connected_blocks(df4)
  res_slow4 <- infer_group_connected_blocks_slow(df4)
  expect_equal(res_fast4, res_slow4)
})
