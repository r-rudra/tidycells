
make_read_cell_part <- function(obj) {
  stage <- attr(obj, "read_cells_stage")
  if (is.null(stage)) {
    abort(paste0(
      "No 'read_cells_stage' attribute found!",
      "\nMake sure the 'obj' is outcome from read_cells with 'simplify = TRUE' and 'at_level<6'"
    ))
  }

  if (!(stage %in% read_cell_task_orders)) {
    abort("Unknown stage of the object!")
  }

  attr(obj, "read_cells_stage") <- NULL

  out_l <- list(stage = stage)
  if (stage == read_cell_task_orders[1]) {
    out_l$info <- list(content = obj)
  }

  if (stage == read_cell_task_orders[2]) {
    out_l$is_empty <- is.null(obj)
    out_l$cell_list <- obj
  }

  if (stage == read_cell_task_orders[3]) {
    out_l$is_empty <- is.null(obj)
    out_l$cell_list <- obj
  }

  if (stage == read_cell_task_orders[4]) {
    out_l$is_empty <- is.null(obj)
    out_l$cell_analysis_list <- obj
  }

  # kept for compatibilty maybe this is not required
  if (stage == read_cell_task_orders[5]) {
    out_l$is_empty <- is.null(obj)
    out_l$final_composition <- obj
  }

  class(out_l) <- read_cell_part_class

  out_l
}
