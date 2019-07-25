

#' @export
print.read_cell_part <- function(x, ...) {
  msg <- cli_bb("A partial read_cell")
  msg <- paste0(
    msg,
    "\n",
    cli_b("At stage "), x$stage
  )
  cat(msg)
}
