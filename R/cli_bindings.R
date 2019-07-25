
cli_bb <- function(x) {
  cli::style_bold(cli::col_blue(x))
}

cli_b <- function(x) {
  cli::col_blue(x)
}


cli_br <- function(x) {
  cli::style_bold(cli::col_red(x))
}

cli_bs <- paste0(cli::symbol$menu, " ")
