

if (rlang::is_installed("cli")) {
  cli_bs <- paste0(cli::symbol$menu, " ")
  cli_tick <- cli::symbol$tick
  cli_cross <- cli::symbol$cross


  cli_bb <- function(x) {
    cli::style_bold(cli::col_blue(x))
  }

  cli_b <- function(x) {
    cli::col_blue(x)
  }

  cli_r <- function(x) {
    cli::col_red(x)
  }

  cli_g <- function(x) {
    cli::col_green(x)
  }

  cli_br <- function(x) {
    cli::style_bold(cli::col_red(x))
  }

  cli_box <- function(x, ...) {
    cli::cat_boxx(x, ...)
  }
} else {
  cli_bs <- "* "
  cli_tick <- "V"
  cli_cross <- "X"

  cli_bb <- identity

  cli_b <- identity

  cli_r <- identity

  cli_g <- identity

  cli_br <- identity

  cli_box <- function(x, ...) {
    cat(x, ...)
  }
}
