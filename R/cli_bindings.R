
ok_cli <- function() {
  is_available("cli")
}

cli_bs <- function() {
  if (ok_cli()) {
    paste0(cli::symbol$menu, " ")
  } else {
    "* "
  }
}
cli_tick <- function() {
  if (ok_cli()) {
    cli::symbol$tick
  } else {
    "V"
  }
}
cli_cross <- function() {
  if (ok_cli()) {
    cli::symbol$cross
  } else {
    "X"
  }
}


cli_bb <- function(x) {
  if (ok_cli()) {
    cli::style_bold(cli::col_blue(x))
  } else {
    x
  }
}

cli_b <- function(x) {
  if (ok_cli()) {
    cli::col_blue(x)
  } else {
    x
  }
}

cli_r <- function(x) {
  if (ok_cli()) {
    cli::col_red(x)
  } else {
    x
  }
}

cli_g <- function(x) {
  if (ok_cli()) {
    cli::col_green(x)
  } else {
    x
  }
}

cli_br <- function(x) {
  if (ok_cli()) {
    cli::style_bold(cli::col_red(x))
  } else {
    x
  }
}

cli_box <- function(x, ...) {
  if (ok_cli()) {
    cli::cat_boxx(x, ...)
  } else {
    cat(x, sep = "\n")
  }
}
