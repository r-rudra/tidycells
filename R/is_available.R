
# this is designed to test and simulate
# non availability of certain packages
is_available <- function(pkgname) {
  if (pkgname %in% tidycells_pkg_env$na_pkgs) {
    return(FALSE)
  }
  rlang::is_installed(pkgname)
}

not_available <- function(pkgs, add = TRUE) {
  if (missing(pkgs)) {
    tidycells_pkg_env$na_pkgs <- NULL
  } else {
    if (length(pkgs) >= 1) {
      if (is.character(pkgs)) {
        if (add) {
          old <- tidycells_pkg_env$na_pkgs
          pkgs <- c(old, pkgs)
        }
        tidycells_pkg_env$na_pkgs <- pkgs
      }
    }
  }
  return(invisible(0))
}
