
# pkg share global vars
tidycells_pkg_env <- new.env()
# used in --> is_available.R
assign("na_pkgs", NULL, envir = tidycells_pkg_env)


# safe_dependency_check
# in later version create a proper framework
# used in --> is_available.R
assign("safe_dependency_check", list(xlsx = function() {
  chk <- F
  try(
    {
      if (is_xlsx_ok(no_pkg_check = T)) {
        sample_xls <- system.file("extdata", "messy", "xls.pdf", package = "tidycells", mustWork = TRUE)
        x <- read_xls_from_xlsx(sample_xls, no_pkg_check = T)
        if (is.data.frame(x[[1]])) {
          if (nrow(x[[1]]) > 0) {
            chk <- T
          }
        }
      }
    },
    silent = T
  )
  chk
}), envir = tidycells_pkg_env)
