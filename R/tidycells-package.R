#' `tidycells` package
#'
#' **Read Tabular Data from Diverse Sources and Easily Make Them Tidy**
#'
#' The package provides utilities to read, cells from complex tabular data
#' and heuristic detection based structural assignment of those cells to a
#' columnar or tidy format. Read functionality has the ability to read (in
#' a unified manner) structured, partially structured or unstructured
#' tabular data (usually spreadsheets for public data dissemination and
#' aimed for common human understanding) from various types of documents.
#' The tabular information is read as cells. The 'structure assignment'
#' functionality has both supervised and unsupervised way of assigning
#' cells data to columnar/tidy format. Multiple disconnected blocks of
#' tables in a single sheet are also handled appropriately. These tools are
#' suitable for unattended conversation of (maybe a pile of) messy tables
#' (like government data) into a consumable format(usable for further
#' analysis and data wrangling).
#'
#' To get started check out \href{../doc/tidycells-intro.html}{\code{vignette("tidycells-intro")}}.
#'
#' @name tidycells-package
#' @keywords internal
"_PACKAGE"


# pkg share global vars
tidycells_pkg_env <- new.env()
# used in --> is_available.R
assign("na_pkgs", NULL, envir = tidycells_pkg_env)
