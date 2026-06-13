# This module provides utilities to simulate non-availability of specific packages.
# It is intended for use within a host package during testing, to verify behavior
# when optional dependencies are missing or non-functional.

# This module consists of two standalone files to be placed in the /R directory
# of the host package:
#
# 1. pkg_availability_sim.R
#    - Provides internal functions for checking and simulating package availability.
#
# 2. pkg_availability_sim_fuse.R
#    - Optionally extended by the host package with specific checks for functionality.

# Main Functions:
# - pkg_is_available(): Used within the host package to check for availability.
# - pkg_not_available(): Used in test code to simulate unavailable packages.
# - pkg_init_if_not_already(): Initializes internal environment; allows host to define
#   custom checks via `is_working_for_this_pkg`.

pkg_availability_sim_env <- new.env()

#' Check Package Availability
#'
#' Determines if a package is installed and optionally functional for the host
#' package.
#'
#' @param pkg Character string. Name of the package to check.
#' @param check_working Logical. If TRUE, also verifies functionality using a
#'   custom-defined check (if available).
#'
#' @return Logical. TRUE if available (and working, if checked); FALSE
#'   otherwise.
#'
#' @seealso [pkg_not_available()]
#' @keywords internal
pkg_is_available <- function(pkg, check_working = FALSE) {
  # All these safe checking may not be required as this function is not designed
  # to be called by end users
  #
  # if (!is.character(pkg)) {
  #   return(FALSE)
  # }
  #
  # if (length(pkg) != 1) {
  #   stop("pkg_is_available - Works only for single <pkg>!", call. = FALSE)
  # }
  #
  # if (nchar(pkg)) {
  #   return(FALSE)
  # }

  pkg_init_if_not_already()

  if (pkg %in% pkg_availability_sim_env$na_pkgs) {
    return(FALSE)
  }

  if (check_working &&
    pkg %in% names(pkg_availability_sim_env$is_working_for_this_pkg)) {
    chk <- tryCatch(
      suppressMessages(
        suppressWarnings(
          suppressPackageStartupMessages(
            pkg_availability_sim_env$is_working_for_this_pkg[[pkg]]()
          )
        )
      ),
      error = function(e) FALSE
    )

    if (isFALSE(chk)) {
      return(FALSE)
    }
  }

  if (check_working) {
    return(requireNamespace(pkg, quietly = TRUE))
  }

  # check like <rlang>
  if (isNamespaceLoaded(pkg)) {
    return(TRUE)
  }

  for (path in file.path(.libPaths(), pkg)) {
    if (file.exists(path)) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Simulate Package Non-Availability
#'
#' Records packages as "not available" for testing purposes.
#'
#' @param pkgs Character vector. Names of packages to simulate as unavailable.
#' @param add Logical. If TRUE, adds to the existing list; otherwise replaces
#'   it.
#'
#' @return Invisible 0. Used for its side effect of modifying the internal
#'   environment.
#'
#' @seealso [pkg_is_available()]
#' @keywords internal
pkg_not_available <- function(pkgs, add = TRUE) {
  pkg_init_if_not_already()

  if (missing(pkgs)) {
    pkg_availability_sim_env$na_pkgs <- NULL
  } else if (is.character(pkgs) && length(pkgs) >= 1) {
    old <- pkg_availability_sim_env$na_pkgs
    pkg_availability_sim_env$na_pkgs <- if (add) unique(c(old, pkgs)) else pkgs
  }

  invisible(0)
}
