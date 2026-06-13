
#' Set Package Options
#'
#' This function sets user-specific options for the package, such as whether to
#' derive nice attribute names or enable Excel-specific optimization etc..
#'
#' @param derive_nice_names_for_attributes Logical. Should the package derive
#'   user-friendly names for attributes? Default is \code{TRUE}.
#' @param excel_specific_optimization Logical. Should Excel-specific
#'   optimization be enabled? Default is \code{FALSE}.
#' @param check_attr_entrapment_on_full_overlap_join Logical. Should the package
#'   check for attribute entrapment during full overlap joins? Default is
#'   \code{FALSE}.
#' @param finalize_through_stages Logical. Should the package finalize through
#'   multiple stages? Default is \code{FALSE}.
#' @param finalize_till Character. Specifies the stage till which to finalize.
#'   Options are "collate_column" (default), "compose", "analyze_cells", and
#'   "value_attribute_classify".
#' @param collate_columns_on_whole Logical. Should columns be collated on whole?
#'   Default is \code{TRUE}.
#' @param auto_detach_shiny Logical. Should the package automatically detach
#'   Shiny when done? Default is \code{TRUE}.
#' @param delink_corner_headers Logical. Should corner headers be delinked from
#'   data blocks? Default is \code{FALSE}.
#'
#' @details This function stores the options in the package cache under the
#'   "user" namespace, which can affect package behavior such as naming
#'   conventions and optimization for Excel based cells etc.
#'
#' @return No return value; (called for side effects - setting user parameters.)
#' @export
set_options <- function(
    derive_nice_names_for_attributes = TRUE,
    excel_specific_optimization = FALSE,
    check_attr_entrapment_on_full_overlap_join = FALSE,
    finalize_through_stages = FALSE,
    finalize_till = "collate_column",
    collate_columns_on_whole = TRUE,
    auto_detach_shiny = TRUE,
    delink_corner_headers = FALSE) {

  # Validate finalize_till argument
  finalize_till <- match.arg(
    finalize_till,
    c("collate_column","compose", "analyze_cells", "value_attribute_classify"))

  # This function sets options for the package.
  util_pkg_cache(
    derive_nice_names_for_attributes = derive_nice_names_for_attributes,
    excel_specific_optimization = excel_specific_optimization,
    check_attr_entrapment_on_full_overlap_join = check_attr_entrapment_on_full_overlap_join,
    finalize_through_stages = finalize_through_stages,
    finalize_till = finalize_till,
    collate_columns_on_whole = collate_columns_on_whole,
    auto_detach_shiny = auto_detach_shiny,
    delink_corner_headers = delink_corner_headers,
    pkg_cache_head_name = "user")
}


# Specific get option method as system (by this package) and user both can set
# options. Priority is given to user options. Also a default value can be set
# (in case not defined by system or user).
#
# Ideally wherever we use this function (core_opt_get) all unique options should
# be listed to users through (set_options) with proper documentation.
core_opt_get <- function(key, default = NULL) {

  user_opt_exists <-
    util_pkg_cache(key, pkg_cache_head_name = "user", exists = TRUE)
  user_opt_val <- util_pkg_cache(key, pkg_cache_head_name = "user")

  sys_opt_exists <-
    util_pkg_cache(key, pkg_cache_head_name = "system", exists = TRUE)
  sys_opt_val <- util_pkg_cache(key, pkg_cache_head_name = "system")

  if (user_opt_exists) {
    return(user_opt_val)
  } else if (sys_opt_exists) {
    return(sys_opt_val)
  } else {
    return(default)
  }

}
