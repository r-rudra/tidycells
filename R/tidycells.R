
#' `tidycells` package
#'
#' **Read Natural Tabular Data from Diverse Sources and Transform them into a Format Compatible to 'Tidyverse'**
#'
#' The package provides utilities to read, cells from complex tabular data and heuristic detection based structural assignment of those cells to a columnar or tidy format.
#' Read functionality has the ability to read (in a unified manner) structured, partially structured or unstructured tabular data (usually spreadsheets for public data dissemination and aimed for common human understanding) from various types of documents. The tabular information is read as cells.
#' The 'structure assignment' functionality has both supervised and unsupervised way of assigning cells data to columnar/tidy format. Multiple disconnected blocks of tables in a single sheet are also handled appropriately.
#' These tools are suitable for unattended conversation of (maybe a pile of) messy tables (like government data) into a consumable format(usable for further analysis and data wrangling).
#' @author Indranil Gayen
#' @docType package
#' @name tidycells-package
NULL

## fix for R CMD check
if (getRversion() >= "2.15.1") {
  gvars <- c(

    ########
    # for dplyr oprations non-standard evaluation (nse)

    ".", "ad", "ag",
    "aid", "attr_gid", "attr_gid_split", "attr_group",
    "c_dim", "c_dim_data", "c_max", "c_max.x",
    "c_max.y", "c_min", "c_min.x", "c_min.y",
    "ccn", "cdt", "cell_group_type", "ch",
    "cid", "cn", "cn_id", "cn_id_",
    "cname", "cname_new", "cname_ord", "coc",
    "col_a", "col_d", "col_names", "col_orig",
    "d", "d1", "d2", "data_block",
    "data_gid", "data_type", "data_types", "date_raw",
    "decision", "dir_n", "direction", "direction_basic",
    "direction_group", "dist", "dist_", "dty",
    "dummy_order", "file_type", "full_dim", "full_dim_orig",
    "g_id_a", "g_id_e", "g_id_v", "gid",
    "implemented", "is_blank", "is_blank_not_num_c", "is_blank_not_num_cb",
    "is_full_dim_present", "is_num", "m_dist", "md",
    "n_att", "n_dirs", "natural_gid", "nc",
    "new_attr_gid", "new_attr_group", "new_dist", "new_gid",
    "new_type", "new_type_c", "new_type_r", "not_num_c",
    "not_num_cb", "nt", "num_and_pm", "num_c",
    "num_c_len", "num_try", "optional_cols", "package",
    "pkg_installed", "present_num_c_b", "r", "r_dim",
    "r_dim_data", "r_max", "r_max.x", "r_max.y",
    "r_min", "r_min.x", "r_min.y", "raw_value",
    "rc", "rc_n", "rel_dim", "rid",
    "RN", "row_a", "row_d", "row_orig",
    "sheet", "support_possible", "this_attr_max_rel", "txt",
    "txt_orig", "txt_size_", "type", "val_type",
    "value", "value_chk", "id",

    ########
    # for shiny

    "absolutePanel", "actionButton", "br",
    "brushedPoints", "brushOpts", "callModule", "checkboxInput",
    "clickOpts", "conditionalPanel", "dialogViewer", "div",
    "h3", "h5", "icon", "incProgress",
    "is.reactive", "isolate", "nearPoints", "NS",
    "observe", "observeEvent", "plotOutput", "radioButtons",
    "reactive", "reactiveVal", "removeNotification", "renderPlot",
    "runGadget", "selectizeInput", "shinyApp", "showNotification",
    "sliderInput", "span", "stopApp", "tags",
    "updateSelectizeInput", "wellPanel", "withProgress",

    ########
    # for miniUI

    "gadgetTitleBar", "miniButtonBlock", "miniContentPanel",
    "miniPage", "miniTabPanel", "miniTabstripPanel",

    ########
    # for rJava and docxtractr internal

    "lo_find", ".jnew"
  )

  utils::globalVariables(gvars)
  rm(gvars)
}
