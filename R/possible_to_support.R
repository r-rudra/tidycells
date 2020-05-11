
supported_types <- tibble(
  file_type = c("csv{utils}", "csv", "xls{readxl}", "xls", "xlsx", "doc", "docx", "pdf", "html"),
  file_type_raw = c("csv", "csv", "xls", "xls", "xlsx", "doc", "docx", "pdf", "html"),
  implemented = TRUE,
  package = c("utils", "readr", "readxl", "xlsx", "tidyxl", "docxtractr", "docxtractr", "tabulizer", "XML")
)


possible_to_support <- function(print_info = TRUE, return_print_info = FALSE, perform_real_file_read_check = FALSE) {
  pkgs <- unique(supported_types$package)
  ins_pkgs <- pkgs %>% map_lgl(is_available)
  ins_pkgs <- pkgs[ins_pkgs]

  st <- supported_types %>%
    filter(implemented) %>%
    mutate(
      pkg_installed = package %in% ins_pkgs,
      support_possible = pkg_installed
    )

  if (perform_real_file_read_check) {
    readable_types <- possible_to_support_real_file_type_checks()
    st <- st %>% mutate(is_read_checked = (file_type_raw %in% readable_types) | (file_type %in% readable_types))
  }

  # extra check for rJava dependency of xlsx
  # this is not required but not a problem if kept
  # in view of safe_dependency_check
  if ("xls" %in% st$file_type) {
    if (!is_xlsx_ok()) {
      st <- st %>%
        mutate(support_possible = if_else(file_type == "xls", FALSE, support_possible))
    }
  }

  # extra check for LibreOffice dependency of docxtractr
  if ("doc" %in% st$file_type) {
    if (!detect_LibreOffice()) {
      st <- st %>%
        mutate(support_possible = if_else(file_type == "doc", FALSE, support_possible))
    }
  }

  if (print_info) {
    st_ok <- st %>%
      filter(support_possible) %>%
      pull(file_type)
    st_not_ok <- st %>%
      filter(!support_possible) %>%
      pull(file_type)
    pkg_need <- st %>%
      filter(!pkg_installed) %>%
      pull(package) %>%
      unique()
    if ("csv" %in% st_ok) {
      st_ok <- setdiff(st_ok, "csv{utils}")
    }
    if ("xls" %in% st_ok) {
      st_ok <- setdiff(st_ok, "xls{readxl}")
    }
    if (length(st_ok) > 0) {
      extra_msg0 <- NULL
      if ("doc" %in% st_ok) {
        extra_msg0 <- paste0(
          "LibreOffice is present so doc files will be supported but it may take little longer time to read/detect.",
          "\n  You may need to open LibreOffice outside this R-Session manually to speed it up.",
          "\n  In case the doc is not working, try running docxtractr::read_docx('<target doc file>').",
          "\n  Check whether the file is being read correctly."
        )
      } else {
        if (st$pkg_installed[st$file_type == "doc"] == TRUE) {
          extra_msg0 <- "LibreOffice may be required for doc files"
        }
      }

      extra_msg1 <- NULL
      if (st$pkg_installed[st$file_type == "xls"] == TRUE & st$support_possible[st$file_type == "xls"] == FALSE) {
        extra_msg1 <- "The 'xlsx' package is installed but not working. Check 'rJava' installation."
      }

      # append in single msg
      extra_msg <- ""
      if (!is.null(extra_msg0)) {
        extra_msg <- paste0(
          extra_msg,
          cli_bs(),
          cli_b(extra_msg0),
          "\n"
        )
      }

      if (!is.null(extra_msg1)) {
        extra_msg <- paste0(
          extra_msg,
          cli_bs(),
          cli_b(extra_msg1),
          "\n"
        )
      }

      st_ok_msg <- st_ok %>%
        paste0(collapse = ", ") %>%
        cli_bb() %>%
        paste0(
          cli_b("Support present for following type of files: "),
          .,
          "\nNote:\n",
          extra_msg,
          cli_bs(),
          cli_b("Support is enabled for content type (means it will work even if the extension is wrong)")
        )
    } else {
      st_ok_msg <- NULL
    }



    if (length(st_not_ok) > 0) {
      if (length(pkg_need) > 0) {
        pkg_msg <- paste0(
          "\nNote:\n",
          cli_bs(),
          cli_b("These packages are required: "),
          cli_br(paste0(pkg_need, collapse = ", "))
        )
      } else {
        pkg_msg <- ""
      }

      st_not_ok_msg <- st_not_ok %>%
        paste0(collapse = ", ") %>%
        cli_br() %>%
        paste0(
          cli_bs(),
          cli_b("Support"),
          cli_bb(" not "),
          cli_b("present for following type of files: "),
          .,
          pkg_msg
        )
    } else {
      st_not_ok_msg <- NULL
    }

    msg <- paste0(st_ok_msg, "\n", st_not_ok_msg)
    if (return_print_info) {
      return(msg)
    }
    cat(msg)
    if (is_available("cli")) {
      xst <- st %>%
        filter(implemented) %>%
        select(-implemented) %>%
        select(type = file_type, package, present = pkg_installed, support = support_possible)


      xst_msg <- format(xst)
      xst_msg <- xst_msg[-c(1, 3)]

      xst_msg[1] <- cli_b(xst_msg[1])

      xst_msg <- xst_msg %>%
        stringr::str_replace_all("TRUE", paste0("  ", cli_g(cli_tick()), "")) %>%
        stringr::str_replace_all("FALSE", paste0("  ", cli_r(cli_cross()), ""))
      cat("\nDetails: \n")
      cli_box(xst_msg, col = "cyan")
    }
  }

  return(invisible(st))
}


# check by reading sample files
possible_to_support_real_file_type_checks_raw <- function() {
  fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
  dm <- tibble(fn = list.files(fold, full.names = TRUE))

  dm <- dm %>%
    mutate(original_file_type = dm$fn %>%
      map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        map_chr(1)))

  dm <- dm %>%
    dplyr::group_by(original_file_type) %>%
    dplyr::sample_n(1) %>%
    dplyr::ungroup()

  dtypes <- dm$fn %>% purrr::map(~ try(read_cells(.x, at_level = "make_cells", simplify = FALSE), silent = TRUE))
  chk <- dtypes %>% map_lgl(~ {
    e <- try(nrow(.x$cell_list[[1]]) > 0, silent = TRUE)
    if_else(is.logical(e), e, FALSE)
  })
  passed_types <- dtypes %>% map_chr(~ {
    e <- try(.x$info$type, silent = TRUE)
    if_else(inherits(e, "try-error"), "", e)
  })
  passed_types <- passed_types[chk]

  passed_types <- intersect(passed_types, dm$original_file_type)

  passed_types
}

possible_to_support_real_file_type_checks <- function() {
  e <- try(possible_to_support_real_file_type_checks_raw(), silent = TRUE)
  if (inherits(e, "try-error")) e <- character(0)
  e
}
