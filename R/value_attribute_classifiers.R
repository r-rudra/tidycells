
#######
#' @title Naive `data_type` Based Value/Attribute Classifier
#' @param x Either a [`cell_df`][cell_df-class] or leave blank for function output
#' (required by [`value_attribute_classify`][value_attribute_classify()])
#'
#' @rdname basic_classifier
#' @keywords internal
#' @export
#' @seealso [value_attribute_classify][value_attribute_classify()]
basic_classifier <- function(x, ...) {
  UseMethod("basic_classifier")
}

#' @rdname basic_classifier
#' @export
basic_classifier.default <- function(x, ...) {
  basic_classifier_raw
}

#' @rdname basic_classifier
#' @export
basic_classifier.cell_df <- function(x, ...) {
  basic_classifier_raw(x)
}



#######

#' @title Sample Based Value/Attribute Classifier
#'
#' @param x Either a [`cell_df`][cell_df-class] or leave blank for function output
#' (required by [`value_attribute_classify`][value_attribute_classify()])
#' @param value_sample Sample of values (an optional `character` vector)
#' @param attribute_sample Sample of attribute (an optional `character` vector)
#' @param empty_sample Sample of empty cells (an optional `character` vector)
#' @param partial_match Logical scalar indicating whether partial match is allowed. (Default `FALSE`)
#' @param case_insensitive Logical scalar indicating whether match will be case insensitive. (Default `FALSE`)
#' @param verbose Logical scalar for printing discovered cells (which gets new type by this rule)
#'
#' @rdname sample_based_classifier
#' @keywords internal
#' @export
#' @seealso [value_attribute_classify][value_attribute_classify()]
sample_based_classifier <- function(x, ...,
                                    value_sample,
                                    attribute_sample,
                                    empty_sample,
                                    partial_match = FALSE,
                                    case_insensitive = FALSE,
                                    verbose = FALSE) {
  UseMethod("sample_based_classifier")
}

#' @rdname sample_based_classifier
#' @export
sample_based_classifier.default <- function(x, ...,
                                            value_sample = NULL,
                                            attribute_sample = NULL,
                                            empty_sample = NULL,
                                            partial_match = FALSE,
                                            case_insensitive = FALSE,
                                            verbose = FALSE) {
  function(d) {
    sample_based_raw(d,
      value_sample = value_sample,
      attribute_sample = attribute_sample,
      empty_sample = empty_sample,
      partial_match = partial_match,
      case_insensitive = case_insensitive,
      verbose = verbose
    )
  }
}

#' @rdname sample_based_classifier
#' @export
sample_based_classifier.cell_df <- function(x, ...,
                                            value_sample,
                                            attribute_sample,
                                            empty_sample,
                                            partial_match = FALSE,
                                            case_insensitive = FALSE,
                                            verbose = FALSE) {
  sample_based_raw(x,
    value_sample = value_sample,
    attribute_sample = attribute_sample,
    empty_sample = empty_sample,
    partial_match = partial_match,
    case_insensitive = case_insensitive,
    verbose = verbose
  )
}

#####
#' @title Value/Attribute Classifier which considers number like cells as values
#' @param x Either a [`cell_df`][cell_df-class] or
#' leave blank for function output (required by [`value_attribute_classify`][value_attribute_classify()])
#' @param allow_chars (Optional) Allow characters together with numbers. (a character vector)
#' @param verbose Logical scalar for printing discovered cells (which gets new type by this rule)
#'
#' @rdname numeric_values_classifier
#' @keywords internal
#' @export
#' @seealso [value_attribute_classify][value_attribute_classify()]
numeric_values_classifier <- function(x, ...,
                                      allow_chars,
                                      verbose = FALSE) {
  UseMethod("numeric_values_classifier")
}

#' @rdname numeric_values_classifier
#' @export
numeric_values_classifier.default <- function(x, ...,
                                              allow_chars = NULL,
                                              verbose = FALSE) {
  function(d) {
    numeric_values_raw(d,
      allow_chars = allow_chars,
      verbose = verbose
    )
  }
}

#' @rdname numeric_values_classifier
#' @export
numeric_values_classifier.cell_df <- function(x, ...,
                                              allow_chars,
                                              verbose = FALSE) {
  numeric_values_raw(x,
    allow_chars = allow_chars,
    verbose = verbose
  )
}


#################
# RAW functions #
#################

basic_classifier_raw <- function(d) {
  if (!is_cell_df(d)) {
    abort("cell_df expected")
  }

  if (!hasName(d, "type")) {
    d <- d %>% mutate(type = if_else(data_type == "numeric", "value", "attribute"))
  }

  d
}

sample_based_part <- function(dat, item_sample, expected_type, partial_match = FALSE, case_insensitive = FALSE) {
  d0 <- dat

  d0_g <- tibble::tibble()

  val <- item_sample

  if (case_insensitive) {
    d0 <- d0 %>% mutate(value_chk = tolower(value))
    val <- tolower(val)
  } else {
    d0 <- d0 %>% mutate(value_chk = value)
  }

  if (partial_match) {
    val <- paste0(val, collapse = "|")
    d0_pick <- d0 %>% filter(stringr::str_detect(value_chk, val))
  } else {
    d0_pick <- d0 %>% filter(value_chk %in% val)
  }

  d0_pick <- d0_pick %>% filter(type != expected_type)

  if (nrow(d0_pick) > 0) {
    d0_g <- d0_pick %>%
      distinct(gid) %>%
      inner_join(d0, by = "gid")

    d0_g$new_type <- NULL

    d0_g <- d0_pick %>%
      mutate(new_type = expected_type) %>%
      select(row, col, new_type) %>%
      right_join(d0_g, by = c("row", "col"))

    d0_g_r <- d0_g %>%
      group_by(row, gid) %>%
      mutate(new_type = if_else(any(!is.na(new_type)), new_type[!is.na(new_type)][1], NA_character_)) %>%
      ungroup()

    d0_g_c <- d0_g %>%
      group_by(col, gid) %>%
      mutate(new_type = if_else(any(!is.na(new_type)), new_type[!is.na(new_type)][1], NA_character_)) %>%
      ungroup()

    d0_g <- d0_g_r %>%
      select(row, col, new_type) %>%
      right_join(d0_g_c, by = c("row", "col"), suffix = c("_r", "_c"))

    d0_g <- d0_g %>%
      mutate(new_type = if_else(is.na(new_type_r), new_type_c, new_type_r)) %>%
      select(-new_type_c, -new_type_r)
  }

  d0_g
}

sample_based_raw <- function(d, value_sample, attribute_sample, empty_sample, partial_match = FALSE, case_insensitive = FALSE, verbose = FALSE) {
  d <- basic_classifier(d)

  dv <- d %>% filter(type == "value")
  if (nrow(dv) > 0) {
    dvgid <- get_group_id(dv)
  } else {
    dvgid <- list(group_id_map = tibble(row = NA, col = NA, gid = "dummy"))
  }

  da <- d %>% filter(type == "attribute")
  if (nrow(da) > 0) {
    dagid <- get_group_id(da)
  } else {
    dagid <- list(group_id_map = tibble(row = NA, col = NA, gid = "dummy"))
  }

  de <- d %>% filter(type == "empty")
  if (nrow(de) > 0) {
    degid <- get_group_id(de)
  } else {
    degid <- list(group_id_map = tibble(row = NA, col = NA, gid = "dummy"))
  }

  d0 <- d
  d0 <- dagid$group_id_map %>%
    mutate(g_id_a = paste0("A", gid)) %>%
    select(row, col, g_id_a) %>%
    right_join(d0, by = c("row", "col"))
  d0 <- dvgid$group_id_map %>%
    mutate(g_id_v = paste0("V", gid)) %>%
    select(row, col, g_id_v) %>%
    right_join(d0, by = c("row", "col"))
  d0 <- degid$group_id_map %>%
    mutate(g_id_e = paste0("E", gid)) %>%
    select(row, col, g_id_e) %>%
    right_join(d0, by = c("row", "col"))
  d0 <- d0 %>%
    mutate(gid = if_else(is.na(g_id_a), if_else(is.na(g_id_v), g_id_e, g_id_v), g_id_a)) %>%
    select(-g_id_v, -g_id_a, -g_id_e) %>%
    filter(!is.na(gid))

  take_val <- FALSE
  take_att <- FALSE
  take_emp <- FALSE

  if (!missing(value_sample)) {
    if (length(value_sample) > 0) {
      take_val <- TRUE
    }
  }

  if (!missing(attribute_sample)) {
    if (length(attribute_sample) > 0) {
      take_att <- TRUE
    }
  }

  if (!missing(empty_sample)) {
    if (length(empty_sample) > 0) {
      take_emp <- TRUE
    }
  }

  if (take_val) {
    d0_n <- sample_based_part(
      dat = d0,
      item_sample = value_sample,
      expected_type = "value",
      partial_match = partial_match,
      case_insensitive = case_insensitive
    )

    # delete potential column
    d0$nt <- NULL
    if (nrow(d0_n) > 0) {
      d0 <- d0_n %>%
        select(row, col, nt = new_type) %>%
        right_join(d0, by = c("row", "col")) %>%
        rename(new_type = nt)
    }
  }

  if (!hasName(d0, "new_type")) {
    d0 <- d0 %>% mutate(new_type = NA_character_)
  }

  if (take_att) {
    d0_n <- sample_based_part(
      dat = d0,
      item_sample = attribute_sample,
      expected_type = "attribute",
      partial_match = partial_match,
      case_insensitive = case_insensitive
    )

    # delete potential column
    d0$nt <- NULL

    if (nrow(d0_n) > 0) {
      d0 <- d0_n %>%
        select(row, col, nt = new_type) %>%
        right_join(d0, by = c("row", "col")) %>%
        mutate(new_type = if_else(is.na(nt), new_type, nt)) %>%
        select(-nt)
    }
  }

  if (take_emp) {
    d0_n <- sample_based_part(
      dat = d0,
      item_sample = empty_sample,
      expected_type = "empty",
      partial_match = partial_match,
      case_insensitive = case_insensitive
    )

    # delete potential column
    d0$nt <- NULL

    if (nrow(d0_n) > 0) {
      d0 <- d0_n %>%
        select(row, col, nt = new_type) %>%
        right_join(d0, by = c("row", "col")) %>%
        mutate(new_type = if_else(is.na(nt), new_type, nt)) %>%
        select(-nt)
    }
  }

  d0 <- d0 %>% mutate(new_type = if_else(is.na(new_type), type, new_type))
  d0 <- d0 %>% select(row, col, data_type, value, type, new_type)

  # discovered objects
  discover <- d0 %>% filter(type != new_type)
  if (nrow(discover) & verbose) {
    cat("\nNew cells detected as different type\n")
    print(tibble::as_tibble(discover))
  }

  d0 %>%
    select(row, col, data_type, value, type = new_type) %>%
    new_cell_df()
}

check_num <- function(x, allowed_strings) {
  omit_allowed_strings <- TRUE

  if (!missing(allowed_strings)) {
    if (is.character(allowed_strings)) {
      if (length(allowed_strings) > 0) {
        omit_allowed_strings <- FALSE
      }
    }
  }

  if (omit_allowed_strings) {
    this_reg <- "(?:(?:\\+)?(?:-)?(?:\\.)?(?:,)?[[:digit:]])+"
    this_reg_non_n <- "[+\\-\\.,]+"
  } else {
    this_reg <- paste0("?(?:", allowed_strings, ")", collapse = "") %>%
      paste0("(?:(?:\\+)?(?:-)?(?:\\.)?(?:,)", ., "?[[:digit:]])+")
    this_reg_non_n <- paste0(allowed_strings, collapse = "") %>%
      paste0("[+\\-\\.,", ., "]+")
  }

  this_reg_dc <- "[\\.,]+"

  xdo <- tibble(txt_orig = x, seq = seq(txt_orig))
  xdo <- xdo %>% mutate(txt = stringr::str_trim(txt_orig))

  xd <- xdo %>% distinct(txt)

  xd <- xd %>% mutate(
    num_c = stringr::str_extract_all(txt, this_reg),
    num_c_len = num_c %>% map_int(length),
    not_num_c = stringr::str_remove_all(txt, this_reg),
    present_num_c_b = stringr::str_detect(not_num_c, this_reg_non_n),
    is_blank_not_num_c = nchar(stringr::str_trim(not_num_c)) == 0
  )

  xd <- xd %>%
    filter(present_num_c_b) %>%
    mutate(
      not_num_cb = stringr::str_remove_all(not_num_c, this_reg_non_n),
      is_blank_not_num_cb = nchar(stringr::str_trim(not_num_cb)) == 0
    ) %>%
    bind_rows(xd %>%
      filter(!present_num_c_b))

  # 1st decision
  xd <- xd %>% mutate(decision = if_else(num_c_len != 1,
    if_else(((present_num_c_b & is_blank_not_num_cb) | (is_blank_not_num_c)) &
      num_c_len == 0,
    "blank",
    "non_num"
    ),
    if_else(is_blank_not_num_c,
      NA_character_,
      if_else(present_num_c_b & is_blank_not_num_cb,
        NA_character_, "non_num"
      )
    )
  ))

  # 2nd decision
  # pm : plus minus
  # num_and_pm :- numer and plus minus
  xd <- xd %>%
    filter(is.na(decision)) %>%
    mutate(
      num_and_pm = num_c %>% map_chr(~ paste0(.x, collapse = "")) %>% stringr::str_remove_all(this_reg_dc),
      num_try = suppressWarnings(as.numeric(num_and_pm)),
      decision = if_else(is.na(num_try), "non_num", "num")
    ) %>%
    bind_rows(xd %>% filter(!is.na(decision)))


  xd %>%
    select(txt, decision) %>%
    right_join(xdo, by = "txt") %>%
    arrange(seq) %>%
    pull(decision)
}


numeric_values_raw <- function(d, allow_chars, verbose = FALSE) {
  d <- basic_classifier(d)

  if (!missing(allow_chars)) {
    d0 <- d %>% mutate(val_type = check_num(value, allowed_strings = allow_chars))
  } else {
    d0 <- d %>% mutate(val_type = check_num(value))
  }


  d0 <- d0 %>% mutate(new_type = recode(val_type,
    non_num = "attribute", num = "value", blank = "empty"
  ))


  d0 <- d0 %>% select(row, col, data_type, value, type, new_type)

  # discovered objects
  discover <- d0 %>% filter(type != new_type)
  if (nrow(discover) & verbose) {
    cat("\nNew cells detected as different type\n")
    print(tibble::as_tibble(discover))
  }

  d0 %>% select(row, col, data_type, value, type = new_type)
}
