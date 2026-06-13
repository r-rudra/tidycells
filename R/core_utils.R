
util_convertible_to_processable <- function(x){
  # either data frame or list of data frame or matrix or list matrix list of
  # matrix and data frame (mix is also allowed)
  if (is.data.frame(x) || is.matrix(x)) {
    return(TRUE)
  }

  if (is.list(x)) {
    if (all(purrr::map_lgl(x, ~ is.data.frame(.x) || is.matrix(.x)))) {
      return(TRUE)
    }
  }

  return(FALSE)
}


util_utilize_namespace_object <- function(pkg, what, ...){
  e <- asNamespace(pkg)
  if(exists(what, envir = e, inherits = FALSE)) {
    if (is.function(e[[what]])) {
      return(e[[what]](...))
    }
  }
}

# Internal environment for package-wide use
util_internal_env_of_this_pkg <- new.env()

#' Common Knowledge/ Package Cache - Store/Get for this package environment
#'
#' Stores or retrieves named values in the package-wide internal environment for
#' cross-module sharing.
#'
#' @param ... Named arguments to set (key = value); unnamed character vectors to
#'   get keys.
#' @param pkg_cache_head_name Name for the main list in the environment (default
#'   "pkg_cache").
#' @param clean If TRUE, clears all stored values.
#' @param append If TRUE, appends to existing keys (data.frames merged, vectors
#'   concatenated).
#' @param exists If TRUE, returns TRUE if all keys exist (for gets).
#' @return Value(s) for get
#' @keywords internal
util_pkg_cache <- function(
    ...,
    pkg_cache_head_name = "pkg_cache",
    clean = FALSE, append = TRUE, exists = FALSE) {

  env <- util_internal_env_of_this_pkg

  if (clean) {
    env[[pkg_cache_head_name]] <- list()
    return(invisible(NULL))
  }

  dots <- list(...)
  nds <- names(dots)
  ck <- env[[pkg_cache_head_name]]
  if (is.null(ck)) ck <- list()

  # SET: named args
  if (!is.null(nds) && any(nzchar(nds))) {
    for (nm in nds[nzchar(nds)]) {
      val <- dots[[nm]]
      if (append && !is.null(ck[[nm]])) {
        # If both are data.frames, merge and remove duplicates
        if (is.data.frame(val) && is.data.frame(ck[[nm]])) {
          ck[[nm]] <- dplyr::bind_rows(ck[[nm]], val) %>% dplyr::distinct()
        } else {
          # Otherwise, concatenate and de-duplicate
          ck[[nm]] <- unique(c(ck[[nm]], val))
        }
      } else {
        ck[[nm]] <- val
      }
    }
    env[[pkg_cache_head_name]] <- ck
    return(invisible(NULL))
  }

  # GET: unnamed args (must be character keys)
  if (length(dots) > 0) {
    keys <- unlist(dots)
    vals <- ck[keys]
    if (exists) return(all(!vapply(vals, is.null, logical(1))))
    if (length(vals) == 1) return(vals[[1]])
    return(vals)
  }
  invisible(NULL)
}


#' Optimally apply a function to a vector by handling duplicates
#'
#' Efficiently applies a (potentially expensive) function to a vector by
#' avoiding redundant computations for duplicate values. If the input vector is
#' large and contains many repeated values, the function is applied only to the
#' unique values, and results are mapped back to the original ordering. This
#' optimization is triggered only when both the input length and the proportion
#' of unique values cross user-specified thresholds.
#'
#' This function is only safe for "value-based" functions: those for which
#' `f(x)[i]` depends only on the value of `x[i]`, not on its position in `x` or
#' any global state. The output of `f(x)` must be the same length as `x`.
#'
#' If the function also requires other arguments that should be subset in sync
#' with `x`, their names may be supplied in `paired_arguments`.
#'
#' @param x Input vector.
#' @param f A vectorized function to apply to `x`. Must be "value-based" (see
#'   Details).
#' @param ratio_threshold If (`n_distinct / n_total`) is below this, the
#'   optimization is triggered. Defaults to 0.7.
#' @param length_threshold If `length(x)` is below this, `f` is applied
#'   directly. Defaults to 10000.
#' @param paired_arguments Optional character vector of additional argument
#'   names that, if supplied, should be subset in parallel with unique values of
#'   `x`.
#' @param ... Additional arguments passed to `f`.
#'
#' @return A vector with the result of `f(x, ...)`.
#' @keywords internal
util_vector_operation_optim <- function(
    x, f,
    ratio_threshold = 0.7,
    length_threshold = 10000,
    paired_arguments = NULL,
    ...
) {

  n_total <- length(x)
  if (n_total == 0) return(x)
  if (n_total < length_threshold) return(f(x, ...))

  # Identify unique values (preserve order of first appearance)
  unique_idx <- !duplicated(x)
  x_unique <- x[unique_idx]
  n_distinct <- length(x_unique)
  if ((n_distinct / n_total) < ratio_threshold) {

    # Collect all arguments to pass to f
    arg_list <- c(list(x = x_unique), list(...))
    if (!is.null(paired_arguments)) {
      # Subset paired arguments in sync with x_unique
      arg_list[paired_arguments] <- lapply(
        arg_list[paired_arguments],
        function(arg) if (!is.null(arg)) arg[unique_idx] else arg
      )
    }

    results_unique <- do.call(f, arg_list)
    # Map results back to the original vector
    return(results_unique[match(x, x_unique)])
  } else {
    return(f(x, ...))
  }
}




#' Infer the most frequent character in a vector
#'
#' This function takes a character vector and returns the most frequently occurring
#' character. If there are multiple characters with the same maximum frequency,
#' it returns the first one in alphabetical order. If the input vector is empty,
#' it returns `NA_character_`.
#'
#' @param x A character vector.
#'
#' @return The most frequent character in the vector, or `NA_character_` if the
#' vector is empty.
#'
#' @keywords internal
util_most_frequent <- function(x) {
  if (length(x) == 0) {
    return(NA_character_)
  }
  counts <- table(x)
  most_frequent <- names(counts[counts == max(counts)])

  return(max(most_frequent))
}



#' Make Vector Elements Unique with Minimal Suffixes
#'
#' Adds numeric suffixes to duplicated elements in a character or numeric
#' vector, using the minimal number of changes required to ensure all elements
#' are unique. The first occurrence of each unique value is left unchanged;
#' subsequent duplicates receive a "_2", "_3", etc. suffix.
#'
#' @param x A character or numeric vector.
#'
#' @return A character vector the same length as \code{x}, with duplicates made
#'   unique by the minimal addition of integer suffixes.
#'
#' @examples
#' \dontrun{
#' util_make_unique_minimal(c("a", "b", "a", "a", "c", "b"))
#' # [1] "a" "b" "a_2" "a_3" "c" "b_2"
#'
#' util_make_unique_minimal(c(1, 2, 2, 3, 1))
#' # [1] "1" "2" "2_2" "3" "1_2"
#' }
#'
#' @keywords internal
util_make_unique_minimal <- function(x) {

  # Note: This differs from make.unique(sep = "_")
  # util_make_unique_minimal: adds suffix (_1, _2, ...) to all duplicates, incl.
  # first occurrence. make.unique: first occurrence unchanged; suffix added to
  # later duplicates.
  # Example:
  #   Input: c("a", "b", "b", "c", "a")
  #   util_make_unique_minimal: "a_1", "b_1", "b_2", "c", "a_2"
  #   make.unique:              "a", "b", "b_1", "c", "a_1"

  # For each value in x, assign a sequential index within its group
  idx <- stats::ave(seq_along(x), x, FUN = seq_along)
  # Identify which values are duplicated (appear more than once)
  is_dup <- stats::ave(x, x, FUN = length) > 1
  # If duplicated, append index to make unique; else keep as is
  ifelse(is_dup, paste0(x, "_", idx), x)
}



#' Assign Group Ranks After Multi-Column Ordering
#'
#' Returns an integer vector assigning contiguous group ranks (1, 2, ..., n)
#' after ordering the inputs by all columns, supporting both numeric and
#' character sorting. Tied rows are assigned the same rank. Supports a variable
#' number of arguments; all must be the same length.
#'
#' The sequence of input vectors determines the sorting hierarchy: the first
#' argument is the primary sort key, the second breaks ties, and so on.
#'
#' @section Relation to \code{util_natural_segment_rank}: Both
#'   \code{util_hierarchical_rank()} and \link{util_natural_segment_rank} assign
#'   dense ranks based on multi-level, segment-wise numeric ordering.
#'   \code{util_hierarchical_rank()} uses multiple input vectors (columns),
#'   while \link{util_natural_segment_rank} uses a single character vector split
#'   into numeric segments (e.g., "1_10").
#'
#'   When all segments or vectors are numeric and no input is cleaned to blank,
#'   \code{as.integer(util_natural_segment_rank(paste(x1, x2, sep = "_")))} will
#'   give the same result as \code{util_hierarchical_rank(x1, x2)}. However, the
#'   two functions can differ if:
#' \itemize{
#'   \item The input to \code{util_natural_segment_rank()} contains non-numeric
#'   or extra characters (these are removed during cleaning).
#'   \item After cleaning, some values become blank (""), which are assigned
#'   rank "" (0-equivalent), and ranking starts from 1 for the rest.
#' }
#'
#' @section Benchmark: For 10,000 rows, \code{util_hierarchical_rank()} is about
#'   10x faster (median ~1.8 ms) than \code{util_natural_segment_rank()} (median
#'   ~22.8 ms), because the latter incurs additional overhead from string
#'   splitting and conversion.
#'
#' @param ... Vectors (numeric or character), all of the same length, to rank
#'   by—first argument has highest priority, then second, etc.
#' @param decreasing Logical; if TRUE, sort in decreasing order. Default is
#'   FALSE.
#'
#' @return An integer vector of dense group ranks, length equal to input
#'   vectors.
#'
#' @examples
#' \dontrun{
#' x1 <- c("b", "a", "a", "b", "c", "b")
#' x2 <- c(2, 3, 2, 2, 1, 2)
#' util_hierarchical_rank(x1, x2)
#'
#' n1 <- c(5, 1, 1, 5)
#' n2 <- c(2, 3, 2, 2)
#' util_hierarchical_rank(n1, n2, decreasing = TRUE)
#'
#' # Same result as util_natural_segment_rank() if all segments are numeric and no blanks:
#' util_hierarchical_rank(x1 = 1:3, x2 = c(2, 10, 2)) # 1 3 2
#' as.integer(util_natural_segment_rank(paste(1:3, c(2, 10, 2), sep = "_"))) # 1 3 2
#' }
#'
#' @seealso \link{util_natural_segment_rank}
#' @keywords internal
util_hierarchical_rank <- function(..., decreasing = FALSE) {
  args <- list(...)
  n <- length(args[[1]])
  # Safety check: all arguments must have the same length
  if (any(vapply(args, length, 1L) != n)) {
    rlang::abort("All arguments must have the same length.", call = NULL)
  }
  # Combine inputs into a data frame for multi-column ordering
  df <- as.data.frame(args, stringsAsFactors = FALSE)
  # Compute the order of rows based on all columns and decreasing flag
  ord <- do.call(order, c(df, list(decreasing = decreasing)))
  # Build a key for each row, concatenating all columns with "|"
  key <- do.call(paste, c(df, sep = "|"))
  # Assign dense ranks to each unique key in order
  rank_by_key <- match(key[ord], unique(key[ord]))
  # Place the ranks back in the original order
  out <- integer(n)
  out[ord] <- rank_by_key
  out
}

#' Segment-wise Natural Numeric Rank for Underscore-separated Strings
#'
#' Returns a character vector assigning contiguous group ranks ("1", "2", ...,
#' "n") after segment-wise numeric ordering of underscore-separated strings.
#' Each segment between underscores is compared numerically, so "1_10_1" will
#' rank after "1_2_10", and so on. Handles any number of segments per entry.
#' Non-numeric characters are discarded, and non-numeric segments are treated as
#' NA.
#'
#' Unlike \code{util_hierarchical_rank()}, this function first cleans the input:
#' removes all characters except digits and underscores, collapses multiple
#' underscores, and trims leading/trailing underscores. If a string becomes
#' blank after cleaning, it is assigned rank "" (empty string), and actual
#' ranking starts at "1" for non-blank entries.
#'
#' This is useful for ranking IDs or codes like "1_2", "1_10", "2_1" in natural
#' numeric order rather than strict character order.
#'
#' @section Relation to \code{util_hierarchical_rank}: Both
#'   \code{util_natural_segment_rank()} and \link{util_hierarchical_rank} assign
#'   dense ranks based on multi-level, segment-wise numeric ordering. When all
#'   input values are valid numbers (no blanks after cleaning), then
#'   \code{as.integer(util_natural_segment_rank(paste(x1, x2, sep = "_")))} will
#'   produce the same result as \code{util_hierarchical_rank(x1, x2)}.
#'
#' @section Benchmark: For 10,000 rows, \code{util_hierarchical_rank()} is about
#'   10x faster (median ~1.8 ms) than \code{util_natural_segment_rank()} (median
#'   ~22.8 ms), because the latter incurs additional overhead from string
#'   splitting and conversion.
#'
#' @param x A character vector of underscore-separated strings, where each
#'   segment should be compared as a number. Non-numeric characters are removed
#'   before ranking.
#'
#' @return A character vector of dense group ranks ("1", "2", ...), same length
#'   as \code{x}. Entries that become blank after cleaning are assigned "".
#'
#' @examples
#' \dontrun{
#' util_natural_segment_rank(c("1_4_1", "1_5", "1_4_2", "1_40_1", "1_3",
#'                             "1_20"))
#' # [1] "2" "4" "3" "6" "1" "5"
#' x <- c("2", "3", "2_1", "3_1", "4_1", "5_1", "6_1", "7_1", "8", "9",
#'        "10", "1", "2_2", "3_2", "4_2", "5_2", "6_2", "7_2", "2", "3")
#' util_natural_segment_rank(x)
#'
#' # Cleaned blanks get rank "":
#' util_natural_segment_rank(c("abc", "3_2", "2_2", ""))
#' # [1] ""  "2" "1" ""
#' }
#'
#' @seealso \link{util_hierarchical_rank}
#' @keywords internal
util_natural_segment_rank <- function(x) {
  # 1. Clean x: retain only digits and underscores, collapse multiple
  # underscores, trim leading/trailing underscores
  x_clean <- stringr::str_replace_all(x, "[^0-9_]", "")
  x_clean <- stringr::str_replace_all(x_clean, "_+", "_")
  x_clean <- stringr::str_replace_all(x_clean, "^_+|_+$", "")

  # 2. Identify blanks after cleaning
  is_blank <- x_clean == ""
  result <- character(length(x))
  result[is_blank] <- ""  # assign "" for blanks

  # 3. For non-blanks, compute ranks
  if (any(!is_blank)) {
    x_valid <- x_clean[!is_blank]
    # Split each string at underscores into a list of numeric parts
    split_parts <- stringr::str_split(x_valid, "_")
    # Find the maximal segment count across all strings
    maxlen <- max(purrr::map_int(split_parts, length))
    # Pad each vector with NA and convert segments to integer
    padded <- purrr::map(
      split_parts,
      ~ as.integer(c(.x, rep(NA, maxlen - length(.x)))))
    # Create a data frame where each column is a segment
    ordmat <- do.call(rbind, padded)
    df <- as.data.frame(ordmat, stringsAsFactors = FALSE)
    # Compute the order of rows based on all segments
    o <- do.call(order, df)
    # Build a key for each row, concatenating all segments with "|"
    key <- do.call(paste, c(df, sep = "|"))
    # Assign dense ranks as character, starting from "1"
    r <- character(length(x_valid))
    r[o] <- as.character(match(key[o], unique(key[o])))
    result[!is_blank] <- r
  }

  result
}



util_rect_intersect_filter <- function(rcd1, rcd2, boundary){
  # Here rcd1 and rcd2 are RC-DF (row-col DF). Either of rcd1 or boundary are
  # optional. If rcd2 is not supplied boundary has to be provided.
  if(missing(rcd2)){
    bd2 <- boundary
  } else{
    bd2 <- rcd2 %>% dplyr::ungroup() %>% infer_block_boundary(exact = TRUE)
  }

  rcd1 %>%
    dplyr::filter(
      # Check if the row and col of rcd1 is within the boundary of rcd2
      .data$row >= bd2$r_min & .data$row <= bd2$r_max &
        .data$col >= bd2$c_min & .data$col <= bd2$c_max
    )
}


# Tiny function for area of rectangle given by two corners
util_area_of_rect <- function(r1, c1, r2, c2) {
  (abs(r1 - r2) + 1) * (abs(c1 - c2) + 1)
}


# Section : "Mostly used in Shiny" - functions ----

# Below functions which are mostly used in Shiny modules but can be used in
# generic use also

# Get the names of all attached packages in the search path
util_attached_packages <- function() {
  pkgs <- grep("^package:", search(), value = TRUE)
  sub("^package:", "", pkgs)
}

# Detach a package if it is attached to the search path
util_detach_pkg_if_attached <- function(pkg, unload = FALSE) {
  # Detach a package if it is attached to the search path.
  pkg_names <- paste0("package:", pkg)
  attached <- pkg_names[pkg_names %in% search()]
  lapply(attached, function(p) detach(p, unload = unload, character.only = TRUE))
  invisible(0)
}


# Convert cells_analysis to a near cells object (rc-df) like for plotting. Used
# in plot.cells_analysis.
util_convert_cells_analysis_for_plot <- function(
    ca,
    attr_cols = c("data_gid", "nice_header_name", "header_orientation_tag"),
    focus_on_data_blocks = NULL,
    color_attrs_separately = FALSE,
    show_values_in_cells = FALSE) {
  # Convert cells_analysis to a cells object like for plotting.


  # Step 1: Process the attribute data map
  # First extract the attribute data
  attr_data <- ca$attr_data_map

  if(!is.null(focus_on_data_blocks)) {
    # If focus_on_data_blocks is provided, filter the attr_data_map
    attr_data <- attr_data %>%
      dplyr::filter(.data$data_gid %in% focus_on_data_blocks)
  }

  # Select specified columns and combine them with ":" separator
  # (avoiding the dot notation)
  if (!show_values_in_cells){
    # In case cells value is not requested, then we can create values for plot
    # which are specific to cells-analysis
    selected_columns <- dplyr::select(attr_data, dplyr::all_of(attr_cols))
    combined_values <- apply(selected_columns, 1, paste, collapse = ":")
  } else {
    # If show_values_in_cells is TRUE, then we need to use the value column
    # from original data later. Here creating a dummy combined_values.
    combined_values <- ""
  }


  # Create attribute data with combined values
  if(color_attrs_separately) {
    # If color_attrs_separately is TRUE, then we need to create a tag for each
    # attribute groups to mark them differently in plot.
    attr_data_with_combined_values <- attr_data %>%
      dplyr::group_by(.data$data_gid) %>%
      dplyr::mutate(tag=util_hierarchical_rank(.data$attr_gid)) %>%
      dplyr::ungroup() %>%
      # create attr_1 attr_2 etc. based on tag
      dplyr::mutate(val = combined_values, type = paste0("attr_", .data$tag)) %>%
      # Extract only the necessary columns for the plot
      dplyr::distinct(.data$row, .data$col, value = .data$val, .data$type)

    # Create a scale_fill_map for the attributes
    all_attrs  <- unique(attr_data_with_combined_values$type)
    # We can order them nicely based on the natural segment rank
    all_attrs <- all_attrs[order(util_natural_segment_rank(all_attrs))]
    # Create color-ramp
    pal <- grDevices::colorRampPalette(
      c("#F8766D","#F2E88A")
    )
    scale_fill_map <- pal(length(all_attrs))
    names(scale_fill_map) <- all_attrs

    # Add color for data blocks
    scale_fill_map <- c(c(data = "#00BFC4"), scale_fill_map)

  } else {
    # Otherwise, just take the values without creating separate tags
    attr_data_with_combined_values <- attr_data %>%
      dplyr::mutate(val = combined_values) %>%
      # Extract only the necessary columns for the plot
      dplyr::distinct(.data$row, .data$col, value = .data$val, type = "attr")

    scale_fill_map <- NULL
  }


  # Step 2: Prepare the data blocks
  # Extract the relevant columns from data_blocks

  if(!is.null(focus_on_data_blocks)) {
    # If focus_on_data_blocks is provided, filter the data_blocks
    data_blocks_this <- ca$data_blocks %>%
      dplyr::filter(.data$data_gid %in% focus_on_data_blocks)
  } else {
    # Otherwise, use the entire data_blocks
    data_blocks_this <- ca$data_blocks
  }

  # Format the data blocks for plotting
  data_blocks_this <- data_blocks_this %>%
    dplyr::distinct(.data$row, .data$col, value = .data$data_gid, type = "data")


  # Step 3: Combine both data-sets
  combined_data <- dplyr::bind_rows(
    attr_data_with_combined_values,
    data_blocks_this
  )

  # Display original cells values in the cells instead of the cell analysis
  # induced fields
  if (show_values_in_cells) {
    # If show_values_in_cells is TRUE, then we need to show the values in cells
    # i.e. the value column should contain the actual values from data_blocks
    combined_data <- combined_data %>%
      dplyr::select(-"value") %>%
      dplyr::left_join(
        ca$original_sheet[c("row","col","value")],
        by = c("row", "col")) %>%
      dplyr::group_by(.data$row, .data$col) %>%
      # Take one from each group
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }


  return(list(combined_data = combined_data,
              scale_fill_map = scale_fill_map))
}


# Trackback utility functions ----
util_get_traceback_composition <- function(ca){

  ca_trace <- ca

  ca_trace$original_sheet <- ca_trace$original_sheet %>%
    dplyr::mutate(value = .data$row + 1i *.data$col )

  lout <- list(source = ca$original_sheet)

  lout$comp_orig <- compose(ca, simplify = FALSE)
  lout$comp_trace <- compose(ca_trace)

  lout$comp_orig_collate <- collate_columns(
    lout$comp_orig,
    dicard_cell_address = FALSE)

  lout$comp_orig <- dplyr::bind_rows(
    unclass(lout$comp_orig)
  )

  lout

}

util_traceback <- function(
    cell_row , cell_col, traceback_render,
    do_collated = TRUE) {

  this_row_in_composition <- traceback_render$comp_trace %>%
    dplyr::filter(.data$row == cell_row & .data$col == cell_col)

  # Early return if no row found
  if (NROW(this_row_in_composition) == 0) {
    return(NULL)
  }

  # Initialize output list
  lout <- list()

  # Remove data_gid, row, col and value
  lout$connected_cells <- this_row_in_composition %>%
    dplyr::select(-"data_gid", -"row", -"col", -"value") %>%
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "colname",
      values_to = "cell_address"
    ) %>%
    dplyr::mutate(row = Re(.data$cell_address),
                  col = Im(.data$cell_address)) %>%
    dplyr::select(-"cell_address")

  # Now do for collated composition (this is little bit different as collate
  # column reply on cell contents)

  if(do_collated){

    this_row_in_collated_composition <- traceback_render$comp_orig_collate %>%
      dplyr::filter(.data$row == cell_row & .data$col == cell_col)

    connected_collated_cells <- this_row_in_collated_composition %>%
      dplyr::select(-"data_gid", -"row", -"col", -"value") %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "colname",
        values_to = "value"
      )

    # A Special Join:
    # In order to do join but retaining all cells (in case value has same string)
    # following adjustment is required

    # See this:-
    #
    # df1 <- data.frame(name =c("x1","x2","x3"), value = c("a","a","b")) df2 <-
    # data.frame(value = c("a","a","b"), row= c(1,2,3), col=c(3,4,4))
    #
    # df1 <- df1 %>% dplyr::group_by(.data$value) %>% dplyr::mutate(idx =
    # dplyr::row_number()) df2 <- df2 %>% dplyr::group_by(.data$value) %>%
    # dplyr::mutate(idx = dplyr::row_number())
    #
    # df1 %>% dplyr::left_join(df2, by = c("value","idx")) %>%
    # dplyr::select(-"idx") %>% dplyr::ungroup()

    connected_cells_with_value <- lout$connected_cells %>%
      dplyr::inner_join(traceback_render$source[c("row","col","value")],
                        by = c("row","col"))

    connected_cells_with_value <- connected_cells_with_value %>%
      dplyr::group_by(.data$value) %>%
      dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::ungroup()

    connected_collated_cells_with_value <- connected_collated_cells %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::group_by(.data$value) %>%
      dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::ungroup()

    connected_collated_cells_with_value <- connected_collated_cells_with_value %>%
      dplyr::left_join(
        connected_cells_with_value[c("row","col","value","idx")],
        by = c("value", "idx")
      ) %>%
      dplyr::select(-"idx")

    # Join back with connected_collated_cells
    lout$connected_collated_cells <- connected_collated_cells %>%
      dplyr::left_join(
        connected_collated_cells_with_value[c("colname", "row", "col")],
        by = c("colname")
      ) %>%
      dplyr::distinct()
  }


  return(lout)

}


util_auto_round <- function(x) {
  num_x <- suppressWarnings(as.numeric(x))
  abs_x <- abs(num_x)

  digits <- dplyr::case_when(
    is.na(abs_x) ~ 0,

    abs_x == 0 ~ 0,

    # Condition: Fractional part is 0
    abs_x == round(abs_x, 0) ~ 0,

    # Condition: Absolute value > 100
    abs_x > 100 ~ 0,

    # Condition: Between 1 and 100
    abs_x >= 1 ~ 2,

    # Condition: Fractional (< 1), but survives rounding to 2 digits
    round(abs_x, 2) > 0 ~ 2,

    # Condition: Fractional (< 1) and needs deeper rounding
    TRUE ~ abs(floor(log10(abs_x)))
  )

  rounded_num <- round(num_x, digits)

  xo <- format(rounded_num, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  xo[is.na(num_x)] <- x[is.na(num_x)]
  xo
}
