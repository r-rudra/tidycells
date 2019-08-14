


#' Collate Columns Based on Content
#'
#' @description After [`compose_cells`][compose_cells()], this function rearranges and rename attribute-columns in order to
#' make columns properly aligned, based on the content of the columns.
#'
#' @param composed_data output of [`compose_cells`][compose_cells()] (preferably not processed)
#' @param combine_threshold a numerical threshold (between 0-1) for content-based collation of columns. (Default 1)
#' @param rest_cols number of rest columns (beyond `combine_threshold` joins these many numbers of columns to keep)
#' @param retain_other_cols whether to keep other intermediate (and possibly not so important) columns. (Default `FALSE`)
#' @param retain_cell_address whether to keep columns like (`row`, `col`, `data_block`).
#' This may be required for [`traceback`][cell_composition_traceback()] (Default `FALSE`)
#'
#' @return A column collated data.frame
#'
#' @details
#' * **Dependency on _stringdist_**: If you have \code{\link[stringdist:stringdist-package]{stringdist}} installed,
#' the approximate string matching will be enhanced. There may be variations in outcome if you have `stringdist`
#' vs if you don't have it.
#' * **Possibility of randomness**: If the attribute column is containing many distinct values, then a column representative sample will be drawn.
#' Hence it is always recommended to [`set.seed`][base::set.seed()] if reproducibility is a matter of concern.
#'
#' @export
#'
#' @examples
#'
#' d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>%
#'   readRDS()
#' d <- numeric_values_classifier(d)
#' da <- analyze_cells(d)
#'
#' dc <- compose_cells(da, print_attribute_overview = TRUE)
#'
#' collate_columns(dc)
collate_columns <- function(composed_data,
                            combine_threshold = 1,
                            rest_cols = Inf,
                            retain_other_cols = FALSE,
                            retain_cell_address = FALSE) {
  ok <- FALSE

  defcols_this <- defcols
  if (is.data.frame(composed_data)) {
    if (!utils::hasName(composed_data, "table_tag")) {
      defcols_this <- setdiff(defcols_this, "table_tag")
    }
    if (all(utils::hasName(composed_data, defcols_this))) {
      ok <- TRUE

      if (utils::hasName(composed_data, "table_tag")) {
        dcl <- composed_data %>%
          group_by(data_block, table_tag)
      } else {
        dcl <- composed_data %>%
          group_by(data_block)
      }

      dcl <- dcl %>%
        group_split() %>%
        map(~ {
          .d <- ungroup(.x)
          this_cols <- colnames(.d)
          nm_cols <- this_cols[stringr::str_detect(this_cols, "row|col|corner")]
          nm_cols <- setdiff(nm_cols, defcols_this)
          if (length(nm_cols) == 0) {
            nm_cols <- setdiff(this_cols, defcols_this)
          }
          .d <- .d[c(defcols_this, nm_cols)]
          na_c <- .d %>% map_lgl(~ is.na(.x) %>% all())
          .d[!na_c]
        })
    }
  } else {
    # data.frame is a list,  first data.frame check is required
    if (is.list(composed_data)) {
      if (all(map_lgl(composed_data, is.data.frame))) {
        if (!any(map_lgl(composed_data, ~ utils::hasName(.x, "table_tag")))) {
          defcols_this <- setdiff(defcols_this, "table_tag")
        }
        if (all(map_lgl(composed_data, ~ all(utils::hasName(.x, defcols_this))))) {
          ok <- TRUE
          dcl <- composed_data
        }
      }
    }
  }



  if (!ok) {
    abort("The argument composed_data has to be output of compose_cells. Given composed_data has no known format.")
  }

  if (length(dcl) == 1) {
    out_d <- dcl[[1]]

    restcols <- setdiff(colnames(out_d), defcols_this)
    if (length(restcols) > 0) {
      cn_map_0 <- tibble(cn = restcols) %>%
        mutate(is_major = stringr::str_detect(tolower(cn), "major")) %>%
        arrange(cn) %>%
        mutate(sn = seq_along(cn), sn_m = sn + is_major * (10^10)) %>%
        arrange(desc(sn_m)) %>%
        mutate(fsn = seq_along(cn), new_cn = paste0("collated_", fsn)) %>%
        select(cn, new_cn)

      for (i in seq_along(cn_map_0$cn)) {
        colnames(out_d)[which(colnames(out_d) == cn_map_0$cn[i])] <- cn_map_0$new_cn[i]
      }
    }
  } else {
    out_d <- dcl %>% reduce(reduce_2dfs,
      combine_th = combine_threshold,
      rest_cols = rest_cols,
      retain_other_cols = retain_other_cols
    )
  }





  if (!retain_cell_address) {
    out_d <- out_d[setdiff(colnames(out_d), c("row", "col", "data_block"))]
  }

  out_d[sort(colnames(out_d))]
}
