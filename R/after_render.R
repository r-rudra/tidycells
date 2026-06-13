#' Rename data-frame columns by string / regex matching of their values
#'
#' @description
#' Scores every valid candidate column of `d` against a set of *nodes* (named
#' groups of literal target strings and/or regex patterns), assigns the
#' best-matching column to each node (optimally or greedily), and renames it to
#' the node's name.
#'
#' @details
#' Each (column, node) score lies in `[0, 1]` and is computed over the column's
#' **unique** values only. String nodes (`name_map`) combine a word-overlap
#' reward, a whole-phrase containment reward, and a leftover-word penalty. Regex
#' nodes (`name_map_regex`) use a pattern-match reward scaled by matched
#' character coverage (no word-overlap term). For both families the column score
#' is `mean(score over matched uniques) * (1 - proportion non-matched)`.
#'
#' Before scoring, columns that are entirely `NA` / empty and node targets that
#' are `NA` / zero-length are dropped; a node left with no targets is dropped
#' too. If no valid column or no valid node survives, `d` is returned unchanged
#' with a warning.
#'
#' Assignment is `"LSAP"` (default, optimal — maximises the total score via
#' `clue::solve_LSAP`) or `"greedy"` (repeatedly take the global best pair).
#' A winning pair scoring below `min_score` is reported and left unmapped rather
#' than forcing a meaningless rename, so its node and column keep clear.
#'
#' Performance: column unique-values and node targets/patterns are tokenised /
#' compiled once and cached, so the per-cell cost is just small set operations
#' and pattern matches — suitable for wide and/or long data frames.
#'
#' @param d A data frame whose columns are to be renamed.
#' @param name_map Named list of literal target strings; each name becomes a
#'   candidate new column name. Scored with word-overlap + whole-phrase.
#' @param name_map_regex Named list of regex patterns, same shape as `name_map`.
#'   Scored with pattern-match + matched-coverage only (no word-overlap term).
#'   Defaults to an empty list.
#' @param include_cols Optional character vector of columns to restrict the
#'   search to. `NULL` (default) considers every column of `d`.
#' @param exclude_cols Optional character vector of columns never to be mapped or
#'   renamed (e.g. key columns). Applied after `include_cols`.
#' @param assignment_method `"LSAP"` (default, optimal via `clue::solve_LSAP`) or
#'   `"greedy"`. If `clue` is unavailable, falls back to greedy with a warning.
#' @param w_word Weight in `[0, 1]` on the word-overlap reward (fraction of a
#'   target's words present in the entry). String nodes only. Default `0.5`.
#' @param w_phrase Weight in `[0, 1]` on the whole-phrase reward (one entry is a
#'   substring of a target, or vice versa). String nodes only; usually set so
#'   `w_word + w_phrase == 1`. Default `0.5`.
#' @param lambda Penalty strength in `[0, 1]` on unmatched content — leftover
#'   words (string nodes) or uncovered characters (regex nodes). `0` disables,
#'   `1` applies fully. Default `0.5`.
#' @param min_score Floor in `[0, 1]` for a valid mapping. Winning pairs below it
#'   are warned about and left unmapped. Default `0.1`.
#' @param case_sensitive Logical. If `FALSE` (default) matching is
#'   case-insensitive; if `TRUE`, caseless matching is disabled.
#' @param keep_details Logical. If `TRUE` the result carries
#'   `attr(., "score_matrix")` (nodes x columns) and `attr(., "mapping")`.
#'   Default `FALSE`.
#' @param summary_msg Logical. If `TRUE` (default) emit a message listing which
#'   `name_map` / `name_map_regex` nodes were not mapped and which candidate
#'   columns kept their original names.
#' @param warn Logical. If `TRUE` (default) and `assignment_method = "greedy"`,
#'   warn when a step's best positive score is tied (resolved by first match).
#'   Ignored for LSAP. *(Warning for other cases also managed by same argument)*
#' @param reg_ex_match_bias Numeric. A tie-breaker bonus applied to regex match
#'   scores based on the absolute number of matched characters. Default `0.1`.
#'
#' @return `d` with mapped columns renamed to their node names. Excluded,
#'   non-included, invalid, and unmapped columns keep their original names.
#'
#' @export
#'
#' @examples
#' d <- tibble::tibble(
#'   id        = 1:5,                                              # key col
#'   region    = c("North", "South", "East", "West", "North"),     # key col
#'   category  = c("GVA at Basic Prices", "Agriculture", "Mining",
#'                 "Manufacturing", "GVA at Basic Prices"),
#'   statement = c("Statement 1", "Statement 2", "Statement 1",
#'                 "Statement 3", "Statement 2"),
#'   qtr       = c("2024-Q1", "2024-Q2", "2024-Q3", "2025-Q1", "2025-Q2"),
#'   isin      = c("INE001A01036", "INE002B02045", "INE003C03012",
#'                 "INE004D04087", "INE005E05063"),
#'   note      = c("foo", "bar", "baz", "qux", "quux")             # matches nothing
#' )
#' name_map <- list(
#'   cat   = c("GVA at Basic Prices", "Agriculture, Forestry & Fishing"),
#'   info  = c("Statement 1", "Statement 2", "Statement 3"),
#'   extra = "Completely Unrelated Target Phrase"   # string node: will NOT map
#' )
#' name_map_regex <- list(
#'   period = "^\\d{4}-Q[1-4]$",    # -> qtr
#'   isin   = "^INE[A-Z0-9]{9}$",   # -> isin
#'   pin    = "^\\d{6}$"            # regex node: will NOT map
#' )
#' out <- rename_by_content(
#'   d, name_map, name_map_regex,
#'   exclude_cols = c("id", "region"),
#'   assignment_method = "LSAP",
#'   warn = FALSE
#' )
#' names(out)
rename_by_content <- function(
    d,
    name_map,
    name_map_regex    = list(),
    include_cols      = NULL,
    exclude_cols      = NULL,
    assignment_method = c("LSAP", "greedy"),
    w_word            = 0.5,
    w_phrase          = 0.5,
    lambda            = 0.5,
    min_score         = 0.1,
    case_sensitive    = FALSE,
    keep_details      = FALSE,
    summary_msg       = TRUE,
    warn              = TRUE,
    reg_ex_match_bias = 0.1
) {

  assignment_method <- match.arg(assignment_method)

  if (is.null(name_map_regex)) name_map_regex <- list()

  if (assignment_method == "LSAP" && !pkg_is_available("clue") && warn) {
    warning("Package 'clue' not installed; falling back to greedy assignment.",
            call. = FALSE)
    assignment_method <- "greedy"
  }

  ## ---- drop invalid node targets / patterns (NA or zero-length) ------------
  name_map_in       <- name_map
  name_map_regex_in <- name_map_regex

  clean_strings <- function(v) {
    v <- as.character(v)
    v <- v[!is.na(v)]
    v[nzchar(stringr::str_squish(v))]
  }

  clean_patterns <- function(v) {
    v <- as.character(v)
    v <- v[!is.na(v)]
    v[nzchar(v)]
  }

  name_map <- name_map |>
    purrr::map(clean_strings) |>
    purrr::keep(\(x) length(x) > 0)

  name_map_regex <- name_map_regex |>
    purrr::map(clean_patterns) |>
    purrr::keep(\(x) length(x) > 0)

  ## ---- candidate columns (include / exclude / validity) --------------------
  all_cols <- names(d)
  unknown  <- setdiff(c(include_cols, exclude_cols), all_cols)

  if (length(unknown) && warn) {
    warning("Ignoring unknown column(s): ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }

  cand_req <- if (is.null(include_cols)) all_cols else intersect(include_cols, all_cols)
  cand_req <- setdiff(cand_req, exclude_cols)

  col_valid <- function(cl) {
    s <- as.character(d[[cl]])
    s <- s[!is.na(s)]
    length(s) > 0L && any(nzchar(stringr::str_squish(s)))
  }

  cand <- cand_req[purrr::map_lgl(cand_req, col_valid)]
  dropped_cols <- setdiff(cand_req, cand)

  ## ---- node table (string + regex); names unique across both ---------------
  nodes <- dplyr::bind_rows(
    tibble::tibble(node = as.character(names(name_map)),       kind = "string"),
    tibble::tibble(node = as.character(names(name_map_regex)), kind = "regex")
  )

  nodes <- stats::na.omit(nodes)

  dup <- unique(nodes$node[duplicated(nodes$node)])
  if (length(dup)) {
    stop("Node name(s) present in more than one map: ",
         paste(dup, collapse = ", "), call. = FALSE)
  }

  ## ---- early exit ----------------------------------------------------------
  if ((!length(cand) || !nrow(nodes)) && warn) {
    warning("No valid ", if (!length(cand)) "candidate columns" else "nodes",
            " to match; returning `d` unchanged.", call. = FALSE)
    return(d)
  }

  if (nrow(nodes) > length(cand) && warn) {
    warning(nrow(nodes), " nodes but only ", length(cand),
            " candidate column(s); some nodes will stay unmapped.", call. = FALSE)
  }

  ## ---- normalisation helpers (close over case_sensitive) -------------------
  norm_str <- function(x) {
    x <- stringr::str_squish(tidyr::replace_na(as.character(x), ""))
    if (!case_sensitive) x <- stringr::str_to_lower(x)
    x
  }

  tok_str <- function(x) {
    x |>
      stringr::str_split("[^[:alnum:]]+") |>
      purrr::map(\(w) unique(w[w != ""]))
  }

  ## ---- caches: tokenise / compile ONCE -------------------------------------
  col_cache <- cand |>
    purrr::set_names() |>
    purrr::map(\(cl) {
      u   <- unique(d[[cl]])
      sN  <- norm_str(u)
      sRx <- stringr::str_squish(tidyr::replace_na(as.character(u), ""))
      list(s_norm = sN, tok = tok_str(sN), s_rx = sRx, nch = nchar(sRx))
    })

  str_node_cache <- purrr::map(name_map, \(tg) {
    ts <- norm_str(tg)
    list(str = ts, tok = tok_str(ts))
  })

  rx_node_cache <- purrr::map(name_map_regex, \(pl) {
    list(pat = purrr::map(pl, \(p) stringr::regex(p, ignore_case = !case_sensitive)))
  })

  ## ---- scorers (shared aggregation) ----------------------------------------
  finalize <- function(es) {
    if (!length(es)) return(0)
    matched <- es[es > 0]
    (if (!length(matched)) 0 else mean(matched)) * (1 - mean(es == 0))
  }

  score_str <- function(col, node) {
    purrr::map2_dbl(col$tok, col$s_norm, \(et, es) {
      if (!length(et)) return(0)

      purrr::map2_dbl(node$tok, node$str, \(tt, ts) {
        inter    <- length(intersect(et, tt))
        word_hit <- if (!length(tt)) 0 else inter / length(tt)
        phrase   <- as.numeric(
          nzchar(es) & nzchar(ts) &
            (stringr::str_detect(es, stringr::fixed(ts)) |
               stringr::str_detect(ts, stringr::fixed(es)))
        )

        (w_word * word_hit + w_phrase * phrase) * (1 - lambda * (length(et) - inter) / length(et))
      }) |> max()
    }) |> finalize()
  }

  score_rx <- function(col, node) {
    purrr::map2_dbl(col$s_rx, col$nch, \(es, nch) {
      if (!nzchar(es)) return(0)

      purrr::map_dbl(node$pat, \(p) {
        # 1. Extract matches and drop empty strings
        m <- stringr::str_extract_all(es, p)[[1]]
        m <- m[nzchar(m)]
        if (!length(m)) return(0)

        # 2. Count absolute matched characters
        matched_chars <- sum(nchar(m))

        # 3. Calculate original [0, 1] coverage score
        coverage_penalty <- 1 - min(matched_chars, nch) / nch
        base_score <- 1 - (lambda * coverage_penalty)

        # 4. Apply length tie-breaker bonus
        base_score + (matched_chars * reg_ex_match_bias)
      }) |> max()
    }) |> finalize()
  }

  ## ---- m x n score table ---------------------------------------------------
  scores <- tidyr::expand_grid(column = cand, node = nodes$node) |>
    dplyr::left_join(nodes, by = "node") |>
    dplyr::mutate(score = purrr::pmap_dbl(
      list(column, node, kind),
      \(cl, nd, kd) {
        if (kd == "string") {
          score_str(col_cache[[cl]], str_node_cache[[nd]])
        } else {
          score_rx(col_cache[[cl]], rx_node_cache[[nd]])
        }
      }
    ))

  ## ---- assignment ----------------------------------------------------------
  assign_greedy <- function(scores) {
    step <- function(acc, .i) {
      rem <- acc$remaining
      if (nrow(rem) == 0L) return(acc)

      top <- dplyr::slice_max(rem, score, n = 1, with_ties = TRUE)

      if (warn && nrow(top) > 1L && top$score[1] > 0) {
        warning("Tie at score ", round(top$score[1], 4), " between { ",
                paste0(top$column, " <-> ", top$node, collapse = " | "),
                " }. Resolved by first match.", call. = FALSE)
      }

      pick <- dplyr::slice(top, 1)

      list(
        remaining = dplyr::filter(rem, column != pick$column, node != pick$node),
        picks     = dplyr::bind_rows(acc$picks, pick)
      )
    }

    purrr::reduce(
      seq_len(nrow(nodes)),
      step,
      .init = list(remaining = scores, picks = scores[0, ])
    )$picks |>
      dplyr::select(node, column)
  }

  assign_lsap <- function(scores) {
    mat <- scores |>
      dplyr::select(node, column, score) |>
      tidyr::pivot_wider(names_from = column, values_from = score) |>
      tibble::column_to_rownames("node") |>
      as.matrix()

    rn <- rownames(mat)
    cn <- colnames(mat)

    if (nrow(mat) <= ncol(mat)) {
      sol <- clue::solve_LSAP(mat, maximum = TRUE)
      tibble::tibble(node = rn, column = cn[as.integer(sol)])
    } else {
      sol <- clue::solve_LSAP(t(mat), maximum = TRUE)
      tibble::tibble(column = cn, node = rn[as.integer(sol)])
    }
  }

  pairs <- if (assignment_method == "LSAP") assign_lsap(scores) else assign_greedy(scores)
  picks <- dplyr::left_join(pairs, scores, by = c("node", "column"))

  ## ---- threshold gate ------------------------------------------------------
  applied <- dplyr::filter(picks, score >= min_score)
  weak    <- dplyr::filter(picks, score <  min_score)

  if (nrow(weak) && warn) {
    warning("Match(es) below min_score (", min_score, ") left unmapped: ",
            paste0(weak$node, " <- ", weak$column, " (", round(weak$score, 3), ")",
                   collapse = "; "), call. = FALSE)
  }

  ## ---- rename --------------------------------------------------------------
  d_out <- dplyr::rename(d, dplyr::all_of(stats::setNames(applied$column, applied$node)))

  ## ---- summary -------------------------------------------------------------
  if (summary_msg) {
    fmt <- function(x) if (length(x)) paste(x, collapse = ", ") else "(none)"

    msg_parts <- c(
      paste0("rename_by_content [", assignment_method, "]\n"),
      paste0("  mapped: ",
             if (nrow(applied)) paste0(applied$column, " -> ", applied$node, collapse = ", ")
             else "(none)", "\n"),
      paste0("  unmapped name_map nodes:       ", fmt(setdiff(names(name_map), applied$node)), "\n"),
      paste0("  unmapped name_map_regex nodes: ", fmt(setdiff(names(name_map_regex), applied$node)), "\n"),
      paste0("  candidate columns retained:    ", fmt(setdiff(cand, applied$column)))
    )

    if (length(dropped_cols)) {
      msg_parts <- c(msg_parts, paste0("\n  ignored (empty/NA) columns:    ", fmt(dropped_cols)))
    }

    ignored_nodes <- c(
      setdiff(names(name_map_in), names(name_map)),
      setdiff(names(name_map_regex_in), names(name_map_regex))
    )

    if (length(ignored_nodes)) {
      msg_parts <- c(msg_parts, paste0("\n  ignored (empty/NA) nodes:      ", fmt(ignored_nodes)))
    }

    message(paste0(msg_parts, collapse = ""))
  }

  if (keep_details) {
    attr(d_out, "score_matrix") <- scores |>
      dplyr::select(node, column, score) |>
      tidyr::pivot_wider(names_from = column, values_from = score) |>
      tibble::column_to_rownames("node") |>
      as.matrix()

    attr(d_out, "mapping") <- applied
  }

  d_out
}
