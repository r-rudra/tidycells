
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
#' is `weighted_mean(score over matched uniques) * (1 - weighted proportion
#' non-matched) * node_coverage`, where each unique value is weighted by its
#' frequency in the column and `node_coverage` is the share of the node's
#' distinct targets matched by the column (so a column spanning more of the
#' node's values ranks higher). Node targets are de-duplicated; column
#' frequencies are not.
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
#' @param unite_other_cols Logical. If `TRUE`, all columns not mapped (and not excluded via `exclude_cols`) are united into a single column. Default `FALSE`.
#' @param united_others_col_name Character. The name of the newly united column if `unite_other_cols` is `TRUE`. Default `"united_info"`.
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
    summary_msg       = FALSE,
    warn              = FALSE,
    reg_ex_match_bias = 0.1,
    unite_other_cols = FALSE,
    united_others_col_name = "united_info"
) {

  assignment_method <- match.arg(assignment_method)

  if (is.null(name_map_regex)) name_map_regex <- list()

  if (assignment_method == "LSAP" && !pkg_is_available("clue")) {
    if(warn) {
      warning("Package 'clue' not installed; falling back to greedy assignment.",
              call. = FALSE)
    }
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
  if ((!length(cand) || !nrow(nodes))) {
    if(warn){
      warning("No valid ", if (!length(cand)) "candidate columns" else "nodes",
              " to match; returning `d` unchanged.", call. = FALSE)
    }
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
  ## columns keep per-unique frequency weights (w); nodes de-duplicate targets.
  col_cache <- cand |>
    purrr::set_names() |>
    purrr::map(\(cl) {
      u   <- unique(d[[cl]])
      w   <- tabulate(match(d[[cl]], u), nbins = length(u))   # count of each unique
      sN  <- norm_str(u)
      sRx <- stringr::str_squish(tidyr::replace_na(as.character(u), ""))
      list(s_norm = sN, tok = tok_str(sN), s_rx = sRx, nch = nchar(sRx), w = w)
    })

  str_node_cache <- purrr::map(name_map, \(tg) {
    ts <- unique(norm_str(tg))                                # de-dup node targets
    list(str = ts, tok = tok_str(ts))
  })

  rx_node_cache <- purrr::map(name_map_regex, \(pl) {
    pl <- unique(pl)                                          # de-dup node patterns
    list(pat = purrr::map(pl, \(p) stringr::regex(p, ignore_case = !case_sensitive)))
  })

  ## ---- scorers (shared aggregation) ----------------------------------------
  ## weighted quality * (1 - weighted non-match) * node coverage.
  finalize <- function(es, w = rep(1, length(es)), coverage = 1) {
    if (!length(es)) return(0)
    matched <- es > 0
    q  <- if (any(matched)) stats::weighted.mean(es[matched], w[matched]) else 0
    nm <- sum(w[!matched]) / sum(w)
    q * (1 - nm) * coverage
  }

  score_str <- function(col, node) {
    n_t <- length(node$tok)

    cells <- purrr::map2(col$tok, col$s_norm, \(et, es) {
      if (!length(et)) return(rep(0, n_t))

      purrr::map2_dbl(node$tok, node$str, \(tt, ts) {
        inter    <- length(intersect(et, tt))
        word_hit <- if (!length(tt)) 0 else inter / length(tt)
        phrase   <- as.numeric(
          nzchar(es) & nzchar(ts) &
            (stringr::str_detect(es, stringr::fixed(ts)) |
               stringr::str_detect(ts, stringr::fixed(es)))
        )

        (w_word * word_hit + w_phrase * phrase) * (1 - lambda * (length(et) - inter) / length(et))
      })
    })

    mm <- do.call(rbind, cells)                       # uniques x targets
    finalize(
      es       = apply(mm, 1, max),                   # best target per unique
      w        = col$w,                               # frequency weights
      coverage = sum(apply(mm, 2, max) > 0) / n_t     # distinct node targets matched / total
    )
  }

  score_rx <- function(col, node) {
    n_p <- length(node$pat)

    cells <- purrr::map2(col$s_rx, col$nch, \(es, nch) {
      if (!nzchar(es)) return(rep(0, n_p))

      purrr::map_dbl(node$pat, \(p) {
        # 1. Extract matches and drop empty strings
        mt <- stringr::str_extract_all(es, p)[[1]]
        mt <- mt[nzchar(mt)]
        if (!length(mt)) return(0)

        # 2. Count absolute matched characters
        matched_chars <- sum(nchar(mt))

        # 3. Calculate original [0, 1] coverage score
        coverage_penalty <- 1 - min(matched_chars, nch) / nch
        base_score <- 1 - (lambda * coverage_penalty)

        # 4. Apply length tie-breaker bonus
        base_score + (matched_chars * reg_ex_match_bias)
      })
    })

    mm <- do.call(rbind, cells)                       # uniques x patterns
    finalize(
      es       = apply(mm, 1, max),
      w        = col$w,
      coverage = sum(apply(mm, 2, max) > 0) / n_p     # distinct patterns matched / total
    )
  }

  ## ---- m x n score table ---------------------------------------------------
  scores <- tidyr::expand_grid(column = cand, node = nodes$node) |>
    dplyr::left_join(nodes, by = "node") |>
    dplyr::mutate(score = purrr::pmap_dbl(
      list(.data$column, .data$node, .data$kind),
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

      top <- dplyr::slice_max(rem, .data$score, n = 1, with_ties = TRUE)

      if (warn && nrow(top) > 1L && top$score[1] > 0) {
        warning("Tie at score ", round(top$score[1], 4), " between { ",
                paste0(top$column, " <-> ", top$node, collapse = " | "),
                " }. Resolved by first match.", call. = FALSE)
      }

      pick <- dplyr::slice(top, 1)

      list(
        remaining = dplyr::filter(rem, .data$column != pick$column, .data$node != pick$node),
        picks     = dplyr::bind_rows(acc$picks, pick)
      )
    }

    purrr::reduce(
      seq_len(nrow(nodes)),
      step,
      .init = list(remaining = scores, picks = scores[0, ])
    )$picks |>
      dplyr::select("node", "column")
  }

  assign_lsap <- function(scores) {
    mat <- scores |>
      dplyr::select("node", "column", "score") |>
      tidyr::pivot_wider(names_from = "column", values_from = "score") |>
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
  applied <- dplyr::filter(picks, .data$score >= min_score)
  weak    <- dplyr::filter(picks, .data$score <  min_score)

  if (nrow(weak) && warn) {
    warning("Match(es) below min_score (", min_score, ") left unmapped: ",
            paste0(weak$node, " <- ", weak$column, " (", round(weak$score, 3), ")",
                   collapse = "; "), call. = FALSE)
  }

  ## ---- rename --------------------------------------------------------------
  d_out <- dplyr::rename(d, dplyr::all_of(stats::setNames(applied$column, applied$node)))

  if(unite_other_cols){
    non_mapped_cols <- colnames(d) |> setdiff(c(exclude_cols, applied$column))
    d_out <- d_out |>
      # 1. Unite into a hardcoded temporary column
      tidyr::unite(
        col = ".TEMP_UNITED_COL",
        tidyr::all_of(non_mapped_cols),
        remove = TRUE,
        na.rm = TRUE,
        sep = " "
      ) |>
      # 2. Rename it dynamically using standard evaluation
      dplyr::rename(
        tidyr::all_of(stats::setNames(".TEMP_UNITED_COL", united_others_col_name))
      )
  }

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
      dplyr::select("node", "column", "score") |>
      tidyr::pivot_wider(names_from = "column", values_from = "score") |>
      tibble::column_to_rownames("node") |>
      as.matrix()

    attr(d_out, "mapping") <- applied
  }

  d_out
}



# [AR : part of After Render]

#' Approximate matching of two character vectors
#'
#' Computes a composite similarity score for every `(x[i], y[j])` pair using
#' three complementary measures — word-overlap Jaccard, character-overlap
#' Jaccard, and longest-common-substring (LCS) ratio — then selects matches
#' according to the chosen relationship type and assignment method.
#'
#' @param x `character` vector of strings to map **from** (e.g. legacy labels).
#' @param y `character` vector of canonical target strings to map **to**.
#' @param relationship `"many_to_many"` *(default)* or `"one_to_one"`.
#'
#'   * `"many_to_many"` — each `x[i]` independently picks its best `y[j]`;
#'     multiple `x` elements may map to the same `y` and vice-versa.
#'     `assignment_method` is ignored in this mode (`"best"` is always used).
#'   * `"one_to_one"` — a strict bijection is enforced; each `x[i]` is paired
#'     with at most one `y[j]` and vice-versa.  Use `assignment_method` to
#'     choose between `"LSAP"` (globally optimal via `clue`) and `"greedy"`
#'     (fast heuristic).
#'
#' @param assignment_method `"LSAP"` *(default for one_to_one)* or `"greedy"`.
#'   Only consulted when `relationship = "one_to_one"`.
#'
#'   * `"LSAP"` — solves the linear sum assignment problem via
#'     [clue::solve_LSAP()], guaranteeing a globally optimal bijection.
#'     Requires the **clue** package.
#'   * `"greedy"` — iterates pairs by descending score and accepts a pair
#'     whenever both sides are still unmatched.  Faster but sub-optimal.
#'
#' @param case_sensitive `logical(1)`. If `FALSE` *(default)*, strings are
#'   folded to lower-case before scoring.
#' @param retain_num `logical(1)`. If `FALSE` *(default)*, digits are stripped
#'   before scoring; if `TRUE`, digits are kept.
#' @param add_acronyms `logical(1)`. If `TRUE` *(default)*, an acronym formed
#'   from the first letter of each word is appended to any string that contains
#'   two or more words (e.g. `"Private Final Consumption Expenditure"` →
#'   `"Private Final Consumption Expenditure PFCE"`).  The augmented string is
#'   then cleaned together with the original.
#' @param wt_word `numeric(1)`. Weight applied to the word-overlap Jaccard
#'   score. Default `0.55`.
#' @param wt_char `numeric(1)`. Weight applied to the character-overlap Jaccard
#'   score (computed only when the word or LCS score is non-zero). Default
#'   `0.15`.
#' @param wt_lcs `numeric(1)`. Weight applied to the LCS ratio score. Default
#'   `0.30`. `wt_word + wt_char + wt_lcs` must equal `1`.
#' @param lcs_min_chars `integer(1)`. Minimum length the longest common
#'   substring must reach to contribute to the score. Default `3L`.
#' @param score_threshold `numeric(1)`. Pairs whose composite score falls below
#'   this value are discarded (set to `NA` in the output). Default `0.10`.
#' @param emit_score_matrix `logical(1)`. If `TRUE`, the full `m × n` numeric
#'   score matrix is attached to the output as `attr(out, "score_matrix")`.
#'   Default `FALSE`.
#'
#' @return A [tibble::tibble()] with columns:
#'   \describe{
#'     \item{`x`}{Original value from `x` (`NA` for unmatched `y` elements).}
#'     \item{`y`}{Matched value from `y` (`NA` when no valid match found).}
#'     \item{`score`}{Composite similarity score rounded to 4 decimal places
#'       (`NA` when no match).}
#'   }
#'   All elements of `x` appear (one row each).  Elements of `y` that were not
#'   matched to any `x` appear as additional rows with `x = NA` (full-join
#'   semantics).  When `emit_score_matrix = TRUE` the full `m × n` matrix is
#'   available via `attr(<result>, "score_matrix")`.
#'
#' @details
#' **Scoring pipeline for each `(x[i], y[j])` pair:**
#' 1. *Augment* — optionally append first-letter acronym.
#' 2. *Clean* — strip punctuation (and optionally digits), squish whitespace,
#'    optionally lower-case.
#' 3. *Word Jaccard* (`sw`) — `|common words| / |union words|`.
#' 4. *LCS ratio* (`slcs`) — `nchar(LCS) / max(nchar(a), nchar(b))` on
#'    space-stripped strings; zero if LCS < `lcs_min_chars`.
#' 5. *Char Jaccard* (`sc`) — `|common chars| / |union chars|`; computed only
#'    when `sw > 0` or `slcs > 0`.
#' 6. *Composite* — `wt_word * sw + wt_char * sc + wt_lcs * slcs`.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' x_vec <- c(
#'   "Agriculture & allied activities",
#'   "Agriculture Forestry and Fishing",
#'   "Mining & Quarrying",
#'   "Manufacturing",
#'   "Electricity Gas & Water Supply",
#'   "Electricity Gas Water Supply & Other Utility Services",
#'   "Construction",
#'   "Trade hotelstransport & communication",
#'   "Trade hotels transport and Communication",
#'   "Trade Hotels Transport Communication and Services Related to Broadcasting",
#'   "Finance Insurance Real Estate & Business Services",
#'   "Finance InsuranceReal Estate & Business Services",
#'   "Financial Real Estate and Professional Services",
#'   "Community Social & Personal Services",
#'   "Public Administration Defence and Other Services",
#'   "Gross Value Added at Basic Prices"
#' )
#'
#' y_vec <- c(
#'   "Agriculture, Livestock, Forestry and Fishing",
#'   "Mining and Quarrying",
#'   "Manufacturing",
#'   "Electricity, Gas, Water Supply & Other Utility Services",
#'   "Construction",
#'   "Trade, Hotels, Transport, Communication & Services Related to Broadcasting",
#'   "Financial, Real Estate & Professional Services",
#'   "Public Administration, Defence & Other Services",
#'   "Total Gross Value Added"
#' )
#'
#' # Many-to-many (default) — multiple x can share the same y
#' ar_fuzzy_match_strings(x_vec, y_vec)
#'
#' # One-to-one with globally optimal LSAP assignment
#' ar_fuzzy_match_strings(
#'   x_vec, y_vec,
#'   relationship      = "one_to_one",
#'   assignment_method = "LSAP"
#' )
#'
#' # One-to-one with greedy assignment, emit score matrix
#' ar_fuzzy_match_strings(
#'   x_vec, y_vec,
#'   relationship      = "one_to_one",
#'   assignment_method = "greedy",
#'   emit_score_matrix = TRUE
#' )
#' }
#'
ar_fuzzy_match_strings <- function(
    x,
    y,
    relationship      = c("many_to_many", "one_to_one"),
    assignment_method = c("LSAP", "greedy"),
    case_sensitive    = FALSE,
    retain_num        = FALSE,
    add_acronyms      = TRUE,
    wt_word           = 0.55,
    wt_char           = 0.15,
    wt_lcs            = 0.30,
    lcs_min_chars     = 3L,
    score_threshold   = 0.10,
    emit_score_matrix = FALSE
) {

  relationship      <- match.arg(relationship)
  assignment_method <- match.arg(assignment_method)


  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  x <- x[nzchar(x)>0]
  y <- y[nzchar(y)>0]

  x <- unique(x)
  y <- unique(y)

  # ── 0. Validation ─────────────────────────────────────────────────────────────
  stopifnot(
    "`x` must be a non-empty character vector" = is.character(x) && length(x) > 0L,
    "`y` must be a non-empty character vector" = is.character(y) && length(y) > 0L,
    "wt_word + wt_char + wt_lcs must equal 1" = abs(wt_word + wt_char + wt_lcs - 1) < 1e-9
  )

  if (relationship == "one_to_one" && assignment_method == "LSAP") {
    if (!pkg_is_available("clue"))
      message(
        'assignment_method = "LSAP" requires the {clue} package.\n',
        'Install it with: install.packages("clue")\n',
        'Taking assignment_method = "greedy" instead.'
      )
    assignment_method <- "greedy"
  }

  m <- length(x)
  n <- length(y)

  if(m*n>10000){
    warning("Too many comparisons. The current algorithm is not optimized for handling this volume.", call. = FALSE)
  }

  # ── 1. String preparation ─────────────────────────────────────────────────────

  make_acronym <- function(s) {
    words <- stringr::str_split(stringr::str_trim(s), "\\s+")[[1L]]
    words <- words[nzchar(words)]
    if (length(words) >= 2L)
      toupper(paste(substr(words, 1L, 1L), collapse = ""))
    else
      NA_character_
  }

  augment <- function(s) {
    if (!add_acronyms) return(s)
    acr <- make_acronym(s)
    if (!is.na(acr) && nchar(acr) >= 2L) paste(s, acr) else s
  }

  clean <- function(s) {
    pat <- if (retain_num) "[^[:alnum:][:space:]]" else "[^[:alpha:][:space:]]"
    s |>
      stringr::str_replace_all(pat, " ") |>
      stringr::str_squish() |>
      (\(z) if (!case_sensitive) tolower(z) else z)()
  }

  prepare <- function(vec) purrr::map_chr(vec, \(s) clean(augment(s)))

  x_clean <- prepare(x)
  y_clean <- prepare(y)

  # ── 2. Per-pair scorers ───────────────────────────────────────────────────────

  word_set <- function(s) unique(stringr::str_split(s, "\\s+")[[1L]])
  char_set <- function(s) unique(strsplit(stringr::str_remove_all(s, " "), "")[[1L]])

  jaccard <- function(a, b) {
    if (length(a) == 0L || length(b) == 0L) return(0)
    length(intersect(a, b)) / length(union(a, b))
  }

  lcs_ratio <- function(a, b) {
    a2 <- stringr::str_remove_all(a, " ")
    b2 <- stringr::str_remove_all(b, " ")
    if (!nzchar(a2) || !nzchar(b2)) return(0)

    if(pkg_is_available("PTXQC")){
      lcs_len <- tryCatch(nchar(PTXQC::LCS(a2, b2)), error = \(e) 0L)
    } else {
      lcs_len <- 0
    }

    if (lcs_len < lcs_min_chars) return(0)
    lcs_len / max(nchar(a2), nchar(b2))
  }

  composite_score <- function(a, b) {
    sw   <- jaccard(word_set(a), word_set(b))
    slcs <- lcs_ratio(a, b)
    sc   <- if (sw > 0 || slcs > 0) jaccard(char_set(a), char_set(b)) else 0
    wt_word * sw + wt_char * sc + wt_lcs * slcs
  }

  # ── 3. Full m×n score matrix ──────────────────────────────────────────────────

  score_vals <-
    tidyr::expand_grid(xi = seq_len(m), yj = seq_len(n)) |>
    dplyr::mutate(
      score = purrr::map2_dbl(
        .data$xi, .data$yj,
        \(i, j) composite_score(x_clean[i], y_clean[j])
      )
    )


  # Build matrix via explicit index assignment, NOT positional fill.
  # matrix(vec, nrow, ncol) fills column-major: (1,1),(2,1),(3,1),...
  # expand_grid produces row-major: (1,1),(1,2),(1,3),...
  # Positional fill would silently transpose the scores — avoid it.
  score_mat <- matrix(NA_real_, nrow = m, ncol = n, dimnames = list(x, y))
  score_mat[cbind(score_vals$xi, score_vals$yj)] <- score_vals$score


  # ── 4. Assignment ─────────────────────────────────────────────────────────────
  #
  # many_to_many → "best": each x independently picks its argmax y.
  #                Multiple x rows may share the same y. assignment_method ignored.
  #
  # one_to_one   → "LSAP":   globally optimal bijection via clue::solve_LSAP().
  #                            Pads to square so the solver always gets a square
  #                            cost matrix; padding cells are given cost = 1 (worst).
  #              → "greedy": ranked-pair heuristic; fast but sub-optimal.

  pairs <-
    if (relationship == "many_to_many") {

      score_vals |>
        dplyr::group_by(.data$xi) |>
        dplyr::slice_max(.data$score, n = 1L, with_ties = FALSE) |>
        dplyr::ungroup()

    } else if (assignment_method == "LSAP") {

      pad      <- max(m, n)
      cost_mat <- matrix(1, nrow = pad, ncol = pad)
      cost_mat[seq_len(m), seq_len(n)] <- 1 - score_mat

      assignment <- as.integer(clue::solve_LSAP(cost_mat, maximum = FALSE))

      # assignment[i] is the column (1-indexed) assigned to padded row i.
      # Retain only rows where the assigned column falls within real y indices.
      tibble::tibble(xi = seq_len(m), yj = assignment[seq_len(m)]) |>
        dplyr::filter(.data$yj <= n) |>
        dplyr::mutate(score = score_mat[cbind(.data$xi, .data$yj)])

    } else {

      # Greedy 1-to-1: accumulate used indices, accept pair only when both free
      score_vals |>
        dplyr::arrange(dplyr::desc(.data$score)) |>
        dplyr::mutate(
          accepted = purrr::accumulate(
            purrr::map2(.data$xi, .data$yj, \(i, j) c(i, j)),
            .init = list(ux = integer(0), uy = integer(0)),
            \(state, ij)
            if (ij[1L] %in% state$ux || ij[2L] %in% state$uy)
              state
            else
              list(ux = c(state$ux, ij[1L]), uy = c(state$uy, ij[2L]))
          ) |>
            (\(states)
             purrr::map2_lgl(
               states[-length(states)],
               states[-1L],
               \(prev, curr) length(curr$ux) > length(prev$ux)
             )
            )()
        ) |>
        dplyr::filter(.data$accepted) |>
        dplyr::select("xi", "yj", "score")
    }

  # ── 5. Threshold ──────────────────────────────────────────────────────────────

  valid_pairs <- dplyr::filter(pairs, .data$score >= score_threshold)

  # ── 6. Output tibble — full-join semantics ────────────────────────────────────
  #   • All x rows appear (y / score = NA when unmatched)
  #   • y elements with no matching x appear as extra rows (x = NA)

  x_side <-
    tibble::tibble(xi = seq_len(m), x = x) |>
    dplyr::left_join(
      valid_pairs |>
        dplyr::mutate(y = y[.data$yj], score = round(.data$score, 4L)) |>
        dplyr::select("xi", "y", "score"),
      by = "xi"
    ) |>
    dplyr::select("x", "y", "score")

  unmatched_y_side <- tibble::tibble(
    x     = NA_character_,
    y     = y[setdiff(seq_len(n), valid_pairs$yj)],
    score = NA_real_
  )

  out <- dplyr::bind_rows(x_side, unmatched_y_side)

  if (emit_score_matrix) attr(out, "score_matrix") <- score_mat

  out
}



# ══════════════════════════════════════════════════════════════════════════════
# fuzzy_join
# ══════════════════════════════════════════════════════════════════════════════

# Resolve and validate the `by` argument for fuzzy joins
# Internal helper: returns a named character vector `c(x_col = "y_col")`.
ar_resolve_by_arg_in_fuzzy_join <- function(x, y, by) {

  if (is.null(by)) {
    common <- intersect(colnames(x), colnames(y))
    if (length(common) == 0L)
      stop("No common column names found. Specify `by` explicitly.")
    if (length(common) > 1L)
      stop(
        "Multiple common columns found: ",
        paste(common, collapse = ", "),
        ".\nSpecify `by` as a single named or unnamed string."
      )
    return(stats::setNames(common, common))   # c(col = "col")
  }

  if (!is.character(by) || length(by) != 1L)
    stop("`by` must be a single string, optionally named.")

  x_col <- if (is.null(names(by)) || !nzchar(names(by))) by else names(by)
  y_col <- unname(by)

  if (!x_col %in% colnames(x))
    stop(sprintf("`by` column '%s' not found in `x`.", x_col))
  if (!y_col %in% colnames(y))
    stop(sprintf("`by` column '%s' not found in `y`.", y_col))

  stats::setNames(y_col, x_col)   # c(x_col = "y_col")
}


# ── Public join function ──────────────────────────────────────────────────────

#' Fuzzy Join
#'
#' Performs a join where the key matching is approximate,
#' using fuzzy match strings to align string keys before joining.
#'
#' @param x,y Data frames or tibbles to join.
#' @param by `NULL` (use the single common column name) or a length-1 string,
#'   optionally named: `"col"` (same name in both) or
#'   `c("x_col" = "y_col")` (different names).
#' @param join_type `character(1)`. The type of join to perform: `"right"`,
#'   `"left"`, `"inner"`, or `"full"`. Default is `"right"`.
#' @param retain_name_from `character(1)`. Which data frame's key column name
#'   to retain in the final output. Defaults to `"y"`.
#' @param keep_score `logical(1)`. If `TRUE`, a score column is appended
#'   showing the match confidence. Default `FALSE`.
#' @param relationship `character(1)`. Relationship for fuzzy matching:
#'   `"many_to_many"` or `"one_to_one"`.
#' @param assignment_method `character(1)`. `"LSAP"` or `"greedy"`.
#' @param case_sensitive `logical(1)`. Fold strings to lower-case before scoring?
#' @param retain_num `logical(1)`. Retain digits during matching?
#' @param add_acronyms `logical(1)`. Append first-letter acronyms to strings?
#' @param wt_word,wt_char,wt_lcs `numeric(1)`. Weights for Jaccard and LCS
#'   matching (must sum to 1).
#' @param lcs_min_chars `integer(1)`. Minimum length for longest common substring.
#' @param score_threshold `numeric(1)`. Minimum score to consider a valid match.
#' @param emit_score_matrix `logical(1)`. If `TRUE`, the full `m × n` numeric
#'   score matrix from the matching step is attached to the output as
#'   `attr(out, "score_matrix")`.
#' @param retain_xy_cols `logical(1)`. If `TRUE`, retains the original matched columns as `.x_col` and `.y_col`. Default is `FALSE`.
#' @param ... Further arguments forwarded to the underlying `dplyr` join
#'   function (e.g., `suffix`, `na_matches`).
#'
#' @return A tibble in the style of `dplyr` join functions' output.
#'
#' @examples
#' # 1. Your raw, messy data (e.g., from a quick survey or flash report)
#' df_raw <- tibble::tibble(
#'   sector_name = c("Agri & Fishing", "Mfg Sector", "Real Est. & Prof Services"),
#'   value = c(150, 420, 310)
#' )
#'
#' # 2. Your clean, official master mapping
#' df_official <- tibble::tibble(
#'   canonical_sector = c(
#'     "Agriculture, Forestry and Fishing",
#'     "Manufacturing",
#'     "Financial, Real Estate & Professional Services",
#'     "Mining and Quarrying"
#'   ),
#'   weight = c(15.2, 18.1, 22.4, 2.5)
#' )
#'
#' # 3. Perform the Fuzzy Join
#' # We use a left join to keep all our raw data, and we turn on `keep_score`
#' # so we can inspect how confident the algorithm was.
#' fuzzy_join(
#'   x = df_raw,
#'   y = df_official,
#'   by = c("sector_name" = "canonical_sector"),
#'   join_type = "left",
#'   keep_score = TRUE
#' )
#'
#' @export
fuzzy_join <- function(
    x, y,
    by = NULL,
    join_type = c("right", "left", "inner", "full"),
    retain_name_from = c("y", "x"),
    keep_score = FALSE,
    relationship = c("many_to_many", "one_to_one"),
    assignment_method = c("LSAP", "greedy"),
    case_sensitive = FALSE,
    retain_num = FALSE,
    add_acronyms = TRUE,
    wt_word = 0.55,
    wt_char = 0.15,
    wt_lcs = 0.30,
    lcs_min_chars = 3L,
    score_threshold = 0.10,
    emit_score_matrix = FALSE,
    retain_xy_cols = FALSE,
    ...) {

  # Validate arguments
  join_type <- match.arg(join_type)
  retain_name_from <- match.arg(retain_name_from)
  relationship <- match.arg(relationship)
  assignment_method <- match.arg(assignment_method)

  # Map the string to the corresponding dplyr function
  join_fn <- switch(
    join_type,
    "right" = dplyr::right_join,
    "left"  = dplyr::left_join,
    "inner" = dplyr::inner_join,
    "full"  = dplyr::full_join
  )

  # Resolve columns
  by_resolved <- ar_resolve_by_arg_in_fuzzy_join(x, y, by)
  x_col <- names(by_resolved)
  y_col <- unname(by_resolved)

  # ── fuzzy match on the key vectors ─────────────────────────────────────────
  match_tbl <- ar_fuzzy_match_strings(
    x = x[[x_col]],
    y = y[[y_col]],
    relationship = relationship,
    assignment_method = assignment_method,
    case_sensitive = case_sensitive,
    retain_num = retain_num,
    add_acronyms = add_acronyms,
    wt_word = wt_word,
    wt_char = wt_char,
    wt_lcs = wt_lcs,
    lcs_min_chars = lcs_min_chars,
    score_threshold = score_threshold,
    emit_score_matrix = emit_score_matrix
  )

  # ── bridge: x key → y key, optionally carrying the score ───────────────────
  bridge <- match_tbl |>
    (\(tbl) if (keep_score) tbl else dplyr::select(tbl, -dplyr::any_of("score")))()

  # ── join x to bridge on x_col, then join result to y on y_col ──────────────
  xo <- x |>
    dplyr::left_join(bridge, by = stats::setNames("x", x_col)) |>
    join_fn(y, by = stats::setNames(y_col, "y"), ...)

  if(retain_xy_cols){
    xo$.x_col <- xo[[x_col]]
    xo$.y_col <- xo$y
  }

  # ── Cleanup and finalize column names ──────────────────────────────────────
  if (retain_name_from == "y") {
    xo <- dplyr::select(xo, -dplyr::all_of(x_col))

    # Use base R renaming to avoid requiring rlang (`!!`, `:=`) in package namespace
    names(xo)[names(xo) == "y"] <- y_col
  } else {
    xo <- dplyr::select(xo, -dplyr::all_of("y"))
  }

  # ── Attach score matrix attribute if requested ─────────────────────────────
  if (emit_score_matrix) {
    attr(xo, "score_matrix") <- attr(match_tbl, "score_matrix")
    attr(xo, "match_tbl") <- dplyr::as_tibble(match_tbl)
  }

  xo
}



#' Drop Constant Columns from a Data Frame
#'
#' @description
#' Evaluates all columns in a data frame and removes those that contain only a
#' single unique value. This is useful for cleaning up datasets by stripping out
#' zero-variance or uninformative variables.
#'
#' @param d A data frame or tibble.
#' @param na.rm `logical(1)`. If `TRUE` (the default), `NA` values are stripped
#'   before counting unique values. If `FALSE`, `NA` is considered a distinct
#'   value, meaning a column with one constant value plus `NA` will be kept.
#'
#' @return A data frame or tibble with all constant columns removed.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   id = 1:3,
#'   constant_num = c(5, 5, 5),
#'   constant_with_na = c("A", "A", NA),
#'   varying = c("X", "Y", "Z")
#' )
#'
#' # By default (na.rm = TRUE), NA is ignored.
#' # Both 'constant_num' and 'constant_with_na' are dropped.
#' drop_constants(df)
#'
#' # If na.rm = FALSE, NA counts as a distinct unique value.
#' # 'constant_with_na' is kept because it has two unique values: "A" and NA.
#' drop_constants(df, na.rm = FALSE)
drop_constants <- function(d, na.rm = TRUE) {
  if(na.rm){
    d |> dplyr::select(dplyr::where(~.x |> unique() |> stats::na.omit() |> length() > 1))
  } else {
    d |> dplyr::select(dplyr::where(~.x |> unique() |> length() > 1))
  }
}
