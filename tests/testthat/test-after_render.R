

## ---- fixtures -------------------------------------------------------------
make_df <- function() tibble::tibble(
  cat   = c("GVA at Basic Prices", "Agriculture", "Mining"),
  stmt  = c("Statement 1", "Statement 2", "Statement 3"),
  qtr   = c("2024-Q1", "2024-Q2", "2025-Q3"),
  noise = c("foo", "bar", "baz")
)
nm  <- list(category = c("GVA at Basic Prices", "Agriculture"))
nmr <- list(period   = "^\\d{4}-Q[1-4]$")

## ---- core matching --------------------------------------------------------
test_that("string node renames the matching column", {
  out <- rename_by_content(make_df(), nm, assignment_method = "greedy", summary_msg = FALSE)
  expect_true("category" %in% names(out))
  expect_false("cat" %in% names(out))
})

test_that("regex node renames the matching column", {
  out <- rename_by_content(make_df(), list(), nmr, assignment_method = "greedy", summary_msg = FALSE)
  expect_true("period" %in% names(out)); expect_false("qtr" %in% names(out))
})

test_that("string + regex maps both families", {
  out <- rename_by_content(make_df(), nm, nmr, assignment_method = "greedy", summary_msg = FALSE)
  expect_true(all(c("category", "period") %in% names(out)))
})

test_that("name_map_regex = NULL behaves like empty", {
  out <- rename_by_content(make_df(), nm, name_map_regex = NULL,
                           assignment_method = "greedy", summary_msg = FALSE)
  expect_true("category" %in% names(out))
})

## ---- search-space controls ------------------------------------------------
test_that("include_cols restricts the search space", {
  out <- rename_by_content(make_df(), nm, include_cols = c("cat", "noise"),
                           keep_details = TRUE, summary_msg = FALSE)
  expect_setequal(colnames(attr(out, "score_matrix")), c("cat", "noise"))
  expect_true("category" %in% names(out))
})

test_that("exclude_cols are not searched and keep their name", {
  out <- suppressWarnings(rename_by_content(make_df(), nm, exclude_cols = "cat",
                                            keep_details = TRUE, summary_msg = FALSE))
  expect_false("cat" %in% colnames(attr(out, "score_matrix")))
  expect_true("cat"  %in% names(out))
  expect_false("category" %in% names(out))   # no valid match elsewhere
})

test_that("unknown include/exclude columns warn", {
  expect_warning(rename_by_content(make_df(), nm, exclude_cols = "zzz",
                                   assignment_method = "greedy", summary_msg = FALSE),
                 "unknown column")
})

## ---- threshold & case -----------------------------------------------------
test_that("min_score leaves weak matches unmapped (with warning)", {
  expect_warning(
    out <- rename_by_content(make_df(), list(category = "GVA at Basic Prices"),
                             include_cols = "noise", min_score = 0.5,
                             assignment_method = "greedy", summary_msg = FALSE),
    "below min_score"
  )
  expect_true("noise" %in% names(out)); expect_false("category" %in% names(out))
})

test_that("case_sensitive toggles caseless matching", {
  df <- tibble::tibble(x = c("apple pie", "apple pie"))
  ci <- rename_by_content(df, list(dish = "APPLE PIE"),
                          assignment_method = "greedy", summary_msg = FALSE)
  cs <- suppressWarnings(rename_by_content(df, list(dish = "APPLE PIE"), case_sensitive = TRUE,
                                           assignment_method = "greedy", summary_msg = FALSE))
  expect_true("dish"  %in% names(ci))
  expect_false("dish" %in% names(cs))
})

## ---- scoring properties ---------------------------------------------------
test_that("score_matrix is attached, a matrix, and within [0, 1]", {
  out <- rename_by_content(make_df(), nm, keep_details = TRUE, summary_msg = FALSE)
  sm <- attr(out, "score_matrix")
  expect_true(is.matrix(sm)); expect_false(is.null(attr(out, "mapping")))
  expect_true(all(sm >= 0 & sm <= 1))
})

test_that("keep_details = FALSE attaches nothing", {
  out <- rename_by_content(make_df(), nm, summary_msg = FALSE)
  expect_null(attr(out, "score_matrix")); expect_null(attr(out, "mapping"))
})

test_that("lambda penalises unmatched words", {
  df <- tibble::tibble(x = rep("alpha beta gamma delta", 2)); n <- list(n = "alpha")
  s0 <- attr(rename_by_content(df, n, lambda = 0, min_score = 0,
                               keep_details = TRUE, summary_msg = FALSE), "score_matrix")["n", "x"]
  s1 <- attr(rename_by_content(df, n, lambda = 1, min_score = 0,
                               keep_details = TRUE, summary_msg = FALSE), "score_matrix")["n", "x"]
  expect_gt(s0, s1)
})

test_that("regex score rewards higher matched coverage", {
  df <- tibble::tibble(code = rep("AB-12-XYZ", 2))
  part <- attr(rename_by_content(df, list(), list(p = "\\d{2}"), min_score = 0,
                                 keep_details = TRUE, summary_msg = FALSE), "score_matrix")["p", "code"]
  full <- attr(rename_by_content(df, list(), list(p = "^AB-\\d{2}-XYZ$"), min_score = 0,
                                 keep_details = TRUE, summary_msg = FALSE), "score_matrix")["p", "code"]
  expect_gt(full, part)
})

## ---- validity filtering & early exits -------------------------------------
test_that("all-NA / empty columns are dropped before scoring", {
  df <- make_df(); df$blank <- NA_character_; df$empties <- ""
  sm <- attr(rename_by_content(df, nm, keep_details = TRUE, summary_msg = FALSE), "score_matrix")
  expect_false(any(c("blank", "empties") %in% colnames(sm)))
})

test_that("nodes with only NA / empty targets are dropped", {
  nm2 <- c(nm, list(dud = c(NA, "", "   ")))
  sm <- attr(rename_by_content(make_df(), nm2, keep_details = TRUE, summary_msg = FALSE), "score_matrix")
  expect_false("dud" %in% rownames(sm)); expect_true("category" %in% rownames(sm))
})

test_that("no valid columns -> unchanged with warning", {
  df <- tibble::tibble(a = NA_character_, b = "")
  expect_warning(out <- rename_by_content(df, nm, summary_msg = FALSE), "unchanged")
  expect_identical(out, df)
})

test_that("no valid nodes -> unchanged with warning", {
  df <- make_df()
  expect_warning(out <- rename_by_content(df, list(dud = c(NA, "")), summary_msg = FALSE), "unchanged")
  expect_identical(out, df)
})

## ---- errors, warnings, messages -------------------------------------------
test_that("duplicate node names across maps error", {
  expect_error(rename_by_content(make_df(), list(dup = "x"), list(dup = "y"), summary_msg = FALSE),
               "more than one map")
})

test_that("more nodes than columns warns about unmapped nodes", {
  many <- list(a = "GVA at Basic Prices", b = "Statement 1", c = "2024-Q1", d = "foo", e = "extra")
  w <- capture_warnings(rename_by_content(make_df(), many, min_score = 0,
                                          assignment_method = "greedy", summary_msg = FALSE))
  expect_true(any(grepl("stay unmapped", w)))
})

test_that("summary_msg emits a report", {
  expect_message(rename_by_content(make_df(), nm, assignment_method = "greedy"), "rename_by_content")
})

test_that("greedy emits a tie warning on equal top scores", {
  df  <- tibble::tibble(colA = rep("alpha beta", 2), colB = rep("gamma delta", 2))
  tie <- list(nodeX = c("alpha beta", "gamma delta"), nodeY = "alpha")
  w <- capture_warnings(rename_by_content(df, tie, assignment_method = "greedy", summary_msg = FALSE))
  expect_true(any(grepl("Tie at score", w)))
})

## ---- LSAP -----------------------------------------------------------------
test_that("LSAP maps a clean separable case", {
  skip_if_not(pkg_is_available("clue"))
  out <- rename_by_content(make_df(), nm, nmr, summary_msg = FALSE)
  expect_true(all(c("category", "period") %in% names(out)))
})

test_that("LSAP finds the global optimum where greedy is sub-optimal", {
  skip_if_not(pkg_is_available("clue"))
  # Scores ~ X:{A=1,B=1}, Y:{A=0.75,B=0}. Greedy grabs X-A first, stranding Y;
  # LSAP picks X-B + Y-A for a higher total, mapping both nodes.
  df  <- tibble::tibble(colA = rep("alpha beta", 2), colB = rep("gamma delta", 2))
  tie <- list(nodeX = c("alpha beta", "gamma delta"), nodeY = "alpha")
  g <- suppressWarnings(rename_by_content(df, tie, assignment_method = "greedy", summary_msg = FALSE))
  l <- suppressWarnings(rename_by_content(df, tie, assignment_method = "LSAP",  summary_msg = FALSE))
  expect_true("nodeX" %in% names(g) && !("nodeY" %in% names(g)))
  expect_true(all(c("nodeX", "nodeY") %in% names(l)))
})

test_that("LSAP handles more nodes than columns (transpose branch)", {
  skip_if_not(pkg_is_available("clue"))
  many <- list(a = "GVA at Basic Prices", b = "Statement 1", c = "2024-Q1",
               d = "foo", e = "nomatch zzz")
  out <- suppressWarnings(rename_by_content(make_df(), many, assignment_method = "LSAP", summary_msg = FALSE))
  mapped <- intersect(names(out), names(many))
  expect_lte(length(mapped), 4L); expect_gte(length(mapped), 1L)
})



