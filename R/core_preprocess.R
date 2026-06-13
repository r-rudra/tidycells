# This file contains functions for pre-processing `Cells` and helper functions

# --- core_prep_check_str* functions ---
# These functions are string pattern based functions for checking



#' Core function to check a string against patterns.
#'
#' This is an internal helper function that provides a generic way to classify
#' strings based on a set of regex patterns and cleaning rules.
#'
#' @param x A character vector to be checked.
#' @param lookup_regex A named list of regex patterns, must contain `target` and
#'   `exception`.
#' @param pattern_class A named list of classification strings, must contain
#'   `target`, `exception`, `blank`, and `default`.
#' @param allowed_strings A character vector of substrings to be removed from the
#'   input before pattern matching. (These are ignored to allow the underlying
#'   pattern to be matched).
#' @return A character vector with classification results.
#' @keywords internal

core_prep_check_str_pattern <- function(
    x,
    lookup_regex = list(target = "^[\\d.,+-]+$", exception = NULL),
    pattern_class = list(target = "num",  exception = "non_num", blank = "blank", default = "non_num"),
    allowed_strings = NULL) {

  # Ideally there should be a check for `pattern_class` and
  # `lookup_regex` as a predefined-named list, but since this is an internal
  # function this check is omitted.

  if (length(x) == 0) {
    return(character(0))
  }

  # --- Preprocessing ---
  # 1. Standardize the input vector `x` by converting to lowercase and removing
  #    ALL whitespace.
  processed_x <- stringr::str_to_lower(x) %>% stringr::str_remove_all("\\s")

  # 2. Process the `allowed_strings` to create a regex pattern for removal.
  stripped_x <- if (!is.null(allowed_strings) && length(allowed_strings) > 0) {
    allowed_pattern <- stringr::str_to_lower(allowed_strings) %>%
      stringr::str_remove_all("\\s") %>%
      stringr::str_escape() %>%
      stringr::str_c(collapse = "|")
    stringr::str_remove_all(processed_x, allowed_pattern)
  } else {
    processed_x
  }

  # If `lookup_regex$exception` is NULL or empty, we won't use it for
  # exception-classification.
  use_exception <- !is.null(lookup_regex$exception) && nzchar(lookup_regex$exception)

  exception_res <- if (use_exception) {
    stringr::str_detect(stripped_x, lookup_regex$exception)
  } else {
    rep(FALSE, length(stripped_x))
  }


  # --- Classification ---
  result <- dplyr::case_when(
    # Condition 1: "blank"
    # An element is "blank" (pattern_class$blank) if it's an empty string
    # after all removals.
    stripped_x == "" ~ pattern_class$blank,

    # Condition 2: exception-type
    # If `lookup_regex$exception` is provided, classify as
    # `pattern_class$exception` before pattern_class$target.
    exception_res ~ pattern_class$exception,

    # Condition 3: desired pattern
    stringr::str_detect(stripped_x, lookup_regex$target) ~ pattern_class$target,

    # Condition 4: The Default Case
    TRUE ~ pattern_class$default
  )

  return(result)
}




#' Check if a character vector contains numbers based on custom rules.
#'
#' This function evaluates each element of a character vector `x` to determine
#' if it's a number ("num"), not a number ("non_num"), or effectively blank
#' ("blank").
#'
#' @param x A character vector to be checked.
#' @param allowed_strings A character vector of strings to be ignored during the
#'   check. (These are called "allowed strings" as these are considered part of
#'   number if supplied with at least one number)
#' @param exclude_dates A logical value. If `TRUE` (the default), strings that
#'   resemble dates are classified as "non_num".
#' @param use_num_regex A character string indicating the regex pattern to use
#'   for numbers should be one of: "most_relaxed", "relaxed", or "strict".
#'   Defaults to "most_relaxed".
#' @return A character vector of the same length as `x` with values "num",
#'   "non_num", or "blank".
#' @keywords internal
#'
core_prep_check_str_num <- function(
    x,
    allowed_strings = NULL, exclude_dates = TRUE,
    use_num_regex = c("most_relaxed", "relaxed", "strict")) {

  use_num_regex <- match.arg(use_num_regex)

  num_regex <- core_prep_regex_for_number()[[use_num_regex]]

  # This regex identifies common date formats like YYYY-MM-DD and DD-MM-YYYY.
  # Only this date format may get wrongly classified as a number.
  date_pattern <- "(^\\d{4}[-/. ]\\d{1,2}[-/. ]\\d{1,2}$)|(^\\d{1,2}[-/. ]\\d{1,2}[-/. ]\\d{4}$)"

  # Conditionally set the exception regex based on the exclude_dates flag.
  exception_regex <- if (exclude_dates) {
    date_pattern
  } else {
    NULL
  }

  # Call the core function with parameters specific to checking for numbers.
  core_prep_check_str_pattern(
    x = x,
    lookup_regex = list(
      target = num_regex,
      exception = exception_regex
    ),
    pattern_class = list(
      target = "num",
      exception = "non_num",
      blank = "blank",
      default = "non_num"
    ),
    allowed_strings = allowed_strings
  )
}



core_prep_frequent_words <- function(x, threshold_freq) {
  # Preprocess: trim whitespace and convert to lowercase
  x <- stringr::str_trim(stringr::str_to_lower(x))
  words <- unlist(stringr::str_split(x, "\\W+"))
  words <- words[words != ""]

  if (length(words) == 0) return(character(0))

  word_freq <- table(words)
  # Consider only words that occur more than once
  word_freq <- word_freq[word_freq > 1]

  if (length(word_freq) == 0) return(character(0))

  if (missing(threshold_freq)) {
    q1 <- as.numeric(stats::quantile(word_freq, 0.25, names = FALSE, type = 7))
    q3 <- as.numeric(stats::quantile(word_freq, 0.75, names = FALSE, type = 7))
    iqr <- q3 - q1

    # If IQR is 0, all repeated words are similarly frequent-no standouts
    if (iqr == 0) {
      return(character(0))
    }

    threshold_freq <- q3 + 1.5 * iqr
  }

  frequent_words <- names(word_freq[word_freq >= threshold_freq])
  frequent_words <- frequent_words[order(word_freq[frequent_words], decreasing = TRUE)]
  frequent_words
}

core_prep_numeric_context_tokens <- function() {
  l <- list()

  # Currency symbols (major global currencies)
  l$currency_symbols <- c(
    "\u20B9", "$", "\u20AC", "\u00A3", "\u00A5", "\u20A9", "\u20BD", "\u20B4", "\u20A6", "\u20AB", "\u0E3F", "\u20BA", "\u20B5", "\u20A1", "\u20B2", "\u17DB",
    "\u20AA", "\u20A8", "\u20B1", "\u20A2", "\u20A3", "\u20A4", "\u20A5", "\u20A7", "\u20AD", "\u20AE", "\u20AF", "\u20B0", "\u20B3", "\u20B6", "\u20B7", "\u20B8",
    "\u20BB", "\u20BC", "\u20BE", "\u20BF"
  )

  # Special symbols (mathematical and approximation markers)
  l$special_symbols <- c(
    "%", "(", ")", "[", "]", "{", "}",
    "e", "e+", "e-",  # Scientific notation
    "~", "\u2248", "\u2243", "\u2245", "\u223C", "\u224A", "\u224B",  # Approximation symbols
    "approx", "apprx", "approximately", "circa", "ca", "c.",
    "+/-", "\u00B1", "\u2213", "around", "about", "abt", "roughly", "est", "estimated"
  )

  # Ordinal suffixes (English and other languages)
  l$ordinal_suffixes <- c(
    "st", "nd", "rd", "th",  # English
    "er", "re", "e", "\u00E8me", "\u00E8me",  # French
    "te", "de", "ste",  # German/Dutch
    "\u00BA", "\u00AA", "\u00B0"  # Spanish/Portuguese
  )

  # Range indicators (connection and span markers)
  l$range_indicators <- c(
    "-", "\u2013", "\u2014", "\u2212", "\u207B", "\u02D7", "\uFE63", "\uFF0D",  # Various dashes
    "to", "upto", "up to", "between", "from", "til", "till", "until", "through", "thru",
    "vs", "versus", "v", "and", "&", "or", "plus", "+", "with", "w/", "incl", "including"
  )

  # Textual currencies (ISO codes, full names, colloquial terms)
  l$textual_currencies <- c(
    # Major ISO 4217 codes
    "rs", "inr", "usd", "eur", "gbp", "jpy", "cny", "aud", "cad", "nzd", "chf", "sgd",
    "hkd", "sek", "nok", "dkk", "zar", "brl", "mxn", "rub", "idr", "krw", "try", "thb",
    "twd", "vnd", "myr", "php", "pln", "czk", "huf", "ils", "sar", "aed", "qar", "kwd",
    "bhd", "omr", "ngn", "lkr", "bdt", "pkr", "mmk", "isk", "uah", "ron", "kzt", "uzs",
    "azn", "gel", "byn", "mdl", "amd", "tmt", "kgs", "tjk", "afn", "irr", "iqd", "jod",
    "syp", "lbp", "egp", "mad", "dzd", "tnd", "lyd", "sdg", "etb", "kes", "ugx", "tzs",
    "rwf", "bif", "djf", "sos", "scr", "mur", "mzn", "mwk", "zmw", "bwp", "szl", "lsl",
    # Full currency names
    "rupee", "rupees", "dollar", "dollars", "euro", "euros", "pound", "pounds", "sterling",
    "yen", "yuan", "won", "dirham", "dinar", "franc", "francs", "peso", "pesos",
    "real", "reais", "ringgit", "ruble", "rubles", "lira", "krona", "krone", "crown",
    "taka", "riel", "kip", "dong", "shekel", "kuna", "koruna", "leu", "loti", "rand",
    "baht", "riyal", "dram", "manat", "som", "afghani", "rial", "bolivar", "quetzal",
    # Informal/colloquial terms
    "bucks", "quid", "paise", "paisa", "sen", "cent", "cents", "kopeks", "pennies",
    "pence", "bob", "grand", "k", "mill", "mille", "lakh", "crore", "thousand", "million",
    "billion", "trillion"
  )

  # Measurement units (time, length, weight, volume, digital, etc.)
  l$measurement_units <- c(
    # Time units
    "ns", "\u03BCs", "ms", "sec", "secs", "s", "min", "mins", "hr", "hrs", "h",
    "hour", "hours", "day", "days", "d", "week", "weeks", "wk", "wks", "month", "months",
    "mo", "mos", "year", "years", "yr", "yrs", "y", "decade", "decades", "century", "centuries",
    # Length units
    "mm", "cm", "m", "km", "meter", "meters", "metre", "metres", "kilometer", "kilometers",
    "kilometre", "kilometres", "inch", "inches", "in", "ft", "feet", "foot", "yard", "yards",
    "yd", "yds", "mile", "miles", "mi", "nautical", "nm", "fathom", "fathoms", "furlong",
    # Weight/Mass units
    "mg", "g", "kg", "gram", "grams", "kilogram", "kilograms", "ton", "tons", "tonne", "tonnes",
    "t", "lb", "lbs", "pound", "pounds", "oz", "ounce", "ounces", "stone", "stones", "cwt",
    # Volume units
    "ml", "l", "litre", "litres", "liter", "liters", "gallon", "gallons", "gal", "quart",
    "quarts", "qt", "pint", "pints", "pt", "cup", "cups", "fl", "floz", "tablespoon",
    "tbsp", "teaspoon", "tsp", "barrel", "barrels", "bbl",
    # Digital/Data units
    "b", "bit", "bits", "byte", "bytes", "kb", "mb", "gb", "tb", "pb", "eb", "zb", "yb",
    "kib", "mib", "gib", "tib", "pib", "eib", "zib", "yib", "kilobyte", "megabyte",
    "gigabyte", "terabyte", "petabyte", "exabyte", "zettabyte", "yottabyte",
    # Temperature units
    "c", "f", "k", "celsius", "fahrenheit", "kelvin", "centigrade", "\u00B0c", "\u00B0f", "\u00B0k",
    # Area units
    "sq", "square", "sqm", "sqft", "sqkm", "sqmi", "acre", "acres", "hectare", "hectares", "ha",
    # Speed units
    "mph", "kmh", "kph", "mps", "fps", "knot", "knots", "kn", "mach",
    # Energy units
    "j", "kj", "mj", "joule", "joules", "cal", "kcal", "calorie", "calories", "btu", "wh", "kwh",
    # Power units
    "w", "kw", "mw", "gw", "watt", "watts", "kilowatt", "kilowatts", "megawatt", "megawatts",
    "hp", "horsepower",
    # Pressure units
    "pa", "kpa", "mpa", "bar", "psi", "atm", "mmhg", "torr", "pascal",
    # Frequency units
    "hz", "khz", "mhz", "ghz", "hertz", "kilohertz", "megahertz", "gigahertz",
    # Percentage-like units
    "percent", "pct", "percentage", "\u2030", "permille", "permil", "ppm", "ppb",
    # General quantity units
    "units", "unit", "pcs", "piece", "pieces", "items", "item", "score", "dozen", "dozens",
    "gross", "pair", "pairs", "set", "sets", "lot", "lots", "pack", "packs", "box", "boxes",
    "each", "ea", "rate", "ratio", "count", "number", "no", "qty", "quantity"
  )

  return(l)
}

core_prep_regex_for_number <- function(){

  num_regex <- list()

  num_regex$most_relaxed <- "^[\\d.,+-]+$"

  num_regex$relaxed <- paste0(
    "^(",
    # Case 1: String starts with '+' sign (positive number)
    # - Must start with '+'
    # - Allows zero or more groups of either:
    #     - An optional single dot or comma, followed by one or more digits
    #     - OR an optional single dot or comma, followed by a dash ('-') [dash allowed only for '+' at start]
    # - No consecutive dots, commas, dot-comma, or comma-dot allowed (because [.,]? only allows one at a time)
    # - Must end with an optional single dot or comma
    "\\+((([.,]?[0-9]+)|([.,]?-))+)[.,]?",
    "|",
    # Case 2: String starts with '-' sign (negative number)
    # - Must start with '-'
    # - Allows zero or more groups of:
    #     - An optional single dot or comma, followed by one or more digits
    # - No dash allowed anywhere except at the start
    # - No consecutive dots, commas, dot-comma, or comma-dot allowed
    # - Must end with an optional single dot or comma
    "-(([.,]?[0-9]+))+[.,]?",
    "|",
    # Case 3: No sign at start (unsigned number)
    # - Allows zero or more groups of:
    #     - An optional single dot or comma, followed by one or more digits
    # - No dash allowed anywhere
    # - No consecutive dots, commas, dot-comma, or comma-dot allowed
    # - Must end with an optional single dot or comma
    "(([.,]?[0-9]+))+[.,]?",
    ")$"
  )

  num_regex$strict <- paste0(
    "^(",
    # Case 1: String starts with '+' sign (positive number)
    "\\+(([.,]?[0-9]+)+)[.,]?",
    "|",
    # Case 2: String starts with '-' sign (negative number)
    "-(([.,]?[0-9]+)+)[.,]?",
    "|",
    # Case 3: No sign at start (unsigned number)
    "(([.,]?[0-9]+)+)[.,]?",
    ")$"
  )

  num_regex
}

core_prep_regex_for_date <- function() {

  date_regex <- list()

  # Regex for all month names and abbreviations (case-insensitive if used with ignore.case=TRUE)
  months_regex <- "(jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|jun(e)?|jul(y)?|aug(ust)?|sep(tember)?|oct(ober)?|nov(ember)?|dec(ember)?)"

  # ==== Standard date patterns ====
  # Pattern: DD-MON-YYYY (e.g. 31-MAR-2023)
  text_date_pattern1 <- paste0("\\d{1,2}[-./]?", months_regex, "[-./]?\\d{2,4}")
  # Pattern: YYYY-MON-DD (e.g. 2023-MAR-31)
  text_date_pattern2 <- paste0("\\d{4}[-./]?", months_regex, "[-./]?\\d{1,2}")

  # ==== Financial year (FY) date patterns with month names ====
  # Pattern: DD-MON-YYYY-YY or DD-MON-YYYY/YY or DD-MON-YYYY.YY (e.g. 31-MAR-2023-24)
  text_date_pattern1_fy <- paste0("\\d{1,2}[-./]?", months_regex, "[-./]?\\d{4}[-./]?\\d{2}")
  # Pattern: YYYY-YY-MON-DD or YYYY/YY-MON-DD or YYYY.YY-MON-DD (e.g. 2023-24-MAR-31)
  text_date_pattern2_fy <- paste0("\\d{4}[-./]?\\d{2}[-./]?", months_regex, "[-./]?\\d{1,2}")

  # ==== Standard numeric date patterns ====
  # Pattern: YYYY-MM-DD or YYYY/MM/DD or YYYY.MM.DD (e.g. 2025/01/13)
  numeric_date_pattern1 <- "\\d{4}[-./]\\d{1,2}[-./]\\d{1,2}"
  # Pattern: DD-MM-YYYY or DD/MM/YYYY or DD.MM.YYYY (e.g. 13-01-2025)
  numeric_date_pattern2 <- "\\d{1,2}[-./]\\d{1,2}[-./]\\d{4}"

  # ==== Quarter patterns ====
  # Pattern: Q1-YYYY, YYYY-Q1, Q1 (e.g. Q3-2022, 2022-Q3, Q2)
  quarter_patterns <- paste0(
    "q[1-4][-./]?\\d{4}",
    "|\\d{4}[-./]?q[1-4]",
    "|q[1-4]"
  )

  # ==== Financial year text patterns ====
  # Pattern: FY2023, FY23, FY2023-24, FY23-24, FY2023/24, FY23/24 (e.g. FY2023-24)
  # Also allow "2023-24", "2023/24", "23-24", "23/24", "2023-2024" (full 4-digit pair, rare)
  fy_pattern <- "(fy[-./]?)?\\d{2,4}([-./]\\d{2,4})?"

  # ==== EXTRA patterns for rare and reporting-specific conventions ====

  # 1. Full 4-digit year pair for financial years (e.g. 2023-2024)
  fy_full_year_pair <- "\\d{4}[-./]\\d{4}"

  # 2. Month-name year to month-name year: "Apr 2023 - Mar 2024"
  month_year_range <- paste0(
    months_regex, "[ -./]*\\d{2,4}[ ]*[-][ ]*",
    months_regex, "[ -./]*\\d{2,4}"
  )

  # 3. ISO week numbers: YYYY-W## or YYYYW##
  iso_week <- paste0("\\d{4}[-]?w\\d{2}")

  # 4. Year-month: YYYY/MM or MM/YYYY (e.g. 2025/01 or 01/2025)
  year_month1 <- "\\d{4}[-./]\\d{1,2}"
  year_month2 <- "\\d{1,2}[-./]\\d{4}"

  # ==== Base regex combinations ====
  # All basic date patterns
  date_regex$base <- paste0(
    text_date_pattern1, "|",
    text_date_pattern2, "|",
    numeric_date_pattern1, "|",
    numeric_date_pattern2
  )

  # Add quarter formats
  date_regex$base_with_qtrs <- paste0(
    date_regex$base, "|",
    quarter_patterns
  )

  # Add all financial year formats
  date_regex$base_with_qtrs_with_fy <- paste0(
    date_regex$base_with_qtrs, "|",
    fy_pattern, "|",
    text_date_pattern1_fy, "|",
    text_date_pattern2_fy
  )

  # Add rare and reporting-specific patterns
  date_regex$base_with_qtrs_with_fy_extra <- paste0(
    date_regex$base_with_qtrs_with_fy, "|",
    fy_full_year_pair, "|",
    month_year_range, "|",
    iso_week, "|",
    year_month1, "|",
    year_month2
  )

  date_regex
}

core_prep_regex_for_time <- function(){
  # 1. Numeric time like 10:40, 10:40:30, with optional am/pm
  time_numeric_ampm <- "\\d{1,2}([:.-]\\d{1,2}){1,2}(am|pm)?"
  # 2. Simpler time like 7pm or 10-40am
  time_simple_ampm <- "\\d{1,2}([:.-]\\d{1,2})?(am|pm)"
  # 3. Time with units like "4hr", "10min", "80seconds" (not "10 s" or "10 m")
  time_units <- "(\\d+(hr(s?|ours?)?|min(utes?)?|sec(onds?)?))+"

  # 4. Patterns for time with units, allowing combinations of hours, minutes,
  # and seconds. (at least 2) Accepts: 10h, 10hr, 10hrs, 10hour, 10hours
  hr  <- "\\d+\\s*(h(rs?|ours?)?)"
  # Accepts: 20m, 20min, 20mins, 20minute, 20minutes
  min <- "\\d+\\s*(m(in(utes?)?)?)"
  # Accepts: 30s, 30sec, 30secs, 30second, 30seconds
  sec <- "\\d+\\s*(s(ec(onds?)?)?)"

  # Patterns for at least two (or all three) units, in any order.
  two_or_three_units <- paste0(
    "(",
    hr, "\\s*", min, "(\\s*", sec, ")?",  # hr min [sec]
    "|",
    hr, "\\s*", sec, "(\\s*", min, ")?",  # hr sec [min]
    "|",
    min, "\\s*", hr, "(\\s*", sec, ")?",  # min hr [sec]
    "|",
    min, "\\s*", sec, "(\\s*", hr, ")?",  # min sec [hr]
    "|",
    sec, "\\s*", hr, "(\\s*", min, ")?",  # sec hr [min]
    "|",
    sec, "\\s*", min, "(\\s*", hr, ")?",  # sec min [hr]
    ")"
  )

  # Combine all patterns into a single regex component.
  time_regex <- paste0(
    "(",
    time_numeric_ampm, "|",
    time_simple_ampm, "|",
    time_units, "|",
    two_or_three_units,
    ")"
  )
  return(time_regex)
}

core_prep_prob_of_being_year <- function(
    x,
    t1 = 1600, t2 = 2100, alpha = 0.1) {
  # Only consider strings that match exactly 4 digits (YYYY)
  is_year <- stringr::str_detect(x, "^\\d{4}$")
  # Convert to numeric if matches, else NA (no warnings)
  year_num <- ifelse(
    is_year,
    suppressWarnings(as.numeric(x)),
    NA_real_
  )
  # Compute logistic probability only for valid years
  prob <- rep(0, length(x))
  valid_idx <- which(!is.na(year_num))
  if(length(valid_idx) > 0) {
    left <- 1 / (1 + exp(-alpha * (year_num[valid_idx] - t1)))
    right <- 1 / (1 + exp(alpha * (year_num[valid_idx] - t2)))
    prob[valid_idx] <- left * right
  }
  prob
}

#' Check if a character vector contains date formats.
#'
#' This function evaluates each element of a character vector `x` to determine
#' if it represents a date. It is designed to recognize various numeric and
#' text-based date formats.
#'
#' @param x A character vector to be checked.
#' @param allowed_strings A character vector of strings to be ignored during the
#'   check. (These are called "allowed strings" as they are considered part of
#'   a date if supplied with a date expression, e.g., "DOB:")
#' @param use_date_regex A character string indicating the regex pattern to use
#' @return A character vector of the same length as `x` with values "date",
#'   "non_date", or "blank".
#' @keywords internal
#'
core_prep_check_str_date <- function(
    x, allowed_strings = NULL,
    use_date_regex = c("base", "base_with_qtrs", "base_with_qtrs_with_fy",
                       "base_with_qtrs_with_fy_extra")) {

  use_date_regex <- match.arg(use_date_regex)

  date_target_regex <- paste0(
    "^(", core_prep_regex_for_date()[[use_date_regex]], ")$"
  )

  # Call the core pattern matching function with date-specific configurations.
  core_prep_check_str_pattern(
    x = x,
    lookup_regex = list(
      target = date_target_regex,
      exception = NULL # No specific exceptions are needed for date checking.
    ),
    pattern_class = list(
      target = "date",
      exception = "non_date", # Not used as exception is NULL.
      blank = "blank",
      default = "non_date"
    ),
    allowed_strings = allowed_strings
  )
}



#' Check if a character vector contains time formats.
#'
#' This function evaluates each element of a character vector `x` to determine
#' if it represents a time, optionally preceded by a date. Supports numeric times
#' (e.g., "10:40", "7pm"), multi-letter time units (e.g., "4hr", "10min", "80seconds"),
#' and combinations of at least two time units (e.g., "10h 20m", "30m 20s", "10h 20m 30s")
#' with single-letter, three-letter or full-word forms. If a value is a pure number (e.g., "10.4"),
#' it will be classified as "non_time" if \code{exclude_numbers = TRUE}.
#'
#' @param x A character vector to be checked.
#' @param allowed_strings A character vector of strings to be ignored during the
#'   check. (These are called "allowed strings" as they are considered part of
#'   a time expression, e.g., "Starts at")
#' @param exclude_numbers Logical, default TRUE. If TRUE, then pure number strings
#'   (e.g. "10.4", "123") are excluded from being classified as "time" even if they
#'   match a numeric time pattern.
#' @return A character vector of the same length as `x` with values "time",
#'   "non_time", or "blank".
#' @keywords internal
core_prep_check_str_time <- function(
    x,
    allowed_strings = NULL,
    exclude_numbers = TRUE,
    use_date_regex = c("base", "base_with_qtrs", "base_with_qtrs_with_fy",
                       "base_with_qtrs_with_fy_extra")){

  use_date_regex <- match.arg(use_date_regex)

  # Pure number pattern
  number_pattern <- "^[\\d.,+-]+$"

  # If exclude_numbers is TRUE, set this as the exception regex.
  exception_regex <- if (exclude_numbers) number_pattern else NULL

  # Define the target regex for time formats.
  # This regex matches time formats with optional date prefix.
  target_regex <- paste0(
    "^",                                    # Start of string
    "(?:", core_prep_regex_for_date()[[use_date_regex]], ")?", # Optional non-capturing date group
    core_prep_regex_for_time(),              # Required time group
    "$"                                     # End of string
  )

  core_prep_check_str_pattern(
    x = x,
    lookup_regex = list(
      target = target_regex,
      exception = exception_regex
    ),
    pattern_class = list(
      target = "time",
      exception = "non_time",
      blank = "blank",
      default = "non_time"
    ),
    allowed_strings = allowed_strings
  )
}

#' Check if a character vector contains logical formats.
#'
#' This function evaluates each element of a character vector `x` to determine
#' if it represents a logical value (e.g., TRUE, FALSE, T, F, yes, no).
#'
#' @param x A character vector to be checked.
#' @param allowed_strings A character vector of strings to be ignored during the
#'   check. (These are called "allowed strings" as they are considered part of
#'   a logical expression, e.g., "Response:")
#' @return A character vector of the same length as `x` with values "logical",
#'   "non_logical", or "blank".
#' @keywords internal
core_prep_check_str_logical <- function(x, allowed_strings = NULL) {

  # Define the target regex for common logical values.
  # The `core_prep_check_str_pattern` function handles case-insensitivity.
  target_regex <- "^(true|false|t|f|yes|no|y|n)$"

  # Call the core pattern matching function with logical-specific configurations.
  core_prep_check_str_pattern(
    x = x,
    lookup_regex = list(
      target = target_regex,
      exception = NULL # No exceptions needed for logical checking.
    ),
    pattern_class = list(
      target = "logical",
      exception = "non_logical", # Not used as exception is NULL.
      blank = "blank",
      default = "non_logical"
    ),
    allowed_strings = allowed_strings
  )
}



#' Check if a character vector contains categorical values based on frequent
#' words.
#'
#' This function evaluates each element of a character vector `x` to determine
#' if it matches any of the provided frequent words or patterns.
#'
#' @param x A character vector to be checked.
#' @param allowed_strings A character vector of strings to be used as patterns
#'   for matching. If not provided, the function will use the most frequent
#'   words from `x` as patterns.
#'
#' @return A character vector of the same length as `x` with values
#'   "categorical", "non_categorical", or "blank".
#' @keywords internal
core_prep_check_str_categorical <- function(x, allowed_strings = NULL) {

  # Note here allowed string is used differently as compared to other functions.

  if(is.null(allowed_strings)) {
    allowed_strings <- core_prep_frequent_words(x)
  }

  allowed_strings <- stringr::str_to_lower(allowed_strings) %>%
    stringr::str_remove_all("\\s") %>%
    unique()

  if (length(allowed_strings) == 0) {
    # Early return if no patterns are provided or no frequent words found.
    return(rep("non_categorical", length(x)))
  }

  target_regex <- paste0(
    "^(",
    stringr::str_c(allowed_strings, collapse = "|"),
    ")$"
  )

  # Call the core pattern matching function with logical-specific configurations.
  core_prep_check_str_pattern(
    x = x,
    lookup_regex = list(
      target = target_regex,
      exception = NULL # No exceptions needed for logical checking.
    ),
    pattern_class = list(
      target = "categorical",
      exception = "non_categorical", # Not used as exception is NULL.
      blank = "blank",
      default = "non_categorical"
    ),
    allowed_strings = NULL # No allowed strings for categorical checking.
  )
}





# Detect the type of data in a character vector based on its content pattern.
core_prep_detect_type_based_on_str_plain <- function(
    x,
    prior_type_info = NULL,
    num_allowed_strings = NULL,
    date_allowed_strings = NULL,
    time_allowed_strings = NULL,
    logical_allowed_strings = NULL,
    categorical_allowed_strings = NULL,
    check_sequence = c("time", "date", "logical", "numeric", "categorical"),
    honour_blank_detection = TRUE
) {

  if (length(x) == 0) {
    return(character(0))
  }

  detect_steps <- list(
    time = list(
      fn = core_prep_check_str_time,
      allowed_strings = time_allowed_strings,
      tag = "time",
      result_value = "time"
    ),
    date = list(
      fn = core_prep_check_str_date,
      allowed_strings = date_allowed_strings,
      tag = "date",
      result_value = "date"
    ),
    logical = list(
      fn = core_prep_check_str_logical,
      allowed_strings = logical_allowed_strings,
      tag = "logical",
      result_value = "logical"
    ),
    numeric = list(
      fn = core_prep_check_str_num,
      allowed_strings = num_allowed_strings,
      tag = "num",
      result_value = "numeric"
    ),
    categorical = list(
      fn = core_prep_check_str_categorical,
      allowed_strings = categorical_allowed_strings,
      tag = "categorical",
      result_value = "categorical"
    ),
    # Extra patterns for other categories of numbers
    # Typically scoped for probabilistic inference using another function
    numeric_most_relaxed = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_num(
          x,
          allowed_strings = allowed_strings,
          exclude_dates = FALSE, # Include possible dates as num: [more relaxed]
          use_num_regex = "most_relaxed"
        )
      },
      allowed_strings = num_allowed_strings,
      tag = "num",
      result_value = "numeric"
    ),
    numeric_relaxed = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_num(
          x,
          allowed_strings = allowed_strings,
          use_num_regex = "relaxed"
        )
      },
      allowed_strings = num_allowed_strings,
      tag = "num",
      result_value = "numeric"
    ),
    numeric_strict = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_num(
          x,
          allowed_strings = allowed_strings,
          use_num_regex = "strict"
        )
      },
      allowed_strings = num_allowed_strings,
      tag = "num",
      result_value = "numeric"
    ),
    numeric_most_relaxed_with_unit = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_num(
          x,
          allowed_strings = allowed_strings,
          use_num_regex = "strict"
        )
      },
      # added extra units to the allowed strings
      allowed_strings = union(
        num_allowed_strings,
        unlist(core_prep_numeric_context_tokens())),
      tag = "num",
      result_value = "numeric"
    ),
    date_relaxed = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_date(
          x,
          allowed_strings = allowed_strings,
          use_date_regex = "base_with_qtrs_with_fy_extra"
        )
      },
      allowed_strings = union(
        date_allowed_strings,
        c("fy")
      ),
      tag = "date",
      result_value = "date"
    ),
    time_relaxed = list(
      fn = function(x, allowed_strings = NULL) {
        core_prep_check_str_time(
          x,
          allowed_strings = allowed_strings,
          exclude_numbers = FALSE, # Include numbers as time: [more relaxed]
          use_date_regex = "base_with_qtrs_with_fy_extra"
        )
      },
      allowed_strings = time_allowed_strings,
      tag = "time",
      result_value = "time"
    )
  )


  # Validate check_sequence
  invalid_names <- setdiff(check_sequence, names(detect_steps))
  if (length(invalid_names)) {
    rlang::abort(
      paste0(
        "Invalid entries in check_sequence: ",
        paste(invalid_names, collapse = ", "),
        ". Allowed values: ",
        paste(names(detect_steps), collapse = ", ")
      ),
      call = NULL
    )
  }

  steps_to_check <- detect_steps[check_sequence]

  # Prior info logic
  take_prior <- FALSE
  if(length(prior_type_info) == length(x)){
    if(length(setdiff(
      prior_type_info,
      core_cell_recognized_format()
    )) == 0){
      take_prior <- TRUE
    }
  }

  if(take_prior){
    result <- prior_type_info
    to_check <- prior_type_info == "character"
  }else{
    result <- rep("character", length(x))
    to_check <- rep(TRUE, length(x))
  }

  # Step 1: Blanks
  is_blank <- is.na(x) | (stringr::str_trim(x) == "")
  result[is_blank] <- "blank"
  to_check[is_blank] <- FALSE

  # Step 2: Classification loop
  for (step in steps_to_check) {
    if (!any(to_check)) break
    current_indices <- which(to_check)
    fn_args <- list(
      x = x[current_indices],
      allowed_strings = step$allowed_strings
    )
    # Actual function call
    res <- do.call(step$fn, fn_args)
    is_match <- res == step$tag
    original_indices <- current_indices[is_match]
    result[original_indices] <- step$result_value
    to_check[original_indices] <- FALSE

    if(honour_blank_detection) {
      # If we are honoring blank detection, we need to skip the blanks for
      # further checks
      is_match <- res == "blank"
      original_indices <- current_indices[is_match]
      result[original_indices] <- "blank"
      to_check[original_indices] <- FALSE

    }
  }

  return(result)
}



#' Detect data type in a character vector based on content patterns
#'
#' Classifies each element of a character vector as `"numeric"`, `"date"`,
#' `"time"`, `"logical"`, `"categorical"`, `"character"`, or `"blank"` by
#' applying specialized detectors in a configurable sequence. Optimized for
#' large vectors with many repeated values, using unique-value optimization.
#'
#' @param x Character vector to classify.
#' @param prior_type_info Optional vector of prior type assignments (same length
#'   as `x`). If provided and valid, all non-`"character"` elements are
#'   preserved and only `"character"` elements are further classified.
#' @param num_allowed_strings,date_allowed_strings,time_allowed_strings,logical_allowed_strings,categorical_allowed_strings
#'  Optional character vectors of sub-strings to ignore for detection of their
#'  respective types (or in other words are allowed to coexist with value).
#'  (except for `categorical_allowed_strings`, which is used for categorical
#'  sampling.)
#' @param check_sequence Character vector specifying the order (subset of
#'   `"time"`, `"date"`, `"logical"`, `"numeric"`, `"categorical"`) in which
#'   type detectors are applied. The first match is assigned.
#' @param honour_blank_detection Logical (default `TRUE`). If `TRUE`, blanks
#'   detected at any stage are immediately classified as `"blank"` and skipped
#'   in further checks.
#'
#' @return A character vector of detected types, same length as `x`.
#' @keywords internal
core_prep_detect_type_based_on_str <- function(
    x,
    prior_type_info = NULL,
    num_allowed_strings = NULL,
    date_allowed_strings = NULL,
    time_allowed_strings = NULL,
    logical_allowed_strings = NULL,
    categorical_allowed_strings = NULL,
    check_sequence = c("time", "date", "logical", "numeric", "categorical"),
    honour_blank_detection = TRUE
) {

  # Specific tweak for categorical_allowed_strings as it is applied using
  # util_vector_operation_optim which will lose frequency info
  if(is.null(categorical_allowed_strings)) {
    categorical_allowed_strings <- core_prep_frequent_words(x)
  }

  util_vector_operation_optim(
    x = x,
    f = core_prep_detect_type_based_on_str_plain,
    paired_arguments = "prior_type_info",
    prior_type_info = prior_type_info,
    num_allowed_strings = num_allowed_strings,
    date_allowed_strings = date_allowed_strings,
    time_allowed_strings = time_allowed_strings,
    logical_allowed_strings = logical_allowed_strings,
    categorical_allowed_strings = categorical_allowed_strings,
    check_sequence = check_sequence
  )
}


# This is slightly different resource consuming function than the above.
# It calculates probability of being certain types based on the string patterns.
core_prep_prob_of_type_based_on_str <- function(
    x,
    prior_type_info = NULL,
    num_allowed_strings = NULL,
    date_allowed_strings = NULL,
    time_allowed_strings = NULL,
    logical_allowed_strings = NULL,
    categorical_allowed_strings = NULL,
    numeric_unit_check = FALSE
) {

  if (length(x) == 0) {
    return(character(0))
  }

  if(numeric_unit_check){
    all_patterns <- c(
      "time", "date", "logical", "numeric", "categorical",
      "numeric_most_relaxed", "numeric_relaxed", "numeric_strict",
      "numeric_most_relaxed_with_unit", "date_relaxed","time_relaxed"
    )

    # wts for each type
    # c(
    #   "logical", -> 1
    #   "categorical", ->1
    #   # numeric types
    #   "numeric","numeric_most_relaxed", "numeric_relaxed", "numeric_strict",
    #   "numeric_most_relaxed_with_unit", -> 5
    #   "date", "date_relaxed" ->2
    #   "time", "time_relaxed" -> 2
    # )

    wts_for_type <- c(
      logical = 1,
      categorical = 1,
      numeric = 5,
      date = 2,
      time = 2
    )


  } else {

    # Excluding numeric_most_relaxed_with_unit which is most computationally
    # expensive
    all_patterns <- c(
      "time", "date", "logical", "numeric", "categorical",
      "numeric_most_relaxed", "numeric_relaxed", "numeric_strict",
      "date_relaxed","time_relaxed"
    )

    # wts for each type
    # c(
    #   "logical", -> 1
    #   "categorical", ->1
    #   # numeric types
    #   "numeric","numeric_most_relaxed", "numeric_relaxed", "numeric_strict" -> 4
    #   "date", "date_relaxed" ->2
    #   "time", "time_relaxed" -> 2
    # )

    wts_for_type <- c(
      logical = 1,
      categorical = 1,
      numeric = 4,
      date = 2,
      time = 2
    )

  }




  # create two entry for character and blank
  wts_for_type <- c(
    wts_for_type,
    c(
      character = sum(wts_for_type),
      blank = sum(wts_for_type)
    )
  )

  check_res <- purrr::map_dfc(
    all_patterns, ~{
      u <- core_prep_detect_type_based_on_str(
        x,
        prior_type_info = prior_type_info,
        num_allowed_strings = num_allowed_strings,
        date_allowed_strings = date_allowed_strings,
        time_allowed_strings = time_allowed_strings,
        logical_allowed_strings = logical_allowed_strings,
        categorical_allowed_strings = categorical_allowed_strings,
        check_sequence = .x
      )
      d0 <- tibble::tibble(v1 = u)
      colnames(d0) <- .x
      d0
    })


  recognized_types <- core_cell_recognized_format()

  # Prior info logic
  take_prior <- FALSE
  if(length(prior_type_info) == length(x)){
    if(length(setdiff(
      prior_type_info,
      recognized_types
    )) == 0){
      # here further check to see prior_type_info has anything apart from character and blank
      if(length(setdiff(prior_type_info,
                        c("character", "blank"))) > 0){
        # if it has anything apart from character and blank, then we can take prior
        take_prior <- TRUE
      }
    }
  }

  if(take_prior){
    check_res$prior_type_info <- prior_type_info
    wts_for_type[unique(check_res$prior_type_info)] <-
      wts_for_type[unique(check_res$prior_type_info )]+1
  }

  type_counts <- t(apply(check_res, 1, function(row) {
    counts <- table(factor(row, levels = recognized_types))
    as.integer(counts)
  }))

  type_counts_df <- as.data.frame(type_counts)
  colnames(type_counts_df) <- recognized_types
  rownames(type_counts_df) <- NULL

  type_probs_df <- sweep(type_counts_df, 2,
                         wts_for_type[colnames(type_counts_df)], "/")
  # Calculate probabilities for each type

  type_probs_df[is.na(type_probs_df)] <- 0  # handle any 0/0
  type_probs_df[type_probs_df>1] <- 1  # handle any wrong probability

  type_probs_df <- tibble::as_tibble(type_probs_df)

  return(type_probs_df)
}

# --- Helper Functions for Pre-processing Cells ---
# --- Value/Attribute Classification ---

# This one assumes that: logical, categorical, date, time are part of
# attribute.
core_prep_va_simple_heuristic <- function(d, honour_read_type = TRUE) {
  # Assumes d as proper cells and it's verified before calling this function

  if(honour_read_type) {
    # Init with already read format
    d$type  <- d$data_type
  } else {
    d$type <- rep("character", nrow(d))
  }

  d$type <- core_prep_detect_type_based_on_str(
    x = d$value,
    prior_type_info = d$type
  )

  d$PoA <- dplyr::case_when(
    d$type == "date" ~ 1,
    d$type == "time" ~ 1,
    d$type == "logical" ~ 1,
    d$type == "categorical" ~ 1,
    d$type == "numeric" ~ 0,
    d$type == "blank" ~ 0,
    d$type == "character" ~ 1,
    TRUE ~ 0
  )

  d$PoV <- dplyr::case_when(
    d$type == "date" ~ 0,
    d$type == "time" ~ 0,
    d$type == "logical" ~ 0,
    d$type == "categorical" ~ 0,
    d$type == "numeric" ~ 1,
    d$type == "blank" ~ 0,
    d$type == "character" ~ 0,
    TRUE ~ 0
  )

  d
}

# This one assumes that: There is an additional column named `av_class_tag` (a
# character containing only attr, val, empty) created manually or otherwise
# which indicates whether the cell is a value or an attribute. This is typically
# used in manual data entry or when the data is already pre-processed to have
# this information.
core_prep_va_manual <- function(d) {
  # Assumes d as proper cells and it's verified before calling this function

  # Check if the `av_class_tag` column exists
  if(!"av_class_tag" %in% colnames(d)) {
    rlang::abort(
      "The cells must contain tagged in 'av_class_tag'.",
      call = NULL)
  }

  # Check if av_class_tag is character and contains only "attr", "val", or "empty"
  if(!is.character(d$av_class_tag) ||
     (length(setdiff(d$av_class_tag, c("attr", "val", "empty"))) > 0) ) {
    rlang::abort(
      "The 'av_class_tag' must be a character vector containing only 'attr', 'val', or 'empty'.",
      call = NULL)
  }



  # Init with already read format
  d$type  <- d$data_type

  # Set PoA and PoV based on `av_class_tag` only

  d$PoA <- dplyr::case_when(
    d$av_class_tag == "attr" ~ 1,
    TRUE ~ 0
  )

  d$PoV <- dplyr::case_when(
    d$av_class_tag == "val" ~ 1,
    TRUE ~ 0
  )

  d
}


# This approach assumes that types such as logical, categorical, date, and time
# can appear either as values or as attributes. It accommodates dynamic
# attribute detection in cases where more attributes than expected are present
# in non-single-sided data. Note: This method is more resource-intensive and
# uses probabilistic type casting, focusing on the probability of each type
# rather than a definite type conclusion. The final PoV and PoA are determined
# based on the resulting conditional probability distribution.
core_prep_va_heuristic_probabilistic <- function(d, honour_read_type = TRUE) {
  # Assumes d as proper cells and it's verified before calling this function

  if(honour_read_type) {
    # Init with already read format
    d$type  <- d$data_type
  } else {
    d$type <- rep("character", nrow(d))
  }

  prob_type <- core_prep_prob_of_type_based_on_str(
    x = d$value,
    prior_type_info = d$type
  )

  # ETC Adjustments in Probability
  # Adjustment in Date Probability : Possibility of a number to be a year

  etc_prob_adj <- core_prep_prob_of_being_year(d$value)

  if(any(etc_prob_adj>0.6)){
    # If the probability of being a year is high, adjust the date probability
    prob_type$date <- prob_type$date + etc_prob_adj * 0.5
    prob_type$numeric <- pmax(prob_type$numeric - etc_prob_adj * 0.5,0)
  }

  # Assign the type corresponding to the highest probability for each row. This
  # is primarily for tracking or debugging purposes and can be discarded later
  # if not needed.
  d$type <- colnames(prob_type)[max.col(
    as.matrix(prob_type), ties.method = "first")]

  # Note that "numeric", "character", "logical", "date", "time", "categorical",
  # "blank" are not exhaustive so "character" -->> ("logical", "date", "time",
  # "categorical") So PoA_given_type and PoV_given_type are not exhaustive. if
  # we define like this :

  # PoA_given_type <- c(
  #   date = 1,
  #   time = 1,
  #   logical = 1,
  #   categorical = 1,
  #   numeric = 0,
  #   blank = 0,
  #   character = 1
  # )
  #
  # PoV_given_type <- c(
  #   date = 0,
  #   time = 0,
  #   logical = 0,
  #   categorical = 0,
  #   numeric = 1,
  #   blank = 0,
  #   character = 0
  # )

  # Instead we join prob_type DF and define PoA_given_type and PoV_given_type as
  # follows:

  # Only three types are near exhaustive: "numeric", "character_like", "blank"
  # character_like is combination of "logical", "date", "time", "categorical",
  # "character" (it can be based on max values of either of them or weighted
  # average of them) (weighted average is not used here)
  prob_type_exhaustive <- prob_type %>%
    dplyr::mutate(
      character_like = pmax(
        .data$logical, .data$date, .data$time, .data$categorical, .data$character
      )
    ) %>%
    dplyr::select(
      "numeric", "character_like", "blank"
    )

  # Please note even though the prob_type_exhaustive is not exhaustive as row
  # sums may exceed 1,

  PoA_given_type <- c(
    numeric = 0,
    blank = 0,
    character_like = 1
  )

  PoV_given_type <- c(
    numeric = 1,
    blank = 0,
    character_like = 0
  )

  # Ensure PoA_given_type and PoV_given_type are in the same order as prob_type_exhaustive
  PoA_given_type <- PoA_given_type[colnames(prob_type_exhaustive)]

  # Adjustments for non disjoint sets using rowSums(prob_type)
  PoA_scores <- as.numeric(as.matrix(prob_type_exhaustive) %*% PoA_given_type) /
    rowSums(prob_type_exhaustive)

  # Ensure PoV_given_type is in the same order as prob_type
  PoV_given_type <- PoV_given_type[colnames(prob_type_exhaustive)]

  # Adjustments for non disjoint sets using rowSums(prob_type)
  PoV_scores <- as.numeric(as.matrix(prob_type_exhaustive) %*% PoV_given_type) /
    rowSums(prob_type_exhaustive)

  d$PoA <- pmax(pmin(PoA_scores, 1),0)

  d$PoV <- pmax(pmin(PoV_scores, 1),0)

  # recheck integrity of PoA and PoV
  chk <- d$PoA+d$PoV

  # re-normalize PoA and PoV if chk > 1
  if(any(chk > 1)) {
    d$PoA[chk > 1] <- d$PoA[chk > 1] / chk[chk > 1]
    d$PoV[chk > 1] <- d$PoV[chk > 1] / chk[chk > 1]
  }

  d
}
