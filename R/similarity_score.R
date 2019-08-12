
ads_score <- function(ads) {
  ads[ads < 0] <- 0
  ads[ads > 10^10] <- 10^10
  (stats::median(ads, na.rm = TRUE) * 5 + mean(ads, na.rm = TRUE) * 4 + min(ads, na.rm = TRUE) * 1) / 10
}

similarity_score <- function(x, y) {
  comm <- intersect(x, y)
  bscore <- 1 - length(comm) / mean(length(x), length(y))

  if (is.na(bscore)) bscore <- 0
  if (bscore > 10^10) bscore <- 10^10
  if (bscore < 0) bscore <- 0

  # Levenshtein distance
  ads <- utils::adist(x, y)

  ads_l <- list(ads)

  if (rlang::is_installed("stringdist")) {
    suppressMessages(suppressWarnings({
      mthds <- c("osa", "jaccard", "soundex", "jw")
      ads_sdl <- mthds %>% map(~ stringdist::stringdistmatrix(x, y, method = .x, p = 0.1, q = 3))
      ads_l <- c(ads_l, ads_sdl)
    }))
  }

  ascore <- ads_l %>% map_dbl(ads_score)
  c(ascore, bscore)
}
