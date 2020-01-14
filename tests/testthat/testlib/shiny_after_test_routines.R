
# run this line (from project root before check) to pull all tests
# Sys.setenv(SHINYTEST_PULL = normalizePath("."))

library(purrr)

pick_up_test_outputs <- function(cdir, edir){
  target <- Sys.getenv("SHINYTEST_PULL")
  if (nchar(target) > 0) {
    if (file.exists(target) & identical(Sys.getenv("NOT_CRAN"), "true")) {
      if(file.exists(cdir)){
        target_test <- file.path(target, "tests", "testthat" ,get_after_root_in_testthat(edir))
        dir.create(target_test, showWarnings = FALSE, recursive = TRUE)
        fhere <- list.files(cdir, full.names = TRUE)
        file.copy(fhere, target_test, overwrite = TRUE)
        # intentional file injection for warning / notification that the test-results has been copied
        warn_file <- file.path(target, "WARNFILE")
        writeLines(paste0("Test pulled at ", Sys.time()), warn_file)
      }
    }
  }
}

get_after_root_in_testthat <- function(this_dir){
  before <- this_dir
  aft <-NULL
  if(!stringr::str_detect(this_dir, "testthat")) stop("'testthat' not present in the path")
  repeat({
    if(basename(before)=="testthat"){
      if(!stringr::str_detect(dirname(before), "testthat")) break()
    }
    aft <- c(aft, basename(before))
    before <- dirname(before)
  })
  rev(aft) %>% as.list() %>%  do.call(file.path,args = .)
}

process_jsons <- function(this_dir, ndigi = 10){
  fls <- list.files(this_dir, pattern = "json", full.names = TRUE)

  fls %>%
    purrr::walk(~.x %>%
                  jsonlite::fromJSON() %>%
                  jsonlite::toJSON(digits = ndigi) %>%
                  jsonlite::prettify(indent = 2) %>%
                  writeChar(.x, eos = NULL))

}

graceful_stop <- function(app){
  p <- app$.__enclos_env__$private$shinyProcess
  p$interrupt()
  p$wait()
}

shiny_after_test_routines <- function(app){
  expected_dir  <- paste0(app$getSnapshotDir(), "-expected")
  current_dir  <- paste0(app$getSnapshotDir(), "-current")

  #  pickup the test results if SHINYTEST_PULL is set
  pick_up_test_outputs(current_dir, expected_dir)

  # pre_process both current and expected
  if(tolower(Sys.info()[["sysname"]])!="windows"){
    # for non-windows systems digits to be truncated
    process_jsons(expected_dir, 2)
    process_jsons(current_dir, 2)
  }else{
    # for Github LR formatting "LF will be replaced by CRLF"
    # ref https://github.com/rstudio/shinytest/issues/270
    process_jsons(expected_dir, 10)
    process_jsons(current_dir, 10)
  }

  # graceful stop
  graceful_stop(app)

}
