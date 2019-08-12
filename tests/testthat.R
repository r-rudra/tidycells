library(testthat)
library(tidycells)



test_results <- test_check("tidycells")

# get extra information
as.data.frame(test_results)[c("file", "test", "failed", "passed", "skipped", "error", "warning", "real")]
