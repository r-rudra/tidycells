
library(testthat)
library(tidycells)

# For identifying <lifecycle> induced warnings.
options(lifecycle_verbosity = "warning")

test_check("tidycells")
