
# Developer's Note

Also Check [CRAN Comments](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/testlib/cran-comments.md)

### FAQs

##### 1) **Why there are two code coverages?**

The package contains optional functionality which are written as shiny widgets. These are given to the user as [`visual_*`](https://github.com/r-rudra/tidycells/blob/master/R/visual_functions.R) series of functions. Limited [tests](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/test-shiny.R) for these are developed and tested in a few testing environments. These tests are based on [shinytest](https://rstudio.github.io/shinytest/) package. The [covr](https://covr.r-lib.org/) package is not yet (at least the **CRAN** version) capable to track code coverages in shinytest [Ref: r-lib/covr [#277](https://github.com/r-lib/covr/issues/277)]. Also note that shinytest is not yet taking widget-based functions [Ref: rstudio/shinytest [#157](https://github.com/rstudio/shinytest/issues/157)](at least the **CRAN** version). That is why a set of [functions](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/testlib/shiny_test.R) is introduced to run tests for shiny. 

On these grounds, codecov [![Codecov test
coverage](https://codecov.io/gh/r-rudra/tidycells/branch/master/graph/badge.svg)](https://codecov.io/gh/r-rudra/tidycells?branch=master) is used to give full coverage (_without any restiction_) (_ideally this should increase provided the support for covr is introduced in shinytest and covr both_). While the coveralls [![Coveralls test
coverage](https://coveralls.io/repos/github/r-rudra/tidycells/badge.svg)](https://coveralls.io/r/r-rudra/tidycells?branch=master) shows the coverage excluding `shiny_*.R` and `visual_*.R` files (_showing the coverage for only main functionality_)



### Dependency Explained

Note the package has "Induced System Dependency" which is causing to break the code sometimes in [R-hub](https://builder.r-hub.io/). Below table describes the same.

| Package    | Type     | Reason                                             | Implied Critical Dependency | Induced System Dependency |
|------------|----------|----------------------------------------------------|-----------------------------|---------------------------|
| magrittr   | Imports  | core                                               |                             |                           |
| dplyr      | Imports  | core                                               |                             |                           |
| purrr      | Imports  | core                                               |                             |                           |
| stringr    | Imports  | core                                               |                             |                           |
| ggplot2    | Imports  | plots                                              | Rcpp                        |                           |
| tibble     | Imports  | core                                               |                             |                           |
| tidyr      | Imports  | core                                               |                             |                           |
| rlang      | Imports  | core                                               |                             |                           |
| unpivotr   | Imports  | core                                               | xml2                        | libxml2                   |
| methods    | Imports  | base                                               |                             |                           |
| utils      | Imports  | base                                               |                             |                           |
| graphics   | Imports  | base                                               |                             |                           |
| testthat   | Suggests | tests                                              |                             |                           |
| readr      | Suggests | read csv                                           |                             |                           |
| tidyxl     | Suggests | read xlsx                                          |                             |                           |
| plotly     | Suggests | optional interactive ggplot2 in visual_* functions | httr, openssl               | openssl / libssl          |
| DT         | Suggests | for visual_traceback plots                         |                             |                           |
| shiny      | Suggests | for visual_* functions                             |                             |                           |
| miniUI     | Suggests | for visual_* functions                             |                             |                           |
| rstudioapi | Suggests | object selector in Rstudio                         |                             |                           |
| knitr      | Suggests | vignettes                                          |                             |                           |
| rmarkdown  | Suggests | vignettes                                          |                             |                           |
| XML        | Suggests | read html like files                               |                             |                           |
| docxtractr | Suggests | read doc and docx                                  |                             |                           |
| readxl     | Suggests | read xls                                           |                             |                           |
| tabulizer  | Suggests | read pdf                                           |                             |                           |
| xlsx       | Suggests | read xls (prefered option)                         |                             |                           |
| covr       | Suggests | code coverage                                      |                             |                           |
| cli        | Suggests | nice prints                                        |                             |                           |
| shinytest  | Suggests | shiny module tests                                 |                             |                           |