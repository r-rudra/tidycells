
# Developer's Note

Also Check [CRAN Comments](https://github.com/r-rudra/tidycells/blob/master/cran-comments.md)

### Contributing

You are most welcome to contribute to this project in any means. 

Apart from opening an issue in Github (preferably with [reprex](https://reprex.tidyverse.org/)), you can also contribute mainly in the "Heuristic Maturation" process for `tidycells::analyze_cells` and `tidycells::collate_columns`. If any issue is specific to a data which you would like to share with me, there is a friendly function to mask your data using `tidycells:::mask_data` (_this function is not exported to avoid possible name conflicts_).


### FAQs

##### 1) **Why there are two code coverages?**

The package contains optional functionality which are written as shiny widgets. These are given to the user as [`visual_*`](https://github.com/r-rudra/tidycells/blob/master/R/visual_functions.R) series of functions. Limited [tests](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/test-shiny.R) for these are developed and tested in a few testing environments. These tests are based on [shinytest](https://rstudio.github.io/shinytest/) package. The [covr](https://covr.r-lib.org/) package is not yet (at least the **CRAN** version) capable to track code coverages in shinytest [Ref: r-lib/covr [#277](https://github.com/r-lib/covr/issues/277)]. Also note that shinytest is not yet taking widget-based functions [Ref: rstudio/shinytest [#157](https://github.com/rstudio/shinytest/issues/157)] (at least the **CRAN** version). That is why a set of [functions](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/testlib/shiny_test.R) is introduced to run tests for shiny. 

On these grounds, codecov [![Codecov test coverage](https://codecov.io/gh/r-rudra/tidycells/branch/master/graph/badge.svg)](https://codecov.io/gh/r-rudra/tidycells?branch=master) is used to give full coverage (_without any restiction_) (_ideally this should increase provided the support for covr is introduced in shinytest (and covr). Also "brush input fractional mismatch" (mentioned below) issue gets some resolution_) . While the coveralls [![Coveralls test coverage](https://coveralls.io/repos/github/r-rudra/tidycells/badge.svg?branch=master)](https://coveralls.io/r/r-rudra/tidycells?branch=master) shows the coverage excluding `shiny_*.R` and `visual_*.R` files (_showing the coverage for only main functionality_)

##### 2) **Where the shiny modules are tested?**

The shiny tests are only carried out in selected testing environments because of several difficulties. The difficulties are listed below.


**Difficulties during shiny test:**

* [shinytest](https://rstudio.github.io/shinytest/) does not directly provide testing functions like `visual_*` (which are based on shiny widgets). This is the reason a set of complicated code is used during the test.
* The input [`plot brush`](https://shiny.rstudio.com/articles/plot-interaction.html) is changing in fractional values under different OS. Most of the functionalities are recorded with brush input which slightly differs. Since the `JSON` to `JSON` comparison is strict now, these are resulting in test failures. 
* Sometimes GitHub push and pull is changing JSON slightly [getting `LF will be replaced by CRLF` warning.] (Full message: _The file will have its original line endings in your working directory. warning: LF will be replaced by CRLF_). It is solved using tar files (which untar on the fly). This is the reason the all recorded tests (includes JSON) are compress to tar in [tidycells/tests/testthat/testshiny/](https://github.com/r-rudra/tidycells/tree/master/tests/testthat/testshiny). 

Given these difficulties, the shiny tests are tested in the following environments only.

| Test Environment | OS                                      | R Version                                   | Screenshot Tested |
|------------------|-----------------------------------------|---------------------------------------------|-------------------|
| Local            | Windows 10 x86 Build 9200               | R version 3.6.1 (2019-07-05)                | yes               |
| Local            | Windows 10 x64 Build 17134              | R version 3.6.0 (2019-04-26)                | yes               |
| AppVeyor         | Windows Server 2012 R2 x64 (build 9600) | R version 3.6.1 Patched (2019-07-24 r76894) | no                |

**Note**: the screen-shots are also tested (apart from JSON test) (can be tested if `plotly` is not present as the tests are recorded without `plotly`).


### Current State of Development and Way Forward

  - [x] Test it in [**r-hub**](https://builder.r-hub.io/)
  - [x] Test for optional shiny modules (series of `visual_*` functions)
  - [x] Write more tests (increase coverage)
  - [ ] Releasing this package to [**CRAN**](https://cran.r-project.org/submit.html)
  - [ ] Write `collate_columns` function to deal with similar columns in composed data.frame
  - [ ] Write more vignettes on other topics
  - [ ] Making a pkgdown site
  - [ ] Making cell analysis little faster
  

### R-hub other builds

See other successful builds in [CRAN Comments](https://github.com/r-rudra/tidycells/blob/master/cran-comments.md)

#### Minor Issues

* Oracle Solaris 10, x86, 32 bit
  * R-patched

**Result** : WARNING

**Reason** : Pandoc issues in re-building vignettes

* Fedora Linux, 
  * R-devel, clang, gfortran

**Result** : NOTE

**Reason** : Packages suggested but not available for checking: 'tabulizer', 'xlsx'

* CentOS 6
  * stock R from EPEL

**Result** : NOTE

**Reason** : Packages suggested but not available for checking: ‘tidyxl’ ‘plotly’

####  Errors

* Debian Linux, 
  * R-devel, clang, ISO-8859-15 locale
  * R-devel, GCC
  * R-devel, GCC, no long double
  * R-patched, GCC
  * R-release, GCC
  * R-devel, GCC ASAN/UBSAN
* Ubuntu Linux 16.04 LTS
  * R-devel, GCC
  * R-release, GCC
  * R-devel with rchk

**Result** : PREPERROR

**Reason** : xml2 and httr failed to installed due to system dependency (libxml2, libssl/openssl)


**Note** : Neither of these errors (or notes) are attributable to the package as they failed because of induced system dependency or optional package dependency.


**All R-hub build summary**

| OS                                     | R Version                                                                                                | Result    |
|----------------------------------------|----------------------------------------------------------------------------------------------------------|-----------|
| macOS 10.11 El Capitan                 | (R-release) R version 3.6.0 (2019-04-26)                                                                 | SUCCESS   |
| Windows Server 2008 R2 SP1             | (R-devel) R Under development (unstable) (2019-07-04 r76780)                                             | SUCCESS   |
| Windows Server 2008 R2 SP2             | (R-oldrel) R version 3.5.3 (2019-03-11)                                                                  | SUCCESS   |
| Windows Server 2008 R2 SP3             | (R-patched) R version 3.6.0 Patched (2019-06-21 r76731)                                                  | SUCCESS   |
| Windows Server 2008 R2 SP4             | (R-release) R version 3.6.1 (2019-07-05)                                                                 | SUCCESS   |
| Windows Server 2012                    | (R-devel, Rtools4.0, 32/64 bit) R version 3.6.0 Under development   (Testing Rtools) (2019-02-27 r76167) | SUCCESS   |
| Fedora Linux                           | R-devel, GCC                                                                                             | SUCCESS   |
| CentOS 6 with Redhat Developer Toolset | (R from EPEL) R version 3.5.2 (2018-12-20)                                                               | SUCCESS   |
| **WARNING**                            | **Reason : _induced system dependency_**                                                                 |           |
| Oracle Solaris 10, x86, 32 bit         | R-patched                                                                                                | WARNING   |
| **NOTE**                               | **Reason : _optional package dependency_**                                                               |           |
| Fedora Linux                           | R-devel, clang, gfortran                                                                                 | NOTE      |
| CentOS 6                               | stock R from EPEL                                                                                        | NOTE      |
| **PREPERROR**                          | **Reason : _induced system dependency_**                                                                 |           |
| Debian Linux                           | R-devel, clang, ISO-8859-15 locale                                                                       | PREPERROR |
| Debian Linux                           | R-devel, GCC                                                                                             | PREPERROR |
| Debian Linux                           | R-devel, GCC, no long double                                                                             | PREPERROR |
| Debian Linux                           | R-patched, GCC                                                                                           | PREPERROR |
| Debian Linux                           | R-release, GCC                                                                                           | PREPERROR |
| Debian Linux                           | R-devel, GCC ASAN/UBSAN                                                                                  | PREPERROR |
| Ubuntu Linux 16.04 LTS                 | R-devel, GCC                                                                                             | PREPERROR |
| Ubuntu Linux 16.04 LTS                 | R-release, GCC                                                                                           | PREPERROR |
| Ubuntu Linux 16.04 LTS                 | R-devel with rchk                                                                                        | PREPERROR |

### Dependency Explained

Note the package has "Induced System Dependency" which is causing to break the code sometime in [R-hub](https://builder.r-hub.io/). Below table describes the same (note that below table is an indicative list and may not be complete.).

| Package    | Type     | Reason                                               | Implied Critical   Dependency | Induced System   Dependency          |
|------------|----------|------------------------------------------------------|-------------------------------|--------------------------------------|
| magrittr   | Imports  | core                                                 |                               |                                      |
| dplyr      | Imports  | core                                                 |                               |                                      |
| purrr      | Imports  | core                                                 |                               |                                      |
| stringr    | Imports  | core                                                 |                               |                                      |
| ggplot2    | Imports  | plots                                                | Rcpp                          |                                      |
| tibble     | Imports  | core                                                 |                               |                                      |
| tidyr      | Imports  | core                                                 |                               |                                      |
| rlang      | Imports  | core                                                 |                               |                                      |
| unpivotr   | Imports  | core                                                 | xml2                          | libxml2                              |
| methods    | Imports  | base                                                 |                               |                                      |
| utils      | Imports  | base                                                 |                               |                                      |
| graphics   | Imports  | base                                                 |                               |                                      |
| testthat   | Suggests | tests                                                |                               |                                      |
| readr      | Suggests | read csv                                             |                               |                                      |
| tidyxl     | Suggests | read xlsx                                            |                               |                                      |
| plotly     | Suggests | optional interactive   ggplot2 in visual_* functions | httr, openssl                 | openssl / libssl                     |
| DT         | Suggests | for visual_traceback   plots                         |                               |                                      |
| shiny      | Suggests | for visual_*   functions                             | httr, openssl                 | openssl / libssl                     |
| miniUI     | Suggests | for visual_*   functions                             |                               |                                      |
| rstudioapi | Suggests | object selector in   Rstudio                         |                               |                                      |
| knitr      | Suggests | vignettes                                            |                               |                                      |
| rmarkdown  | Suggests | vignettes                                            |                               |                                      |
| XML        | Suggests | read html like files                                 |                               |                                      |
| docxtractr | Suggests | read doc and docx                                    |                               | LibreOffice   (Suggested Dependency) |
| readxl     | Suggests | read xls                                             |                               |                                      |
| tabulizer  | Suggests | read pdf                                             | rJava                         | Java                                 |
| xlsx       | Suggests | read xls (preferred   option)                        | rJava                         | Java                                 |
| covr       | Suggests | code coverage                                        |                               |                                      |
| cli        | Suggests | nice prints                                          |                               |                                      |
| shinytest  | Suggests | shiny module tests                                   |                               |                                      |

