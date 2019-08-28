
# Developer's Note

Also Check [CRAN Comments](https://github.com/r-rudra/tidycells/blob/master/cran-comments.md)

### Dependability

If a user builds a continuous workflow of data wrangling starting with some functions from `tidycells` package or includes `tidycells` in their package, then proper precautions should be taken as `tidycells` functions are heuristic-based. One can face problem like column `collated_2` has been renamed to `collated_3` etc. 
 
The package has two main functions which may raise some dependability issues in future. These functions are `tidycells::analyze_cells` and `tidycells::collate_columns` (which are based on heuristics and internal statistical logic). The main cause for potential output variation across different releases (future CRAN releases) of this package might be due to changes in `tidycells::analyze_cells`. Since `tidycells::read_cells` is dependent on these functions, it will also be affected equally. 
 
The package has been developed observing certain types of oddly structured data available to the developer (me). However, if any user has any issues in automatic understanding of the underlying structure, it is expected that the same will be attempted to address in a future release (provided the user inform me the issue). This may be referred to as the "Heuristic Maturation" process for these two functions. As and when the tests written for these functions requires a modification (which means the output column name and other major details has been changed) I'll bump the <minor> (version convention <major>.<minor>.<patch>) version in the release. If only <patch> is changed since a user last used this package, then possibly you can depend on this package without any worry. 
 
After first CRAN release when next <minor> version will be released in CRAN hopefully I'll be able to provide you with a _compatibility checker function_ to work out and find potential tendency issues.
 
Don't worry I'll try my best not to break your code intentionally. This is just a message to you so that you can build careful dependency on this package.
 

### Contributing

You are most welcome to contribute to this project in any means. 

Apart from opening an issue in Github (preferably with [reprex](https://reprex.tidyverse.org/)), you can also contribute mainly to the "Heuristic Maturation" process for `tidycells::analyze_cells` and `tidycells::collate_columns`. If any issue is specific to a data which you would like to share with me, there is a friendly function to mask your data using `tidycells:::mask_data` (_this function is not exported to avoid possible name conflicts_).


### FAQs

##### 1) **Why there are two code coverages?**

The package contains optional functionality which are written as shiny widgets. These are given to the user as [`visual_*`](https://github.com/r-rudra/tidycells/blob/master/R/visual_functions.R) series of functions. Limited [tests](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/test-shiny.R) for these are developed and tested in a few testing environments. These tests are based on [shinytest](https://rstudio.github.io/shinytest/) package. The [covr](https://covr.r-lib.org/) package is not yet (at least the **CRAN** version) capable to track code coverages in shinytest [Ref: r-lib/covr [#277](https://github.com/r-lib/covr/issues/277)]. Also note that shinytest is not yet taking widget-based functions [Ref: rstudio/shinytest [#157](https://github.com/rstudio/shinytest/issues/157)] (at least the **CRAN** version). That is why a set of [functions](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/testlib/shiny_test.R) is introduced to run tests for shiny. 

On these grounds, codecov [![Codecov test coverage](https://codecov.io/gh/r-rudra/tidycells/branch/master/graph/badge.svg)](https://codecov.io/gh/r-rudra/tidycells?branch=master) is used to give full coverage (_without any restrictions_) (_ideally this should increase provided the support for covr is introduced in shinytest (and covr). Also "brush input fractional mismatch" (mentioned below) issue gets some resolution_) . While the coveralls [![Coveralls test coverage](https://coveralls.io/repos/github/r-rudra/tidycells/badge.svg?branch=master)](https://coveralls.io/r/r-rudra/tidycells?branch=master) shows the coverage excluding `shiny_*.R` and `visual_*.R` files (_showing the coverage for only main functionality_)

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
| AppVeyor         | Windows Server 2012 R2 x64 (build 9600) | R version 3.6.1 (2019-07-05)                | no                |
| AppVeyor         | Windows Server 2012 R2 x64 (build 9600) | R version 3.5.3 (2019-03-11)                | no                |

**Note**: the screen-shots are also tested (apart from JSON test).


### Current State of Development and Way Forward

Check trackable version [here](https://github.com/r-rudra/tidycells/issues/2).

  - [x] Test it in [**r-hub**](https://builder.r-hub.io/)
  - [x] Test for optional shiny modules (series of `visual_*` functions)
  - [x] Write more tests (increase coverage)
  - [x] Write `collate_columns` function to deal with similar columns in composed data.frame
  - [x] Making a pkgdown site
  - [x] Releasing this package to [**CRAN**](https://cran.r-project.org/submit.html)
  - [x] Make [doc test](https://github.com/r-rudra/tidycells/blob/master/tests/testthat/test-etc.R) skip on CRAN.
  - [x] Make possibility for `purrr` like formula, e.g. ~ .x for `tidycells::value_attribute_classify`
  - [ ] A `compatibility function` for the "Heuristic Maturation" process (after CRAN)
  - [ ] Write blog + add it to [R blogger](https://www.r-bloggers.com/add-your-blog/) and other sites
  - [ ] Send it to the [r-packages mailing list](https://stat.ethz.ch/mailman/listinfo/r-packages)
  - [ ] Explore options to add this in [CRAN Task Views](https://cran.r-project.org/web/packages/ctv/vignettes/ctv-howto.pdf)
  - [ ] make a [cheatsheet](https://www.rstudio.com/resources/cheatsheets/how-to-contribute-a-cheatsheet/)
  - [ ] Explore [SDMX](https://sdmx.org/?page_id=4649) Converter possibility
  - [ ] Explore other formats (containing unorganised tables) possibility. Check out [unoconv](http://dag.wiee.rs/home-made/unoconv/).
  - [ ] Write more vignettes on other topics
  - [ ] Making cell analysis little faster
  

### R-hub other builds

See other successful builds in [CRAN Comments](https://github.com/r-rudra/tidycells/blob/master/cran-comments.md)

#### Minor Issues

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


### All R-hub build summary

| OS                                     | R Version                                                                                                | Result    |
|----------------------------------------|----------------------------------------------------------------------------------------------------------|-----------|
| macOS 10.11 El Capitan                 | (R-release) R version 3.6.0 (2019-04-26)                                                                 | SUCCESS   |
| Oracle Solaris 10, x86, 32 bit         | (R-patched) R version 3.6.0 (2019-04-26)                                                                 | SUCCESS   |
| Windows Server 2008 R2 SP1             | (R-devel) R Under development (unstable) (2019-07-04 r76780)                                             | SUCCESS   |
| Windows Server 2008 R2 SP2             | (R-oldrel) R version 3.5.3 (2019-03-11)                                                                  | SUCCESS   |
| Windows Server 2008 R2 SP3             | (R-patched) R version 3.6.0 Patched (2019-06-21 r76731)                                                  | SUCCESS   |
| Windows Server 2008 R2 SP4             | (R-release) R version 3.6.1 (2019-07-05)                                                                 | SUCCESS   |
| Windows Server 2012                    | (R-devel, Rtools4.0, 32/64 bit) R version 3.6.0 Under development   (Testing Rtools) (2019-02-27 r76167) | SUCCESS   |
| Fedora Linux                           | (R-devel, GCC) R Under development (unstable) (2019-08-18 r77026)                                        | SUCCESS   |
| CentOS 6 with Redhat Developer Toolset | (R from EPEL) R version 3.5.2 (2018-12-20)                                                               | SUCCESS   |
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

To install `tidycells` (with bare minimum functionality) you need do following two things 

* `install.packages("tidyverse")` (This possibly you have already done)
* `install.packages("unpivotr")`

Rest packages are optional and can be installed as per your requirements. 

Note the package has "Induced System Dependency" which is causing to break the code sometime in [R-hub](https://builder.r-hub.io/). Below table describes the same (note that below table is an indicative list and may not be complete.).

| Package      | Type     | Reason                                                                  | Implied   Critical Dependency | Induced System   Dependency          |
|--------------|----------|-------------------------------------------------------------------------|-------------------------------|--------------------------------------|
| **Imports**  |          |                                                                         |                               |                                      |
| dplyr        | Imports  | core                                                                    |                               |                                      |
| ggplot2      | Imports  | plots                                                                   | Rcpp                          |                                      |
| graphics     | Imports  | base                                                                    |                               |                                      |
| magrittr     | Imports  | core                                                                    |                               |                                      |
| methods      | Imports  | base                                                                    |                               |                                      |
| purrr        | Imports  | core                                                                    |                               |                                      |
| rlang        | Imports  | core                                                                    |                               |                                      |
| stats        | Imports  | tidycells::collate_columns   --> tidycells:::similarity_score           |                               |                                      |
| stringr      | Imports  | core                                                                    |                               |                                      |
| tibble       | Imports  | core                                                                    |                               |                                      |
| tidyr        | Imports  | core                                                                    |                               |                                      |
| unpivotr     | Imports  | core                                                                    | xml2                          | libxml2                              |
| utils        | Imports  | base                                                                    |                               |                                      |
| **Suggests** |          |                                                                         |                               |                                      |
| cli          | Suggests | nice   prints                                                           |                               |                                      |
| covr         | Suggests | code   coverage                                                         |                               |                                      |
| docxtractr   | Suggests | read   doc and docx                                                     |                               | LibreOffice   (Suggested Dependency) |
| DT           | Suggests | for   visual_traceback plots                                            |                               |                                      |
| knitr        | Suggests | vignettes                                                               |                               |                                      |
| miniUI       | Suggests | for   visual_* functions                                                |                               |                                      |
| plotly       | Suggests | optional   interactive ggplot2 in visual_* functions                    | httr,   openssl               | openssl   / libssl                   |
| readr        | Suggests | read   csv                                                              |                               |                                      |
| readxl       | Suggests | read   xls                                                              |                               |                                      |
| rmarkdown    | Suggests | vignettes                                                               |                               |                                      |
| rstudioapi   | Suggests | object   selector in Rstudio                                            |                               |                                      |
| shiny        | Suggests | for   visual_* functions                                                | httr,   openssl               | openssl   / libssl                   |
| shinytest    | Suggests | shiny   module tests                                                    |                               |                                      |
| stringdist   | Suggests | tidycells::collate_columns   --> tidycells:::similarity_score (Enhance) |                               |                                      |
| tabulizer    | Suggests | read   pdf                                                              | rJava                         | Java                                 |
| testthat     | Suggests | tests                                                                   |                               |                                      |
| tidyxl       | Suggests | read   xlsx                                                             |                               |                                      |
| xlsx         | Suggests | read   xls (prefered option)                                            | rJava                         | Java                                 |
| XML          | Suggests | read   html like files                                                  |                               |                                      |
