
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

Given these difficulties, the shiny tests are tested in the following environments (and in all *Windows* environment listed below).

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

See whole build matrix below

**Note** : Neither of these errors (or notes) are attributable to the package as they failed because of induced system dependency or optional package dependency.


### All build summary

| Package   | Version | Submit Date | Where      | OS Type | OS Description                          | R Version                                                              | R Version Tag                                                     | Platform                           | State     |
|-----------|---------|-------------|------------|---------|-----------------------------------------|------------------------------------------------------------------------|-------------------------------------------------------------------|------------------------------------|-----------|
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Windows | Windows Server 2008 R2 SP1              | R version 3.6.2 (2019-12-12)                                           | R-release 32/64 bit                                               | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Windows | Windows Server 2008 R2 SP1              | R version 3.6.2 Patched (2019-12-12 r77564)                            | R-patched 32/64 bit                                               | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Windows | Windows Server 2008 R2 SP1              | R version 3.5.3 (2019-03-11)                                           | R-oldrel 32/64 bit                                                | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Windows | Windows Server 2012                     | R version 4.0.0 Under development (Testing Rtools) (2019-09-30 r77236) | R-devel Rtools4.0 32/64 bit                                       | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Windows | Windows Server 2008 R2 SP1              | R Under development (unstable) (2019-11-08 r77393)                     | R-devel 32/64 bit                                                 | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Ubuntu Linux 16.04 LTS                  | R Under development (unstable) (2020-01-03 r77629)                     | R-devel with rchk                                                 |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Ubuntu Linux 16.04 LTS                  |                                                                        | R-release GCC                                                     |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Ubuntu Linux 16.04 LTS                  | R Under development (unstable) (2020-01-03 r77629)                     | R-devel GCC                                                       |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Solaris | Oracle Solaris 10 x86 32 bit            | R version 3.6.0 (2019-04-26)                                           | R-patched                                                         | i386-pc-solaris2.10 (32-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | macOS   | macOS 10.11 El Capitan                  | R version 3.6.2 (2019-12-12)                                           | R-release                                                         | x86_64-apple-darwin15.6.0 (64-bit) | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            | R Under development (unstable) (2018-06-20 r74924)                     | R-devel GCC ASAN/UBSAN                                            |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | CentOS 6 with Redhat Developer Toolset  | R version 3.5.2 (2018-12-20)                                           | R from EPEL                                                       | x86_64-redhat-linux-gnu (64-bit)   | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | CentOS 6                                |                                                                        | stock R from EPEL                                                 |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Fedora Linux                            | R Under development (unstable) (2020-01-03 r77629)                     | R-devel GCC                                                       | x86_64-pc-linux-gnu (64-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Fedora Linux                            | R Under development (unstable) (2020-01-03 r77629)                     | R-devel clang gfortran                                            | x86_64-pc-linux-gnu (64-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            |                                                                        | R-release GCC                                                     |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            |                                                                        | R-patched GCC                                                     |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            | R Under development (unstable) (2020-01-03 r77629)                     | R-devel GCC no long double                                        |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            | R Under development (unstable) (2020-01-03 r77629)                     | R-devel GCC                                                       |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | RHub       | Linux   | Debian Linux                            | R Under development (unstable) (2019-08-18 r77026)                     | R-devel clang ISO-8859-15 locale                                  |                                    | PREPERROR |
| tidycells | 0.2.2   | 2020-01-06  | AppVeyor   | Windows | Windows Server 2012 R2 x64 (build 9600) | R version 3.6.2 (2019-12-12)                                           | R_VERSION=release, R_ARCH=x64                                     | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | AppVeyor   | Windows | Windows Server 2012 R2 x64 (build 9600) | R Under development (unstable) (2020-01-03 r77629)                     | R_VERSION=devel                                                   | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | AppVeyor   | Windows | Windows Server 2012 R2 x64 (build 9600) | R version 3.6.2 Patched (2020-01-03 r77629)                            | R_VERSION=patched                                                 | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | AppVeyor   | Windows | Windows Server 2012 R2 x64 (build 9600) | R version 3.5.3 (2019-03-11)                                           | R_VERSION=oldrel, RTOOLS_VERSION=33, CRAN=http://cran.rstudio.com | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Travis     | linux   | Ubuntu 16.04.6 LTS                      | R version 3.5.3 (2017-01-27)                                           | R: oldrel                                                         | x86_64-pc-linux-gnu (64-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Travis     | linux   | Ubuntu 16.04.6 LTS                      | R version 3.6.1 (2017-01-27)                                           | R: release                                                        | x86_64-pc-linux-gnu (64-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Travis     | osx     | macOS High Sierra 10.13.6               | R version 3.6.2 (2019-12-12)                                           | R: release                                                        | x86_64-apple-darwin15.6.0 (64-bit) | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Travis     | linux   | Ubuntu 16.04.6 LTS                      | R Under development (unstable) (2020-01-03 r77628)                     | R: devel                                                          | x86_64-pc-linux-gnu (64-bit)       | OK        |
| tidycells | 0.2.2   | 2020-01-06  | WinBuilder | Windows |                                         | R version 3.5.3 (2019-03-11)                                           |                                                                   | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | WinBuilder | Windows |                                         | R version 3.6.2 (2019-12-12)                                           |                                                                   | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | WinBuilder | Windows |                                         | R Under development (unstable) (2020-01-03 r77629)                     |                                                                   | x86_64-w64-mingw32 (64-bit)        | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Local      | Windows | Windows 10 x64 (build 17134)            | R version 3.6.1 (2019-07-05)                                           |                                                                   | x86_64-w64-mingw32/x64 (64-bit)    | OK        |
| tidycells | 0.2.2   | 2020-01-06  | Local      | Windows | Windows 10 x64 (build 17134)            | R version 3.6.2 (2019-12-12)                                           |                                                                   | x86_64-w64-mingw32/x64 (64-bit)    | OK        |


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
