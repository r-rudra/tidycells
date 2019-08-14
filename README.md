# **tidycells** <img src="vignettes/ext/logo.png" align="right" width="200"/>

#### *Read Tabular Data from Diverse Sources and Easily Make Them Tidy*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidycells)](https://cran.r-project.org/package=tidycells)
[![Travis build
status](https://travis-ci.org/r-rudra/tidycells.svg?branch=master)](https://travis-ci.org/r-rudra/tidycells)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/r-rudra/tidycells?branch=master&svg=true)](https://ci.appveyor.com/project/bedantaguru/tidycells)
[![Codecov Coverage
Status](https://codecov.io/gh/r-rudra/tidycells/branch/master/graph/badge.svg)](https://codecov.io/gh/r-rudra/tidycells?branch=master)
[![Coveralls Coverage
Status](https://coveralls.io/repos/github/r-rudra/tidycells/badge.svg?branch=master)](https://coveralls.io/github/r-rudra/tidycells?branch=master)
[![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://raw.githubusercontent.com/r-rudra/tidycells/master/LICENSE.md)
[![See
DevNotes](https://img.shields.io/badge/See-DevNotes-blue.svg)](https://github.com/r-rudra/tidycells/blob/master/dev-notes.md)

<!-- badges: end -->

## Author

Indranil Gayen

## TL;DR

Given a `file_name` which is a path of a file that contains table(s).
Run this `read_cells()` in the R-console to see whether support is
present for the file type. If support is present, just run

``` r
read_cells(file_name)
```

**Note** Just start with a small file.

## Introduction

The package provides utilities to read, cells from complex tabular data
and heuristic detection based ‘structural assignment’ of those cells to
a columnar or tidy format.

Read functionality has the ability to read (in a unified manner)
structured, partially structured or unstructured tabular data (usually
spreadsheets for public data dissemination and aimed for common human
understanding) from various types of documents. The tabular information
is read as cells. The ‘structure assignment’ functionality has both
supervised and unsupervised way of assigning cells data to columnar/tidy
format. Multiple disconnected blocks of tables in a single sheet are
also handled appropriately.

These tools are suitable for unattended conversation of (maybe a pile
of) messy tables (like government data) into a consumable format(usable
for further analysis and data wrangling).

## Installation

To install the development version from GitHub you’ll need `devtools`
package in R. Assuming you have `devtools` you can install this package
in R with the following command:

``` r
devtools::install_github("r-rudra/tidycells")
```

To start with `tidycells`, I invite you to see
`vignette("tidycells-intro")` (*to see vignette you need to install the
package with vignette. That can be done in above command by specifying
`build_vignettes = TRUE`. Note that is might be time consuming*) or
check out [tidycells-website](https://r-rudra.github.io/tidycells/).

## Quick Overview

Let’s take a quick look at an example data as given
in

``` r
system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE)
```

The data looks like (in excel)

<img src="vignettes/ext/marks.png" width="451px" />

Let’s try `tidycells` functions in this data

Read at
once

``` r
system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
  read_cells()
```

| collated\_1 | collated\_2 | collated\_3 | collated\_4        | table\_tag | value |
| :---------- | :---------- | :---------- | :----------------- | :--------- | :---- |
| Male        | School A    | Score       | Utsyo Roy          | Sheet1     | 95    |
| Male        | School A    | Score       | Nakshatra Gayen    | Sheet1     | 99    |
| Female      | School A    | Score       | Titas Gupta        | Sheet1     | 89    |
| Female      | School A    | Score       | Ujjaini Gayen      | Sheet1     | 100   |
| Male        | School B    | Score       | Indranil Gayen     | Sheet1     | 70    |
| Male        | School B    | Score       | S Gayen            | Sheet1     | 75    |
| Female      | School B    | Score       | Sarmistha Senapati | Sheet1     | 81    |
| Female      | School B    | Score       | Shtuti Roy         | Sheet1     | 90    |
| Male        | School C    | Name        | I Roy              | Sheet1     | 50    |
| Male        | School C    | Name        | S Ghosh            | Sheet1     | 59    |
| Female      | School C    | Name        | S Senapati         | Sheet1     | 61    |
| Female      | School C    | Name        | U Gupta            | Sheet1     | 38    |

The function `read_cells` is a set of ordered operations connected
together. The flowchart of
`read_cells`:

<img src="vignettes/ext/read_cells.svg" width="356px" style="display: block; margin: auto;" />

Let’s understand step by step procedures followed by `read_cells`.

``` r
# if you have tidyxl installed
d <- system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
  read_cells(at_level = "make_cells") %>% 
  .[[1]]
```

``` r
# or you may do
d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
```

``` r
d <- numeric_values_classifier(d)
da <- analyze_cells(d)
```

Then you need to run `compose_cells` with additional new argument
`print_attribute_overview =
TRUE`

``` r
dc <- compose_cells(da, print_attribute_overview = TRUE)
```

<img src="vignettes/ext/compose_cells_cli1.png" width="451px" />

``` r
# bit tricky and tedious unless you do print_attribute_overview = TRUE in above line
dcfine <- dc %>% 
  dplyr::mutate(name = dplyr::case_when(
    data_block == 1 ~ major_row_left_2_1,
    data_block == 2 ~ major_col_bottom_1_1,
    data_block == 3 ~ major_row_left_1_1
  ),
  sex = dplyr::case_when(
    data_block == 1 ~ major_row_left_1_1,
    data_block == 2 ~ major_col_bottom_2_1,
    data_block == 3 ~ minor_row_right_1_1
  ),
  school = dplyr::case_when(
    data_block == 1 ~ minor_col_top_1_1,
    data_block == 2 ~ minor_corner_topLeft_1_1,
    data_block == 3 ~ minor_col_top_1_1
  )) %>% 
  dplyr::select(school,sex, name, value)
```

`head(dcfine)` looks like

| school   | sex    | name            | value |
| :------- | :----- | :-------------- | :---: |
| School A | Male   | Utsyo Roy       |  95   |
| School A | Male   | Nakshatra Gayen |  99   |
| School A | Female | Titas Gupta     |  89   |
| School A | Female | Ujjaini Gayen   |  100  |
| School B | Male   | Indranil Gayen  |  70   |
| School B | Male   | S Gayen         |  75   |

This is still not good right\! You had to manually pick some weird
column-names and spent some brain (when it was evident from data which
columns should be aligned with whom).

The `collate_columns` functions does exactly this for you. So instead of
manually picking column-names after compose cells you can simply run

``` r
# collate_columns(dc) should be same with 
# direct read_cells() result except table_tag column
collate_columns(dc) %>% 
  head()
```

| collated\_1 | collated\_2 | collated\_3 | collated\_4     | value |
| :---------- | :---------- | :---------- | :-------------- | :---: |
| Male        | School A    | Score       | Utsyo Roy       |  95   |
| Male        | School A    | Score       | Nakshatra Gayen |  99   |
| Female      | School A    | Score       | Titas Gupta     |  89   |
| Female      | School A    | Score       | Ujjaini Gayen   |  100  |
| Male        | School B    | Score       | Indranil Gayen  |  70   |
| Male        | School B    | Score       | S Gayen         |  75   |

Looks like staged example\! Yes you are right this is not always perfect
(same is true for `analyze_cells` also). However, if the data is somehow
helpful in demystifying underlying columns structure (like this one),
then this will be useful.

These functions `read_cells` (all functionalities combined),
`analyze_cells`, `collate_columns` are here to ease your pain in data
wrangling and reading from various sources. It may not be full-proof
solution to all types of tabular data. It is always recommended to
perform these tasks manually whenever expected results are not coming.

### Plots and Interactive Modules

The package provides `ggplot` based plots and `shiny` based interactive
visualisations for understanding how the heuristic is functioning and
also provides object (like `cell-df` or `cell-analysis`) editing
capabilities.

The [shiny](https://shiny.rstudio.com/) package is required for
interactive modules. Most of the features are self-explanatory and
guided.

Check out interactive documentation of any of these functions listed
below. All of these functions are available as [RStudio
Addins](https://rstudio.github.io/rstudioaddins/).

Here are screenshots of each interactive widgets.

1.  Plot tune (part of all modules)
2.  `visual_crop()` for data crop and deletion of sections

<img src="vignettes/ext/v12.png" width="516px" />

3.  `visual_va_classify()` for interactive VA classification
4.  `visual_data_block_inspection()` this shows how the heuristic has
    performed the analysis after `analyze_cells`

<img src="vignettes/ext/v34.png" width="516px" />

5.  `visual_orientation_modification()` for modification to heuristic
    based results
6.  `visual_traceback()` this is for observing how the original data is
    composed to form the final output. (`compose_cells` is called
    internally)

<img src="vignettes/ext/v56.png" width="516px" />

For each of these modules, there is a dynamic plot option available from
[plotly](https://github.com/ropensci/plotly). If you have that package,
the corresponding tab will be activated. Since all of these modules are
entirely optional the dependency is kept at **tidycells** ‘suggests’
level only.

## Reference and Related Projects

  - [tidyxl](https://github.com/nacnudus/tidyxl) : **Read Untidy Excel
    Files:** Imports non-tabular from Excel files into R. Exposes cell
    content, position and formatting in a tidy structure for further
    manipulation. Tokenizes Excel formulas. Supports ‘.xlsx’ and ‘.xlsm’
    via the embedded ‘RapidXML’ C++ library
    <http://rapidxml.sourceforge.net>. Does not support ‘.xlsb’ or
    ‘.xls’.
  - [unpivotr](https://github.com/nacnudus/unpivotr): **Unpivot Complex
    and Irregular Data Layouts** Tools for converting data from complex
    or irregular layouts to a columnar structure. For example, tables
    with multilevel column or row headers, or spreadsheets. Header and
    data cells are selected by their contents and position, as well as
    formatting and comments where available, and are associated with one
    other by their proximity in given directions. Functions for data
    frames and HTML tables are provided. Major parts of the package
    right now fully depend on **unpivotr**. The **tidycells** package
    would have never existed without this wonderful package from [Duncan
    Garmonsway](https://github.com/nacnudus).
  - The [rsheets](https://github.com/rsheets) project: It hosts several
    R packages (few of them are in CRAN already) which are in the early
    stages of importing spreadsheets from Excel and Google Sheets into
    R. Specifically, have a look at these projects which seems closely
    related to these projects :
    [jailbreaker](https://github.com/rsheets/jailbreakr),
    [rexcel](https://github.com/rsheets/rexcel) (README of this project
    has a wonderful reference for excel integration with R).
  - The [tidyABS](https://github.com/ianmoran11/tidyABS) package: The
    `tidyABS` package converts ABS excel tables to tidy data frames. It
    uses rules-of-thumb to determine the structure of excel tables,
    however it sometimes requires pointers from the user. This package
    is in early development.

## Acknowledgement

This package incomplete without following packages (apart from the
**unpivotr** which is the core package on which **tidycells** depends
largely, as mentioned above ). Each of these packages are in suggests
fields of `tidycells`. (The read\_cells basically, performs unification
on several functions from various packages to give you support for
different file types. These are listed below.)

  - [**readr**](https://cran.r-project.org/package=readr): for csv (in
    melted format)
  - [**readxl**](https://cran.r-project.org/package=readxl): for reading
    xls (if xlsx is present by default xlsx will be used for xls)
  - [**xlsx**](https://cran.r-project.org/package=xlsx): for reading xls
    (also it has capabilities to read xlsx)
  - [**tidyxl**](https://cran.r-project.org/package=tidyxl): really fast
    library for reading xlsx
  - [**docxtractr**](https://cran.r-project.org/package=docxtractr) :
    for docx and doc (it has a system level dependency now)
  - [**tabulizer**](https://cran.r-project.org/package=tabulizer) : for
    pdf
  - [**XML**](https://cran.r-project.org/package=XML) : for html/xml
    type files
  - [**stringdist**](https://CRAN.R-project.org/package=stringdist) :
    for enhanced string matching in `tidycells::collate_columns`
