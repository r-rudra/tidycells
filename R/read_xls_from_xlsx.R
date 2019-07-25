
read_xls_from_xlsx <- function(fn) {
  if (!rlang::is_installed("xlsx")) {
    abort("'xlsx' package is required")
  }

  # local functions
  get_date <- function(x) {
    if (identical(x, round(x))) {
      xo <- as.Date(x - 25569, origin = "1970-01-01")
    } else {
      xo <- as.POSIXct((x - 25569) * 86400,
        tz = "GMT",
        origin = "1970-01-01"
      )
    }
    xo
  }

  for_a_sheet <- function(sheet) {
    rows <- xlsx::getRows(sheet)
    if (length(rows) == 0) {
      return(NULL)
    } # exit early

    cells <- xlsx::getCells(rows)
    res <- cells %>% map(xlsx::getCellValue)

    dat <- names(res) %>%
      stringr::str_split("\\.") %>%
      map(as.integer) %>%
      reduce(rbind) %>%
      as_tibble(.name_repair = "minimal")

    colnames(dat) <- c("row", "col")

    dat <- dat %>% mutate(raw_value = res)
    # credit goes directly to xlsx-R-Package creators
    dateUtil <- date_utils()


    dat <- dat %>% mutate(is_num = raw_value %>% map_lgl(is.numeric))

    dat_n <- dat %>%
      filter(is_num)

    dat_c <- dat %>%
      filter(!is_num) %>%
      mutate(data_type = "chr")

    if (nrow(dat_c) > 0) {
      dat_c <- dat_c %>%
        mutate(chr = raw_value %>% map_chr(1))
    }

    num_cells <- cells[dat$is_num]

    if (length(num_cells) > 0) {
      # possible dates
      is_date <- num_cells %>% map_lgl(dateUtil$isCellDateFormatted)

      dat_n <- dat_n %>%
        mutate(is_date = is_date)

      dat_n_nd <- dat_n %>%
        filter(!is_date) %>%
        mutate(data_type = "dbl")

      if (nrow(dat_n_nd) > 0) {
        dat_n_nd <- dat_n_nd %>%
          mutate(dbl = raw_value %>% map_dbl(1))
      }


      dat_n_d <- dat_n %>%
        filter(is_date)

      if (nrow(dat_n_d) > 0) {
        dat_n_d <- dat_n_d %>%
          mutate(date_raw = raw_value %>% map(get_date))

        dat_n_d <- dat_n_d %>%
          mutate(data_type = ifelse(
            date_raw %>% map_lgl(~ inherits(.x, "POSIXct")),
            yes = "dttm",
            no = "date"
          ))

        dat_n_d_dt <- dat_n_d %>%
          filter(data_type == "date")

        if (nrow(dat_n_d_dt) > 0) {
          dat_n_d_dt <- dat_n_d_dt %>%
            mutate(date = date_raw %>% map(1) %>% reduce(c))
        }

        dat_n_d_dttm <- dat_n_d %>%
          filter(data_type == "dttm")

        if (nrow(dat_n_d_dttm) > 0) {
          dat_n_d_dttm <- dat_n_d_dttm %>%
            mutate(dttm = date_raw %>% map(1) %>% reduce(c))
        }


        dat_n_d <- dat_n_d_dt %>% bind_rows(dat_n_d_dttm)
      }

      dat_n <- dat_n_d %>% bind_rows(dat_n_nd)
    }

    dat <- dat_c %>% bind_rows(dat_n)

    dat <- dat %>% arrange(row, col)
    dat[c("row", "col", "data_type", sort(unique(dat$data_type)))]
  }

  read_xls_for_tidycells <- function(filename) {
    wb <- xlsx::loadWorkbook(filename)
    sheets <- xlsx::getSheets(wb)

    sheets %>% map(for_a_sheet)
  }

  date_utils <- function() {
    # credit goes directly to xlsx-R-Package creators
    .jnew("org/apache/poi/ss/usermodel/DateUtil")
  }

  # put them in xlsx environment
  environment(date_utils) <- environment(xlsx::read.xlsx)

  suppressMessages(
    suppressWarnings(
      read_xls_for_tidycells(fn)
    )
  )
}
