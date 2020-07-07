#' Inexact dates
#'
#' Incomplete dates cannot be directly coerced into date values in R.
#' These functions help
#'
#' @details
#' `unknown_date` is a vectorized function which implements the non-vectorized
#'   `earliest_date` and `latest_date`.
#' If a split vector of 2 is passed to `unknown_date`, it is assumed that the
#'   vector consists of the Month and Year.
#' If a split vector of 1 is passed to `unknown_date`, it is assumed that the
#'   vector consists only of the Year.
#' For cases in which the year is not provided or the month is unknown, it is
#'   suggested that these be recoded before using this function.
#'
#' @param year Year
#' @param month Month
#' @param day Day
#' @param x a date "string" (see details)
#' @param format A date "format" (see details)
#' @param possible Whether to look at earliest or latest
#'
#' @importFrom stats setNames
#'
#' @name possible_date
#' @export
#' @examples
#' earliest_date(2019, 1, 0)
#' earliest_date(2000)
#' earliest_date(2019, NULL, NULL)
#' earliest_date(2019, NULL, 2)
#' earliest_date(2019, NULL, 31)
#'
#' latest_date(2019)
#' latest_date(2019, NULL, NULL)
#' latest_date(2019, 0)
#' latest_date(2019, NULL, 2)
#' latest_date(2019, 2)
#' latest_date(2016, 2)
#'
#' x <- "UN UNK 2019"
#' unknown_date(x)
#' unknown_date(x, format = "dmy")
#' unknown_date(x, format = "dmy", possible = "l")
#'
#' unknown_date(c("01 JAN 1996", "Feb 2010", "2019"), "dmy")
#' unknown_date(c("01 JAN 1996", "Feb 2016"), "dmy", "latest")
#' unknown_date("2015", possible = "e")
#' unknown_date("2015", possible = "l")


#' @export
#' @rdname possible_date
earliest_date <- function(year, month = NULL, day = NULL) {
  if (is.na(year)) {
    return(NA_date_)
  }

  x <- sapply(list(month, day),
              function(x) {
                ifelse(is.null(x) || x <= 0 || is.na(x),
                       "01",
                       formatC(x, width = 2, flag = 0))
              })

  as.Date(paste(c(year, unlist(x)), collapse = "-"))
}

#' @export
#' @rdname possible_date
latest_date <- function(year, month = NULL, day = NULL) {
  if (is.na(year)) {
    return(NA_date_)
  }
  month <- ifelse(is.null(month) || month <= 0 || is.na(month), 12, month)
  if (is.null(month) || month <= 0) {
    day <- 31
  } else if (is.null(day) || day <= 0) {
    day <- get_days_in_month(as.numeric(year))[month]
  }
  as.Date(paste(year, month, day, sep = "-"))
}

#' @export
#' @rdname possible_date
unknown_date <- function(x, format = "ymd", possible = c("earliest", "latest")) {
  possible <- match.arg(possible)
  if (anyNA(match(unlist(strsplit(format, "")), c("y", "m", "d")))) {
    stop("Format not assigned correctly: should use 'y', 'm', and 'd'.", call. = FALSE)
  }
  res <- lapply(x, extract_date, format, possible)
  do.call("c", res)
}

extract_date <- function(x, format, possible) {
  if (is.na(x)) {
    return(NA_date_)
  }
  x <- unlist(strsplit(x, "[^[:alnum:]]"))
  x[grepl("^UNK?|NA$", x, ignore.case = TRUE)] <- 0
  form <- c("y" = 1, "m" = 2, "d" = 3)[unlist(strsplit(format, ""))]

  v <- switch(length(x),
              `1` = insert(c(0, 0, 0), x, which(names(form) == "y"))[form],
              `2` = insert(x, 0, which(names(form) == "d"))[form],
              `3` = x[form])
  v[is.na(v)] <- 0
  if (suppress_wm(is.na(as.numeric(v[2])))) {
    v[2] <- which_month(v[2])
  }
  v <- as.numeric(v)
  switch(possible,
         earliest = earliest_date(year = v[1], month = v[2], day = v[3]),
         latest = latest_date(year = v[1], month = v[2], day = v[3]))
}

#' Parses dates
#'
#' Separates dates from a vector or a data.frame
#'
#' @param x A vector or data.frame
#' @param year Name for year column or column suffix
#' @param month Name for month column or column suffix
#' @param day Name for day column or column suffix
#' @param dates_to_row Logical, if `TRUE`, adds dates to the row names
#' @param cols A character vector of the columns to parse into dates
#' @param sep String to use to separate new columns
#' @param keep Logical, if `TRUE` the original date column is kept
#'
#' @examples
#' x <- c("2010-01-12", "2020-09-30", "1999-12-31")
#' split_date(as.Date(x))
#'
#' xx <- data.frame(
#'   x1 = 1:3,
#'   x2 = runif(3),
#'   date1 = as.Date(c("1950-10-05", "2020-04-29", "1992-12-17")),
#'   x3 = letters[1:3],
#'   date2 = as.Date(c("2010-01-12", "2020-09-30", "1999-12-31")))
#' parse_date(xx, c("date1", "date2"))
#' @export
split_date <- function(x, year = "year", month = "month", day = "day",
                       dates_to_row = FALSE) {
  stopifnot(class(x) == "Date")
  x %>%
    as.character() %>%
    sapply(strsplit, split = "-", fixed = TRUE, simplify = TRUE) %>%
    lapply(as.integer) %>%
    Reduce(rbind, .) %>%
    as.data.frame(row.names = if(dates_to_row) x else FALSE,
                  stringsAsFactors = FALSE) %>%
    setNames(c(year, month, day))
}

#' @export
#' @rdname split_date
parse_date <- function(x, cols, year = "year", month = "month", day = "day",
                       sep = "_", keep = FALSE) {
  stopifnot(is.data.frame(x) && all(cols %in% colnames(x)))
  for (i in cols) {
    if (class(x[[i]]) != "Date") {
      warning("Column `i` is not a Date -- skipped", call. = FALSE)
      next
    }
    cn <- colnames(x)
    place <- which(cn == i) - !keep
    after <- colnames(x)[-seq(place + !keep)]
    x <- cbind(x[, seq(place), drop = FALSE],
               split_date(x[[i]],
                          year = sprintf("%s%s%s", i, sep, year),
                          month = sprintf("%s%s%s", i, sep, month),
                          day = sprintf("%s%s%s", i, sep, day),
                          dates_to_row = FALSE),
               x[, after, drop = FALSE])
  }
  x
}


# Utils -------------------------------------------------------------------

days_in_month <- setNames(
  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
  month.abb)

is_leap <- function(year = NULL) {
  if (year %% 4 != 0) {
    FALSE
  } else if (year %% 100 != 0) {
    TRUE
  } else if (year %% 400 != 0) {
    FALSE
  } else {
    TRUE
  }
}

get_days_in_month <- function(year = NULL) {
  if (is_leap(year)) {
    days_in_month['Feb'] <- 29
  }
  days_in_month
}

which_month <- function(month_abbreviation) {
  x <- which(tolower(month.abb) == tolower(month_abbreviation))
  if (length(x) == 0) {
    return(NA)
  }
  x
}

NA_date_ <- as.Date(NA)
