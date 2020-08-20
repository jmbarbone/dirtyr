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
#' @param x a date "string" (see details)
#' @param format A date "format" (see details)
#' @param possible Whether to look at earliest or latest
#' @param ... Additional arguments passed to `extract_date()`
#' @param year Year
#' @param month Month
#' @param day Day
#'   to be removed from `x`
#'
#' @importFrom stats setNames
#'
#' @name possible_date
#' @export
#' @examples
#' earliest_date(2019, 1, 0)
#' earliest_date(2000)
#' earliest_date(2019, NA, NA)
#' earliest_date(2019, NA, 2)
#' earliest_date(2019, NA, 31)
#'
#' latest_date(2019)
#' latest_date(2019, NA, NA)
#' latest_date(2019, 0)
#' latest_date(2019, NA, 2)
#' latest_date(2019, 2)
#' latest_date(2016, 2)
#'
#' x <- "UN UNK 2019"
#' unknown_date(x) # NA_date_
#'
#' unknown_date(x, format = "dmy")
#' unknown_date(x, format = "dmy", possible = "l")
#'
#' unknown_date(c("01 JAN 1996", "Feb 2010", "2019"), "dmy")
#' unknown_date(c("01 JAN 1996", "Feb 2016"), "dmy", "latest")
#' unknown_date("2015", possible = "e")
#' unknown_date("2015", possible = "l")

#' @export
#' @rdname possible_date
unknown_date <- function(x, format = "ymd", possible = c("earliest", "latest"), ...) {
  possible <- match.arg(possible)

  form <- strsplit(format, "")[[1]]

  if (anyNA(match(form, c("y", "m", "d")))) {
    stop("Format not assigned correctly: should use 'y', 'm', and 'd'.",
         call. = FALSE)
  }

  extract_date(x,
               form = form,
               possible = possible,
               ...)
}

extract_date <- function(x, form = NULL, possible, invalid_date_string = "^UNK?|NA$", format) {

  # x <- c("2019-01-01", "2019-02-28", "3 UNK 2019", "UN JUN 2004", NA, "Feb 2000")
  # format <- "ymd"
  # possible <- "earliest"
  # invalid_date_string <- "^UNK?|NA$"
  # form <- strsplit(format, "")[[1]]

  if (is.null(form) && !missing(format)) {
    form <- strsplit(format, "")[[1]]
  }

  if (length(form) == 1L) form <- strsplit(form, "")[[1]]


  # Split each string
  splits <- strsplit(x, "[^[:alnum:]]")
  form_order <- c("y" = 1, "m" = 2, "d" = 3)[form]

  names(splits) <- x

  # creates list
  mat <- vapply(splits, function(xx) {
    xx[grepl(invalid_date_string, xx, ignore.case = TRUE)] <- NA_character_
    xx[gsub("[[:space:]]+|[[:punct:]]+", "", xx) == ""] <- NA_character_

    al <- grep("[[:alpha:]]", xx)
    if (length(al)) {
      xx[al] <- which_month(xx[al])
    }
    # Make proper inserts
    foo <- form_switch_selector(xx)
    foo(xx, form_order)
  }, character(3), USE.NAMES = TRUE)

  class(mat) <- "integer"

  res <- switch(
    possible,
    earliest = apply(mat, 2, function(x) earliest_date_chr(x[1], x[2], x[3])),
    latest = apply(mat, 2, function(x) latest_date_chr(x[1], x[2], x[3]))
  )

  as_date_strptime(res)
}

form_switch_selector <- function(x) {
  ln <- length(x)

  if (ln == 0L) {
    # Show all NA if no length
    function(...) rep(NA_character_, 3)
  } else if (ln < 3L) {
    # For 1 or 2 decide on inserts
    switch(
      ln,
      `1` = function(x, form) {
        # stopifnot(!is.null(names(form)))
        insert(c(NA_character_, NA_character_, NA_character_),
               x,
               which(names(form) == "y"))[form]
      },
      `2` = function(x, form){
        # stopifnot(!is.null(names(form)))
        insert(x, NA_character_, which(names(form) == "d"))[form]
      }
    )
  } else {
    # Default values
    # Numeric EXPR does not allow a default value in switch()
    function(x, form) x[form]
  }
}

#' @export
#' @rdname possible_date
earliest_date <- function(year, month = NA, day = NA) {
  as_date_strptime(earliest_date_chr(year, month, day))
}

earliest_date_chr <- function(year, month = NA, day = NA) {
  if (is.na(year)) {
    return(NA_character_)
  }

  vec <- c(year, month, day)
  out <- as.character(vec)
  out[2][vec[2] > 12] <- "12L"
  max_days <- get_days_in_month(vec[1])[vec[2]]
  out[3][vec[3] > max_days] <- as.character(max_days)
  out[2:3][is.na(vec[2:3]) | vec[2:3] <= 0] <- "01"
  paste(out, collapse = "-")
}


#' @export
#' @rdname possible_date
latest_date <- function(year, month = NA, day = NA) {
  as_date_strptime(latest_date_chr(year, month, day))
}

latest_date_chr <- function(year, month = NA, day = NA) {
  if (is.na(year)) {
    return(NA_character_)
  }

  year <- as.integer(year)

  if (is.na(month) |  month >= 12L | month == 0L) {
    month <- 12L
  } else if (month < 0L) {
    month <- 1L
  }

  if (is.na(day) | day <= 0) {
    day <- get_days_in_month(year)[[month]]
  }

  paste(year, month, day, sep = "-")
}

#' Splits/parses dates
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
    as.data.frame(row.names = if (dates_to_row) x else FALSE,
                  stringsAsFactors = FALSE) %>%
    setNames(c(year, month, day))
}

#' @export
#' @rdname split_date
parse_date <- function(x, cols, year = "year", month = "month", day = "day",
                       sep = "_", keep = FALSE) {
  stopifnot(is.data.frame(x) && all(cols %in% colnames(x)))

  for (i in cols) {
    if (!inherits(x, "Date")) {
      warning(paste0("Column `", i, "` is not a Date -- skipped"),
              call. = FALSE)
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
  as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
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
    days_in_month['Feb'] <- 29L
  }
  days_in_month
}

month_abb <- tolower(month.abb)

which_month <- function(month_abbreviation) {
  x <- which(month_abb == tolower(month_abbreviation))

  if (length(x) == 0) {
    return(NA_integer_)
  }

  x
}

NA_date_ <- as.Date(NA)

# To be set on package load
options(dirtyr.tz = Sys.timezone())

as_date_strptime <- function(x) {
  as.Date(strptime(x,
                   format = "%Y-%m-%d",
                   tz = getOption("dirtyr.tz")),
          format = "%Y-%m-%d")
}
