#' Inexact dates
#'
#' Incomplete dates cannot be directly coerced into date values in R.
#' These functions help
#'
#' @details
#' `unknown_date` is a vectorized function which implements the non-vectorized `earliest_date` and `latest_date`.
#' If a split vector of 2 is passed to `unkown_date`, it is assumed that the vector consists of the Month and Year.
#' If a split vector of 1 is passed to `uknown_date`, it is assumed that the vector consists only of the Year.
#' For cases in which the year is not provided or the month is unknown, it is suggested that these be recoded before using this function.
#'
#' @param year Year
#' @param month Month
#' @param day Day
#' @param x a date "string" (see details)
#' @param format A date "format" (see details)
#' @param possible Whether to look at earliest or latest
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


#' @export
#' @rdname possible_date
earliest_date <- function(year, month = NULL, day = NULL) {
  if(is.na(year)) return(NA_date_)

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
  if(is.na(year)) return(NA_date_)
  month <- ifelse(is.null(month) || month <= 0 || is.na(month), 12, month)
  if(is.null(month) || month <= 0) {
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
  res <- lapply(x, extract_date, format, possible)
  do.call("c", res)
}

extract_date <- function(x, format, possible) {
  if(is.na(x)) return(NA_date_)
  x <- unlist(strsplit(x, "[^[:alnum:]]"))
  x[grepl("^UNK?|NA$", x, ignore.case = TRUE)] <- 0
  form <- c("y" = 1, "m" = 2, "d" = 3)[unlist(strsplit(format, ""))]

  v <- switch(length(x),
              `1` = insert(c(0, 0, 0), x, which(names(form) == "y"))[form],
              `2` = insert(x, 0, which(names(form) == "d"))[form],
              `3` = x[form])
  v[is.na(v)] <- 0
  if(suppress_wm(is.na(as.numeric(v[2])))) {
    v[2] <- which_month(v[2])
  }
  v <- as.numeric(v)
  switch(possible,
         earliest = earliest_date(year = v[1], month = v[2], day = v[3]),
         latest = latest_date(year = v[1], month = v[2], day = v[3]))
}


# Utils -------------------------------------------------------------------

days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
names(days_in_month) <- month.abb

is_leap <- function(year = NULL) {
  # if(is.null(year)) return(FALSE)
  if(year %% 4 != 0) {
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
  if(is_leap(year)) days_in_month['Feb'] <- 29
  days_in_month
}

which_month <- function(month_abbreviation) {
  x <- which(tolower(month.abb) == tolower(month_abbreviation))
  if(length(x) == 0) return(NA)
  x
}

NA_date_ <- as.Date(NA)
