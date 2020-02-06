#' Inexact dates
#'
#' Dates sometimes suck
#'
#' @param year Year
#' @param month Month
#'
#' @importFrom dplyr case_when
#'
#' @export
#' @examples
#' earliest_date(2019, 1, 0)

earliest_date <- function(year, month = NULL, day = NULL) {
  if(is.na(year)) return(NA)

  x <- sapply(list(month, day),
              function(x) {
                ifelse(is.null(x) || x <= 0,
                       "01",
                       formatC(x, width = 2, flag = 0))
              })

  as.Date(paste(c(year, unlist(x)), collapse = "-"))
}

# earliest_date(2000)
# earliest_date(2019, NULL, NULL)
# earliest_date(2019, NULL, 2)
# earliest_date(2019, NULL, 31)

latest_date <- function(year, month = NULL, day = NULL) {
  if(is.na(year)) return(NA)
  month <- ifelse(is.null(month) || month <= 0, 12, month)
  if(is.null(month) || month <= 0) {
    day <- 31
  } else if (is.null(day) || day <= 0) {
    day <- get_days_in_month(as.numeric(year))[month]
  }
  as.Date(paste(year, month, day, sep = "-"))
}

# latest_date(2019)
# latest_date(2019, NULL, NULL)
# latest_date(2019, 0)
# latest_date(2019, NULL, 2)
# latest_date(2019, 2)
# latest_date(2016, 2)

unknown_date <- function(x, format = "ymd", possible = c("earliest", "latest")) {
  possible <- match.arg(possible)
  x <- unlist(strsplit(x, "[^[:alnum:]]"))
  x[grepl("UNK?", x, ignore.case = TRUE)] <- 0
  # format = "dmy"
  x <- x[c("y" = 1, "m" = 2, "d" = 3)[unlist(strsplit(format, ""))]]
  switch(possible,
         earliest = earliest_date(year = x[1], month = x[2], day = x[3]),
         latest = latest_date(year = x[1], month = x[2], day = x[3]))
}

# x <- "UN UNK 2019"
# unknown_date(x)
# unknown_date(x, format = "dmy")
# unknown_date(x, format = "dmy", possible = "l")

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
