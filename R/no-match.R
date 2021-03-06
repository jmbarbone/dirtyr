#' Additional value matching
#'
#' Non matching alternatives and supplementary functions.
#'   Contrast with [base::match()], [base::intersect()], and [base::`%in%`()]
#'
#' @inheritParams base::`%in%`
#' @export
#' @examples
#' 1:10 %in% c(1,3,5,9)
#' 1:10 %out% c(1,3,5,9)
#'
#' # intersect() and setdiff() are similiar but don't keep duplicates
#' x <- c(1:6, 7:2)
#' y <- c(3, 7, 12)
#'
#' x %wo% y      # --> keeps duplicated
#' setdiff(x, y) # --> unique values
#'
#' x %wi% y        # --> keeps duplicated
#' intersect(x, y) # --> unique values

`%out%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

#' @rdname grapes-out-grapes
#' @export
`%wo%` <- function(x, table) {
  x[x %out% table]
}

#' @rdname grapes-out-grapes
#' @export
`%wi%` <- function(x, table) {
  x[match(x, table, nomatch = 0L)]
}

#' @rdname grapes-out-grapes
#' @export
no_match <- function(x, table) {
  all_na(match(x, table, nomatch = NA_integer_, incomparables = NULL))
}
