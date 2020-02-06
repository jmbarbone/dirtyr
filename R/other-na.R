#' Additional na functions
#'
#' Additional functions to sort for NA values
#'
#' @param x a vector of values
#' @export
#' @name nas

#' @export
#' @rdname nas
is_na_inf <- function(x) {
  is.na(x) | is.infinite(x)
}

#' @export
#' @rdname nas
count_na <- function(x) {
  sum(is.na(x))
}

#' @export
#' @rdname nas
count_na_vars <- function(df) {
  vapply(df, count_na, integer(1), USE.NAMES = TRUE)
}

#' @export
#' @rdname nas
is_nin <- function(x) {
  vapply(x, any_nin, logical(1), USE.NAMES = T)
}

#' @export
#' @rdname nas
any_nin <- function(x) {
  is.null(x) | is.na(x) | is.infinite(x)
}

# is_nin(x)
# which_nin(x)
# any_nin(x)
# anyNA(x)
