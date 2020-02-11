#' Additional na functions
#'
#' Additional functions to sort for NA values
#'
#' @param x a vector or data.frame of values
#'
#' @export
#' @name nas

is_na_inf <- function(x) {
  is.na(x) | is.infinite(x)
}

#' @export
#' @rdname nas
count_na <- function(x) {
  UseMethod("count_na", x)
}

#' @export
count_na.default <- function(x) {
  sum(is.na(x))
}

#' @export
count_na.data.frame <- function(x) {
  vapply(df, count_na, integer(1), USE.NAMES = TRUE)
}

#' @export
#' @rdname nas
is_nin <- function(x) {
  vapply(x, is_nin, logical(1), USE.NAMES = TRUE)
}

#' @export
#' @rdname nas
any_nin <- function(x) {
  UseMethod("any_nin",)
}

#' @export
any_nin.default <- function(x) {
  is.null(x) | is.na(x) | is.infinite(x)
}

#' @export
any_nin.data.frame <- function(x) {
  vapply(x, any_nin, logical(1), USE.NAMES = TRUE)
}

# is_nin(x)
# which_nin(x)
# any_nin(x)
# anyNA(x)
