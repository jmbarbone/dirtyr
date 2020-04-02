#' Boolean Operators
#'
#' These provide better logical operators for catching TRUE and FALSE statements in the presence of
#' `NA` values.
#' These return `NA` values as `FALSE` and thus no `na.rm` argument is not necessary.
#'
#' @details
#' The functions [is_true()] and [is_false()] are vectorized and will return a logical vector of the
#'   same length as the input `x` denoting whether or not the values are `TRUE` or `FALSE`,
#'   respectively.
#' The `all_*()` and `any_*()` functions apply the vectorized functions and return values of `TRUE`
#'   or `FALSE`.
#'
#' Specialized `any_*_x()` and `all_*_x()` apply exclusivity to the evaluations.
#' These will return `NA` is all the values are `NA` to designate an incompatibility of values.
#' This is useful for when you do not want to apply an evaluation in the _absence_ of data.
#'
#' @param x A vector of logical values
#' @param ... Additional arguments sent to methods (not used)
#' @returns Either a vector of logical values or a single logical values (see details).
#'
#' @name boolean_ops
#'
#' @examples
#' x <- c(TRUE, NA, FALSE)
#' is_true(x)     ##  TRUE FALSE FALSE
#' is_false(x)    ## FALSE FALSE  TRUE
#' all_true(x)    ## FALSE
#' all_true(NA)   ## FALSE
#' all_true_x(NA) ## NA
#' any_true_x(x)  ## TRUE
#' any_true_x(NA) ## NA

#' @export
#' @rdname boolean_ops
is_true <- function(x, ...) {
  UseMethod("is_true", x)
}

#' @export
is_true.default <- function(x, ...) {
  logical_error()
}

#' @export
is_true.logical <- function(x, ...) {
  x[is.na(x)] <- FALSE
  x
}

#' @export
#' @rdname boolean_ops
is_false <- function(x, ...) {
  UseMethod("is_false", x)
}

#' @export
is_false.default <- function(x, ...) {
  logical_error()
}

#' @export
is_false.logical<- function(x, ...) {
  x[is.na(x)] <- TRUE
  !x
}

#' @export
#' @rdname boolean_ops
is_na <- function(x, ...) {
  UseMethod("is_na", x)
}

#' @export
is_na.default <- function(x, ...) {
  logical_error()
}

#' @export
is_na.logical <- function(x, ...) {
  is.na(x)
}

#' @export
#' @rdname boolean_ops
any_na <- function(x) {
  all(is.na(x))
}

#' @export
#' @rdname boolean_ops
all_na <- function(x) {
  all(is_na(x))
}

#' @export
#' @rdname boolean_ops
any_true <- function(x) {
  any(is_true(x))
}

#' @export
#' @rdname boolean_ops
any_true_x <- function(x) {
  if (all_na(x)) NA else any_true(x)
}

#' @export
#' @rdname boolean_ops
any_false <- function(x) {
  any(is_false(x))
}

#' @export
#' @rdname boolean_ops
any_false_x <- function(x) {
  if (all_na(x)) NA else any_false(x)
}

#' @export
#' @rdname boolean_ops
all_true <- function(x) {
  all(is_true(x))
}

#' @export
#' @rdname boolean_ops
all_true_x <- function(x) {
  if (all_na(x)) NA else all(is_true(x))
}

#' @export
#' @rdname boolean_ops
all_false <- function(x) {
  all(is_false(x))
}

#' @export
#' @rdname boolean_ops
all_false_x <- function(x) {
  if (all_na(x)) NA else all(is_false(x))
}

logical_error <- function() {
  stop(sprintf(
    "%s : `%s` is not logical.",
    deparse(sys.calls()[[sys.nframe()-2]]),
    deparse(sys.calls()[[2]][[2]])
  ))
}

# is_na("bad variable")
