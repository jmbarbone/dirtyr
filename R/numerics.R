#' Numeric Vectors - expanded
#'
#' Wrappers for working with numeric vectors
#'
#' @param x A vector
#' @param names Logical.  If true, supplies original value as name.
#'
#' @details
#' The biggest difference in `as_numeric` compared to `as.numeric` is that it returns values of `NaN`.
#' rather than `NA` and does not through an error.
#' Factors are also specially handled as a numeric value is derived from the factor value rather
#' than the factor level.
#'
#' `maybe_numeric` can be used to check is a value, possibly stored as text, can be safely converted.
#'
#' @export
#'
#' @examples
#' ## with characters
#' x <- c(as.character(1:5), NA, letters[1:2], "1e4")
#' suppressWarnings(as.numeric(x))  ## will throw an error
#' as_numeric(x)                    ## converts NA to NaN
#' maybe_numeric(x, names = TRUE)
#'
#' ## With factors
#' f <- factor(c(seq(0, 1, .2), "No", "Na_character_"))
#' suppressWarnings(as.numeric(f))  ## uses factor level instead of text
#' as_numeric(f)                    ## uses text values instead
#' maybe_numeric(f, names = TRUE)

as_numeric <- function(x) {
  UseMethod("as_numeric", x)
}

#' @export
as_numeric.default <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  n[is.na(n)] <- NaN
  n
}

#' @export
as_numeric.numeric <- function(x) {
  x[is.na(x)] <- NaN
  x
}

#' @export
as_numeric.integer <- function(x) {
  x[is.na(x)] <- NA_integer_
  x
}

#' @export
as_numeric.factor <- function(x) {
  n <- suppressWarnings(as.numeric(levels(x))[x])
  n[is.na(n)] <- NaN
  n
}

#' @export
#' @rdname as_numeric
maybe_numeric <- function(x, names = FALSE) {
  UseMethod("maybe_numeric", x)
}

#' @export
maybe_numeric.default <- function(x, names = FALSE) {
  vapply(x,
         function(x) !is.na(suppressWarnings(as.numeric(x))),
         logical(1),
         USE.NAMES = names)
}

#' @export
maybe_numeric.factor <- function(x, names = FALSE) {
  res <- vapply(x,
                function(x) !is.na(suppressWarnings(as.numeric(levels(x))[x])),
                logical(1))
  if(names) names(res) <- as.character(x)
  res
}

