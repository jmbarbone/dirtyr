#' Numeric Vectors - expanded
#'
#' Wrappers for working with numeric vectors
#'
#' @param x A vector
#' @param int Logical.  Integer control, if TRUE will determine if vector may be integers.
#' @param names Logical.  If true, supplies original value as name.
#'
#' @details
#' Unlike, `as.numeric()`, `to_numeric()` will return outpus as integers or as doubles.
#'
#' Factors are also specially handled as a numeric value is derived from the factor value rather
#' than the factor level.
#'
#' `maybe_numeric` can be used to check is a value, possibly stored as text, can be safely converted.
#' When `int = TRUE`, `maybe_integer()` is employed to evaluation the vector.
#' If all values could be integers, the vector is returned as an integer.
#'
#' @export
#'
#' @examples
#' ## with characters
#' x <- c(as.character(1:5), NA, letters[1:2], "1e4")
#' suppressWarnings(as.numeric(x))  ## will throw an error
#' to_numeric(x)                    ## converts NA to NaN
#' maybe_numeric(x, names = TRUE)
#'
#' ## With factors
#' f <- factor(c(seq(0, 1, .2), "No", "Na_character_"))
#' suppressWarnings(as.numeric(f))  ## uses factor level instead of text
#' to_numeric(f)                    ## uses text values instead
#' maybe_numeric(f, names = TRUE)

to_numeric <- function(x, int = TRUE) {
  UseMethod("to_numeric", x)
}

#' @export
to_numeric.default <- function(x, int = TRUE) {
  if (int & all(maybe_integer(x))) {
    as.integer(x)
  } else {
    suppressWarnings(as.double(x))
  }
}

#' @export
to_numeric.factor <- function(x, int = TRUE) {
  if (int & all(maybe_integer(x))) {
    as.integer(levels(x))[x]
  } else {
    suppressWarnings(as.double(levels(x))[x])
  }
}

#' @export
#' @rdname to_numeric
maybe_numeric <- function(x, names = FALSE) {
  UseMethod("maybe_numeric", x)
}

#' @export
maybe_numeric.default <- function(x, names = FALSE) {
  res <- !is.na(suppressWarnings(as.double(x)))
  res[is.na(x)] <- TRUE
  if (names) {
    names(res) <- x
  }
  res
}

#' @export
maybe_numeric.factor <- function(x, names = FALSE) {
  res <- !is.na(suppressWarnings(as.double(levels(x))[x]))
  res[is.na(x)] <- TRUE
  if (names) {
    names(res) <- as.character(x)
  }
  res
}

#' @export
#' @rdname to_numeric
maybe_integer <- function(x, names = FALSE) {
  UseMethod("maybe_integer", x)
}

#' @export
maybe_integer.default <- function(x, names = FALSE) {
  xx <- suppressWarnings(as.double(x))
  res <- xx == round(xx)
  res[is.na(x)] <- TRUE
  res[is.na(res)] <- FALSE
  if (names) {
    names(res) <- x
  }
  res
}

#' @export
maybe_integer.integer <- function(x, names = FALSE){
  res <- !logical(length(x))
  if (names) {
    names(res) <- x
  }
  res
}

#' @export
maybe_integer.numeric <- function(x, names = FALSE) {
  res <- x == round(x)
  res[is.na(x)] <- TRUE
  if (names) {
    names(res) <- res
  }
  res
}

#' @export
maybe_integer.factor <- function(x, names = FALSE) {
  xx <- suppressWarnings(as.double(levels(x))[x])
  res <- xx == round(xx)
  res[is.na(x)] <- TRUE
  res[is.na(res)] <- FALSE
  if (names) {
    names(res) <- as.character(x)
  }
  res
}
