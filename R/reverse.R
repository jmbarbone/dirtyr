#' Reverse an object
#'
#' Reverses the order of an object or a data.frame
#'
#' @param x A vector, data.frame, or matrix
#' @param ... Additional arguments sent to methods (not used)
#' @export
#'
#' @examples
#' reverse(letters)
#' head(iris)
#' reverse(iris) %>% tail()

reverse <- function(x, ...) {
  UseMethod("reverse", x)
}

#' @export
reverse.default <- function(x, ...) {
  rev(x)
}

#' @export
reverse.data.frame <- function(x, ...) {
 reverse.matrix(x)
}

#' @export
reverse.matrix <- function(x, ...) {
  if(nrow(x)) x[nrow(x):1L, ] else x
}
