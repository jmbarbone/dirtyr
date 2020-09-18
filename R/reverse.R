#' Reverse an object
#'
#' Reverses the order of an object or a data.frame
#'
#' @param x A vector, data.frame, or matrix
#' @param ... Additional arguments sent to methods
#' @param row.names If `TRUE` will reverse row.names (default is `TRUE`)
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
#' @rdname reverse
reverse.data.frame <- function(x, row.names = TRUE, ...) {
 out <- reverse.matrix(x)

 if (!row.names) {
   row.names(out) <- row.names(x)
 }

 out
}

#' @export
reverse.matrix <- function(x, ...) {
  if (nrow(x)) x[nrow(x):1L, , drop = FALSE] else x
}
