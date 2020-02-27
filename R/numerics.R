#' Numeric Vectors - expanded
#'
#' Wrappers for working with numeric vectors
#'
#' @param x A vector
#' @param names Logical.  If true, supplies original value as name.
#'
#' @details
#' The biggest difference in `as_numeric` compared to `as.numeric` is that it returns values of `NaN`
#' rather than `NA` and does not through an error.
#'
#' `maybe_numeric` can be used to check is a value, possibly stored as text, can be safely converted.
#'
#' @export
#'
#' @examples
#' x <- c(as.character(1:5), NA, letters[1:2], "1e4")
#' suppressWarnings(as.numeric(x))  ## will throw an error
#' as_numeric(x)                    ## converts NA to NaN
#' maybe_numeric(x, names = TRUE)

as_numeric <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  n[is.na(n)] <- NaN
  n
}

#' @export
#' @rdname as_numeric
maybe_numeric <- function(x, names = FALSE) {
  vapply(x,
         function(x) !is.na(suppressWarnings(as.numeric(x))),
         logical(1),
         USE.NAMES = names)
}
