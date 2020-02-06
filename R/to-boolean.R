#' To boolean
#'
#' Conditionally transforms a non-boolean vector into a boolean vector
#'
#' @param x A non-boolean vector
#' @param true A condition in which to evaluate as TRUE
#' @param false An optional condition to evlauate as FALSE
#' @param names Logical.  If TRUE, the result will print out with a name
#'
#' @export
#' @examples
#' x <- c("Y", "Y", "N", "Y", "N", "N/A", "Y - not sure", "YN", "YY")
#' to_boolean(x, "Y")
#' to_boolean(x, "Y", "N", names = TRUE)

to_boolean <- function(x, true, false = NULL, names = FALSE) {
  UseMethod("to_boolean", x)
}

to_boolean.default <- function(x, true, false = NULL, names = FALSE) {
  stopifnot(!is.logical(x) || is.vector(x))
  n <- paste(x)
  x <- as.list(n)
  if(names) names(x) <- n

  x[x == true & !is.na(x)] <- 'TRUE'

  if(is.null(false)) {
    x[x != 'TRUE' | is.na(x)] <- 'FALSE'
  } else {
    x[x == false] <- 'FALSE'
    nas <- (x != 'TRUE' & x != 'FALSE') | is.na(x)
    if(any(nas)) {
      message("NAs found: ", paste(unique(x[nas]), collapse = "\n"))
    }
    x[nas] <- 'NA'
  }
  as.logical(unlist(x))
}
