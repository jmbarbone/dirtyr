#' Which NA functions
#'
#' Which wrappers for finding NA values
#'
#' @param x a vector of values
#'
#' @name which_na
#' @export

which_na <- function(x) {
  UseMethod("which_na", x)
}

which_na.default <- function(x) {
  which(is.na(x))
}

#' @export
#' @rdname which_na
which_na_inf <- function(x) {
  which(is_na_inf(x))
}

#' @export
#' @rdname which_na
which_min_na <- function(x) {
  which.min(is.na(x))
}

#' @export
#' @rdname which_na
which_min_na <- function(x) {
  which.min(is_na_inf(x))
}

#' @export
#' @rdname which_na
which_nin <- function(x) {
  which(remove_names(is_nin(x)), useNames = FALSE)
}
