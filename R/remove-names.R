#' Remove names from an object
#'
#' Removes the `names`, `dimnames` and/or `rownames` from an object.
#' Documentation for [base::unname()], this will not throw an error when used with a dataframe.
#' For `data.frames`, the `rownames()`, default to the row numbers.
#'
#' @param object An object
#'
#' @export
#'
#' @examples
#' df <- head(iris)
#' rownames(df) <- letters[1:6]
#' unname(df)               ## removes column names
#' \dontrun{
#' unname(df, force = TRUE) ## doesn't remove rownames; throws an error
#' }
#' remove_names(df)         ## removes column and row names

remove_names <- function(object) {
  UseMethod("remove_names", object)
}

#' @export
remove_names.default <- function(object) {
  names(object) <- NULL
  object
}

#' @export
remove_names.matrix <- function(object) {
  dimnames(object) <- NULL
  object
}

#' @export
remove_names.data.frame <- function(object) {
  rownames(object) <- NULL
  names(object) <- NULL
  object
}
