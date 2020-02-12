#' Get NA values
#'
#' Return NA values from a data set.
#'
#' @param .data A data.frame.
#' @param vars Column names of the data set to check on
#' @param ... Additional arguments passed to methods
#'
#' @importFrom stats complete.cases
#'
#' @export

get_na <- function(.data, vars, ...) {
  UseMethod("get_na", .data)
}

get_na.data.frame <- function(.data, vars = NULL) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[!complete.cases(.data[vars]), ]
}

get_na.data.table <- function(.data, vars = NULL) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[!complete.cases(.data[ , vars, with = FALSE])]
}

#' @export
#' @rdname get_na
get_na_inf <- function(.data, ...) {
  UseMethod("get_na_inf", .data)
}

get_na_inf.data.frame <- function(.data, vars = NULL) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[apply(.data[vars], 1, is_na_inf), ]
}
