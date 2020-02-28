#' Filter for missing data
#'
#' Filter rows for missing data
#'
#' @param df A data.frame.
#' @param ... Columns to include.  If not included, all columns filtered on.
#'
#' @details
#' `...` supports tidy select
#'
#' @importFrom dplyr filter_at
#' @importFrom dplyr vars
#' @importFrom dplyr any_vars
#' @export
#' @name keep_na
#' @examples
#' \notrun{
#' load(dirtyr)
#' keep_na(dirty, V8)
#' keep_nin(dirty, V4)
#' }

keep_na <- function(df, ...) {
  v <- enquos(...)

  if(length(v) == 0) {
    filter_all(df, any_vars(anyNA(.)))
  } else {
    filter_at(df, vars(!!! v ), any_vars(anyNA(.)))
  }
}



#' @export
#' @rdname keep_na
keep_nin <- function(df, ...) {
  v <- enquos(...)

  if(length(v) == 0) {
    filter_all(df, any_vars(any_nin(.)))
  } else {
    filter_at(df, vars(!!! v ), any_vars(any_nin(.)))
  }
}



# df <- dplyr::sample_n(airquality, 10000, T)
#
# microbenchmark::microbenchmark(
#   # a = subset(df, vapply(as.data.frame(t(df)), anyNA, logical(1))),
#   anyNA   = subset(df, apply(df, 1, anyNA)),
#   is_nin  = subset(df, apply(df, 1, is_nin)),
#   any_nin = subset(df, apply(df, 1, any_nin))
# )

# x <- c(1, "a", NA, Inf, -Inf, NULL)


