#' Filter for missing data
#'
#' Filter rows for missing data
#'
#' @param df A data.frame.
#' @param cols Columns to include.  If NULL, all columns will be examined.
#'
#' @export
#' @name keep_na

keep_na <- function(df, ...)
{
  subset(df, apply(df, 1, anyNa))
}

#' @export
#' @rdname keep_na
keep_nin <- function(df, ...) {
  subset(df, apply(df, 1, anyNin))
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


