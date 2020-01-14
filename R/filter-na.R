#' Filter for missing data
#'
#' Filter rows for missing data
#'
#' @param df A data.frame.
#' @param cols Columns to include.  If NULL, all columns will be examined.
#'
#' @export

keep_na <- function(df, ...)
{
  subset(df, apply(df, 1, anyNa))
}

keep_nin <- function(df, ...) {
  subset(df, apply(df, 1, anyNin))
}


df <- dplyr::sample_n(airquality, 10000, T)

microbenchmark::microbenchmark(
  # a = subset(df, vapply(as.data.frame(t(df)), anyNA, logical(1))),
  anyNA   = subset(df, apply(df, 1, anyNA)),
  is_nin  = subset(df, apply(df, 1, is_nin)),
  any_nin = subset(df, apply(df, 1, any_nin))
)

is_nin <- function(x) {
  vapply(x, any_nin, logical(1), USE.NAMES = T)
}

any_nin <- function(x) {
  is.null(x) | is.na(x) | is.infinite(x)
}

x <- c(1, "a", NA, Inf, -Inf, NULL)

is_nin(x)
which_nin(x)
any_nin(x)
anyNA(x)

which_nin <- function(x) {
  which(drop_names(is_nin(x)), useNames = FALSE)
}

drop_names <- function(x) {
  names(x) <- NULL
  x
}

