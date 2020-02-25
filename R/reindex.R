#' Re-index
#'
#' Re-index a column based on a new index
#'
#' @param df A data.frame
#' @param index The name of the column to reindex by
#' @param new_index the new order of the index
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' iris1 <- head(iris, 5)
#' iris1$index <- 1:5
#' iris2 <- iris1[c(2, 3, 5, 4, 1), ]
#' reindex(iris1, "index", iris2$index)

reindex <- function(df, index, new_index) {
  stopifnot(index %in% colnames(df))
  df[na.omit(match(new_index, df[[index]])), ]
}
