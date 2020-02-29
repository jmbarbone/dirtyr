#' Re-index
#'
#' Re-index a column based on a new index
#'
#' @param df A data.frame
#' @param index The name of the column to reindex by
#' @param new_index The new order of the index as a vector
#' @param add_empty Logical, whether to keep empty values when index isn't present
#'
#' @details
#' When implementing the `add_empty` argument, NA values in the same class as the data.frame (df)
#' are assigned.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' iris1 <- head(iris, 5) %>% as_tibble
#' iris1$index <- 1:5
#' reindex(iris1, "index", seq(2, 8, 2))
#' reindex(iris1, "index", seq(2, 8, 2), add_empty = TRUE)

reindex <- function(df, index, new_index, add_empty = FALSE) {
  cn <- colnames(df)
  stopifnot(index %in% cn)
  m <- match(new_index, df[[index]])
  temp <- df[na.omit(m), ]

  if(add_empty) {
    nas <- is.na(m)
    if(none(nas)) return(temp)
    ls <- lapply(df[cn[cn != "index"]], class_na)
    ls[[index]] <- c(new_index[which(nas)])
    return(rbind(temp,
                 as.data.frame(ls, stringsAsFactors = FALSE)))
  }
  temp

}
