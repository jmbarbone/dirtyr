#' Re-index
#'
#' Re-index a column based on a new index
#'
#' @param x A data.frame or named vector
#' @param ... Additional arguments passed to methods
#' @param index The name of the column to reindex by
#' @param new_index The new order of the index as a vector
#' @param add_empty Logical, whether to keep empty values when index isn't
#'   present
#'
#' @details
#' When implementing the `add_empty` argument, NA values in the same class as
#'   the data.frame (df) are assigned.
#'
#' @export
#'
#' @examples
#' iris1 <- head(iris, 5) %>% as_tibble
#' iris1$index <- 1:5
#' reindex(iris1, "index", seq(2, 8, 2))
#' reindex(iris1, "index", seq(2, 8, 2), add_empty = TRUE)

reindex <- function(x, index = NULL, new_index, add_empty = FALSE, ...) {
  UseMethod("reindex", x)
}

# x <- 2:5
# index <- NULL
# names(x) <- letters[x]
# new_index <- c(1, 3, 5, 7)
# names(new_index) <- letters[new_index]
# reindex(x, new_index = new_index, add_empty = FALSE)
# reindex(x, new_index = new_index, add_empty = TRUE)

#' @export
reindex.default <- function(x, index = NULL, new_index, add_empty = FALSE, ...) {
  stopifnot(is_named(x) & is.null(index))
  if(add_empty) {
    n <- sort(unique(c(names(x), names(new_index))))
    return(setNames(x[n], n))
  }
  setNames(x[names(new_index)], names(new_index))

}

#' @export
reindex.data.frame <- function(x, index = NULL, new_index, add_empty = FALSE, ...) {
  stopifnot(!is.null(index))
  cn <- colnames(x)
  stopifnot(index %in% cn)
  m <- match(new_index, x[[index]])
  temp <- x[remove_na(m), ]

  if(add_empty) {
    nas <- is.na(m)
    if(none(nas)) return(temp)
    ls <- lapply(x[cn[cn != index]], class_na)
    ls[[index]] <- c(new_index[which(nas)])
    new_df <- as.data.frame(ls, stringsAsFactors = FALSE)
    cnt <- colnames(temp)
    new_df <- as.data.frame(ls, stringsAsFactors = FALSE)[cnt]
    colnames(new_df) <- cnt
    temp <- r_bind(temp, new_df)
  }
  as_tibble(temp)
}
