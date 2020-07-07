#' Re-index
#'
#' Re-index a column based on a new index
#'
#' @param x A data.frame or named vector
#' @param ... Additional arguments passed to methods
#' @param index The name of the column to reindex by (see details)
#' @param new_index The new order of the index as a vector
#' @param keep_all Logical, whether to use only the index values in the
#'   `new_index` or to use all available index values
#'
#' @details
#' When implementing the `keep_all` argument, NA values in the same class as
#'   the data.frame (df) are assigned.
#' The `index` and `new_index` must be not have any duplicates.
#' If a vector is passed, the `index` argument is not necessary.
#' However, if this argument is set and `new_index` is missing, the latter will
#'   take the value of the former.
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' iris1 <- head(iris, 5)
#' iris1$index <- 1:5
#' reindex(iris1, "index", seq(2, 8, 2))
#' reindex(iris1, "index", seq(2, 8, 2), keep_all = TRUE)
#'
#' x <- c("a" = 1, "b" = 2, "d" = 4)
#' y <- setNames(nm = letters[2:5])
#' reindex(x, new_index = y )
#' reindex(x, new_index = y, keep_all = TRUE)
#'
#' x <- 2:5
#' index <- NULL
#' names(x) <- letters[x]
#' new_index <- c(1, 3, 5, 7)
#' names(new_index) <- letters[new_index]
#' reindex(x, new_index = new_index)
#' reindex(x, new_index = new_index, keep_all = TRUE)
#'
#' reindex(setNames(nm = letters[2:4]), letters[1:5])

reindex <- function(x, index, new_index, keep_all = FALSE, ...) {
  UseMethod("reindex", x)
}


#' @export
reindex.default <- function(x, index, new_index, keep_all = FALSE, ...) {
  if(missing(new_index)) new_index <- index
  stopifnot(is_named(x))
  if(!is_named(new_index)) new_index <- setNames(nm = new_index)
  stopifnot(unique_name_check(x) & unique_name_check(new_index))

  if(keep_all) {
    n <- sort(unique(c(names(x), names(new_index))))
    return(setNames(x[n], n))
  }
  setNames(x[names(new_index)], names(new_index))

}

# x <- a
# index <- "index"
# new_index <- b$index
# keep_all = TRUE

#' @export
reindex.data.frame <- function(x, index , new_index, keep_all = FALSE, ...) {
  xi <- x[[index]]
  stopifnot(unique_name_check(xi), unique_name_check(new_index))
  cn <- colnames(x)
  stopifnot(index %in% cn)
  m <- match(new_index, xi)
  temp <- x[remove_na(m), ]

  if(keep_all) {
    nas <- is.na(m)
    if(none(nas)) return(temp)
    ls <- lapply(x[cn[cn != index]], class_na)
    ls[[index]] <- unique(c(xi %wo% new_index, c(new_index[which(nas)])))
    new_df <- as.data.frame(ls, stringsAsFactors = FALSE)
    cnt <- colnames(temp)
    temp <- r_bind(list(
      temp,
      as.data.frame(ls,
                    stringsAsFactors = FALSE,
                    optional = TRUE)[cnt]))
  }
  temp
}
