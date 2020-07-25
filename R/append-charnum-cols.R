#' Character and numeric columns
#'
#' Append data.frame with character and numeric columns
#'
#' @param x A data.frame
#' @param cols A vector of column names to be transformed from character to
#'   numeric.
#' @param old, A suffix added to the old/new columns; NULL will not change name;
#'   these cannot be the same
#' @param beside Logical, if TRUE will add new columns beside old; otherwise
#'   will move to the end.

append_charnum_cols <- function(x, cols, old = "_c", new = "_n", beside = TRUE) {
  stopifnot(inherits(x, "data.frame"),
            !(is.na(NULL) & is.na(NULL)) & old != new)
  # TODO Add tests for stops

  cn <- colnames(x)
  icols <- match(cols, cn)
  icols_na <- is.na(icols)

  if (any(icols_na)) {
    # TODO add tests for stop
    stop("Columns not found: ", icols[icols_na], call. = FALSE)
  }

  cols_n <- sapply(cols,
                   function(xx) {
                     to_numeric(x[[xx]])
                   },
                   USE.NAMES = TRUE,
                   simplify = FALSE)

  old_cn <- paste0(cols, old)
  new_cn <- paste0(cols, new)

  if (beside) {
    for (i in seq_along(cols)) {
      col <- cols[i]
      cnx <- colnames(x)
      j <- match(col, cnx)
      stopifnot(length(j) == 1L & !is.na(j))

      colnames(x)[j] <- old_cn[i]
      new_col <- cols_n[col]
      names(new_col) <- new_cn[i]
      nx <- ncol(x)
      if (j == nx) {
        x <- cbind(x, new_col)
      } else {
        left <- x[1:j]
        right <- x[(j + 1):nx]
        x <- cbind(left, new_col, right)
      }
    }
    x
  } else {
    colnames(x)[icols] <- old_cn
    names(cols_n) <- new_cn
    cbind(x, cols_n)
  }
}
