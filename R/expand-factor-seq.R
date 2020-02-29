#' Expand ordered factor sequence
#'
#' Outputs a vector of
#'
#' @param x A vector of ordered factors
#' @param min_level Character.  The minimal for the sequence
#' @param by Integer. Increment of the sequence; 1L or greater.
#'
#' @export
#' @examples
#' x <- factor(letters[c(5:15, 20)], levels = letters, ordered = TRUE)
#' expand_factor_seq(x)
#' expand_factor_seq(x, "j", by = 3L)

expand_factor_seq <- function(x, min_level = min(x), by = 1L) {
  stopifnot(is.ordered(x) & is.integer(by) & by >= 1L)
  lvls <- levels(x)
  factor(lvls[seq(which(lvls == min_level), as.numeric(max(x)), by)],
         levels = lvls,
         ordered = TRUE)
}
