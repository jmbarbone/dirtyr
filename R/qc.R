#' Quality check
#'
#' Perform a QC on an object.
#'
#' @details
#' If both objects are named, they will be `reindex()`'d to be of equal length
#'   and sorted with matching names.
#' `qc.factor` will default to character if not ordered
#'
#' @return
#' A data.frame with each target and reference value and their difference.
#' Values in the `target` and `reference` column will always be returned as character.
#'
#' \item{differences}{A logical vector (`TRUE` or `FALSE` if items were different)}
#'
#' @param target Target
#' @param reference Reference
#' @param ... Additional arguments passed to methods
#' @param ignore_case Should cases be ignored?
#' @param string_dist If TRUE, string distances calculated with [stringr::stringdist]
#' @export
qc <- function(target, reference, ...) {
  UseMethod("qc", target)
}

#' @export
#' @rdname qc
qc.default <- function(target, reference, ...) {
  stopifnot(class(target) == class(reference))
  warning("No qc method available.", call. = FALSE)
}

#' @export
#' @rdname qc
qc.logical <- function(target, reference, ...) {
  qc_name_check(target, reference)

  if(all_na(target) & class(reference) != "logical") {
    cr <- class(reference)
    class(target) <- cr
    warning("target is all `NA` ... trying method for: ", cr)
    return(qc(target, reference, ...))
  }
  d <- are_different(target, reference)
  if(!sum(d)) {
    message("No differences found")
    return(invisible())
  }

  res <- qc_df(target, reference, (target - reference)[d], d)
  attr(res, "differences") <- setNames(d, names(target))
  res
}

#' @export
#' @rdname qc
qc.character <- function(target, reference, string_dist = FALSE, ignore_case = FALSE, ...) {
  qc_name_check(target, reference)
  if(ignore_case) {
    tar <- tolower(target)
    ref <- tolower(reference)
  } else {
    tar <- target
    ref <- reference
  }
  d <- are_different(tar, ref)
  ds <- sum(d)
  if(!ds) {
    message("No differences found")
    return(invisible())
  }
  if(string_dist) {
    require_namespace("stringdist")
    diffs <- mapply(stringdist::stringdist,
                    a = tar[d],
                    b = ref[d],
                    ...,
                    USE.NAMES = FALSE)
  } else {
    diffs <- rep(NA_real_, ds)
  }
  # qc_df(target, reference, diffs, x)
  res <- data_frame(index = if(is_named(target)) names(target)[d] else seq_along(target)[d],
                    target = as.character(target)[d],
                    reference = as.character(reference)[d],
                    difference = diffs)
  attr(res, "differences") <- d
  res
}

# target = x
# reference = y
# threshold = 1L
# string_dist = FALSE

#' @export
qc.ordered <- function(target, reference, threshold = 0L, ..., string_dist = FALSE) {
  if(anyNA(match(levels(target), levels(reference)))) {
    warning("Levels do not match, applying factor method", call. = FALSE)
    return(qc.factor(unordered(target),
                     unordered(reference),
                     string_dist = string_dist,
                     ...))
  }
  if(string_dist) {
    warning("String distances will not be computed for factors", call. = FALSE)
  }
  qc_name_check(target, reference)
  d <- are_different(target, reference)
  diffs <- as.integer(target) - as.integer(reference)
  d <- d & !is_false(abs(diffs) > as.integer(threshold))
  res <- qc_df(target, reference, diffs[d], d)
  attr(res, "differences") <- d
  res
}

#' @export
qc.factor <- function(target, reference, string_dist = FALSE, ...) {
  qc_name_check(target, reference)
  if(string_dist) {
    warning("String distances will not be computed for factors", call. = FALSE)
  }
  qc.character(target, reference, string_dist = FALSE, ignore_case = FALSE, ...)
}

#' @export
qc.numeric <- function(target, reference, threshold = 0.0, ...) {
  qc_name_check(target, reference)

  d <- are_different(target, reference)
  diffs <- target - reference
  d <- d & !is_false(abs(diffs) > threshold)

  res <- qc_df(target, reference, diffs[d], d)
  # res <- data.frame(
  #   target = as.character(target[x]),
  #   reference = as.character(reference[x]),
  #   difference = diffs[x],
  #   stringsAsFactors = FALSE
  # )
  attr(res, "differences") <- d
  res
}

#' @export
qc.integer <- function(target, reference, threshold = 0, ...) {
  qc.numeric(target, reference, threshold = threshold)
}

#' @export
qc.Date <- function(target, reference, threshold = 0, ...) {
  qc.numeric(target, reference, threshold = threshold)
}

#' @export
qc.POSIXct <- function(target, reference, threshold = 0, ...) {
  qc.numeric(as.numeric(target), as.numeric(reference), threshold = threshold)
}


# utils -------------------------------------------------------------------

are_different <- function(x, y) {
  if(length(x) != length(y)) {
    stop(sprintf("`%s` and `%s` are not the same length",
                 deparse(substitute(x)),
                 deparse(substitute(y))),
         call. = FALSE)
  }

  res <- if("factor" %in% class(x)) {
    as.integer(x) != as.integer(y)
  } else {
    x != y
  }

  n <- is.na(res)
  if(sum(n)) {
    res[n] <- !(is.na(x[n]) & is.na(y[n]))
  }
  res
}

unordered <- function(f) {
  factor(f,
         levels = levels(f),
         ordered = FALSE)
}

qc_name_check <- function(x, y) {
  if(is_named(x) & is_named(y)) {
    assign(deparse(substitute(x)),
           reindex(x, y, keep_all = TRUE),
           envir = parent.frame(1))
    assign(deparse(substitute(y)),
           reindex(y, x, keep_all = TRUE),
           envir = parent.frame(1))
  }
  invisible()
}

qc_df <- function(target, reference, difference, d) {
  data_frame(
    index = if(is_named(target)) names(target)[d] else seq_along(target)[d],
    target = as.character(target[d]),
    reference = as.character(reference[d]),
    difference = as.double(difference)
  )
}
