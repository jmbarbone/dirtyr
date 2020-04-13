#' Quality check
#'
#' Perform a QC on an object.
#'
#' @details
#' If both objects are named, they will be `reindex()`'d to be of equal length
#'   and sorted with matching names.
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
    message("No differences found between.")
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

  res <- data_frame(target = target[d],
                    reference = reference[d],
                    difference = diffs)
  attr(res, "differences") <- d
  res
}

#' @export
qc.ordered <- function(target, reference, threshold = 0, ..., string_dist = FALSE) {
  lvls <- levels(target)
  if(all(lvls != levels(reference))) {
    warning("Levels do not match, applying factor method")
    qc(as.character(target), as.character(reference), string_dist = string_dist)
  }

  diffs <- suppress_wm(as.numeric(target) - as.numeric(reference))
  x <- abs(diffs) > threshold | is.na(diffs)
  res <- qc_df(target, reference, diffs, x)
  attr(res, "differences") <- x
  res
}

#' @export
qc.factor <- function(target, reference, string_dist = FALSE, ...) {
  message("qc.factor will default to character if not ordered")
  if(string_dist) warning("String distances will not be computed for factors", call. = FALSE)
  qc.character(target, reference, string_dist = FALSE, ignore_case = FALSE)
}

#' @export
qc.numeric <- function(target, reference, threshold = 0, ...) {
  diffs <- suppress_wm(target - reference)
  x <- abs(diffs) > threshold | is.na(diffs)
  res <- data.frame(
    target = as.character(target[x]),
    reference = as.character(reference[x]),
    difference = diffs[x],
    stringsAsFactors = FALSE
  )
  attr(res, "differences") <- x
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
  qc.numeric(target, reference, threshold = threshold)
}


# utils -------------------------------------------------------------------

are_different <- function(x, y) {
  if(length(x) != length(y)) {
    stop(sprintf("`%s` and `%s` are not the same length",
                 deparse(substitute(x)),
                 deparse(substitute(y))),
         call. = FALSE)
  }
  res <- x != y
  n <- is.na(res)
  if(sum(n)) {
    res[n] <- !(is.na(x[n]) & is.na(y[n]))
  }
  res
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
    target = as.character(target[d]),
    reference = as.character(reference[d]),
    difference = as.double(difference)
  )
}
