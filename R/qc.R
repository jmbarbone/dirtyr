#' Quality check
#'
#' Perform a QC on an object
#'
#' @param target Target
#' @param reference Reference
#' @param ... Additional arguments passed to methods
#' @export
qc <- function(target, reference, ...) {
  UseMethod("qc", target)
}

#' @export
qc.default <- function(target, reference, ...) {
  stopifnot(class(target) == class(reference))
  warning("No qc method available.", call. = FALSE)
}

#' @export
qc.logical <- function(target, reference, ...) {

  if(all_na(target) & class(reference)  != "logical") {
    class(target) <- class(reference)
    message("target is all `NA`; trying method for:", class(reference))
    qc(target, reference, ...)
  }
  x <- !mapply(identical, target, reference)
  diffs <- as.numeric(target) - as.numeric(reference)

  xs <- sum(x)
  if(xs == 0) {
    message("No differences found")
    return(invisible())
  }
  res <- data.frame(
    target = target[x],
    reference = reference[x],
    difference = diffs[x],
    stringsAsFactors = FALSE
  )
  attr(res, "differences") <- x
  res
}

#' @export
qc.character <- function(target, reference, string_dist = FALSE, ignore_case = FALSE, ...) {
  if(ignore_case) {
    tar <- tolower(target)
    ref <- tolower(reference)
  } else {
    tar <- target
    ref <- reference
  }
  x <- suppress_wm(tar != ref)
  x <- x | is.na(x)
  # x <- !mapply(identical, tar, ref)
  xs <- sum(x)
  if(xs == 0) {
    message("No differences found")
    return(invisible())
  }
  if(string_dist) {
    diffs <- mapply(stringdist::stringdist,
                    a = tar[x],
                    b = ref[x],
                    ...,
                    USE.NAMES = FALSE)
  } else {
    diffs <- rep(NA_real_, xs)
  }
  res <- data.frame(
    target = target[x],
    reference = reference[x],
    difference = diffs,
    stringsAsFactors = FALSE
  )
  attr(res, "differences") <- x
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




# Global variables ----------------------------------------------------------------------------

globalVariables(c("."))
