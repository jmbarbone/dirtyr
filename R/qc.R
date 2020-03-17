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
    diffs <- rep(NaN, xs)
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
qc.data.frame <- function(target, reference, index, string_dist = FALSE, add_empty = TRUE, ...) {
  if(!is_named(index)) names(index) <- index

  reind_tar <- reindex(target, names(index), reference[[index]], add_empty = TRUE)

  cols <- colnames(reference)
  valid_cols <- cols[cols %in% colnames(target) & cols != names(index)]

  # valid_cols <- valid_cols[4]
  lapply(valid_cols,
         function(vc) {
           qc_col_implement(
             tar = reind_tar[[vc]],
             ref = reference[[vc]],
             ind = reference[[index]],
             vc = vc)
         }) %>%
    Reduce(rbind, .)
}

# implementation of qc for each column
qc_col_implement <- function(tar, ref, ind, vc) {
  temp <- qc(tar, ref)
  if(nrow(temp) == 0) return(NULL)
  diff_attr <- attr(temp, "differences")
  cbind(data.frame(index = ind[diff_attr | is.na(diff_attr)],
                   comparison = rep(vc, nrow(temp)),
                   stringsAsFactors = FALSE),
        temp)
}


# Global variables ----------------------------------------------------------------------------

globalVariables(c("."))
