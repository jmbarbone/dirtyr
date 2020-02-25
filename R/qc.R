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
qc.default <- function(target, reference) {
  warning("No qc method available.", call. = FALSE)
}

#' @export
qc.character <- function(target, reference, string_dist = FALSE, ignore_case = FALSE, ...) {
  if(ignore_case) {
    tar <- tolower(target)
    ref <- tolower(ref)
  } else {
    tar <- target
    ref <- reference
  }
  x <- tar != ref
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

target = c("this", "that", "those", "what?")
reference = c("thas", "THAT", "what are those?", "what")
string_dist = TRUE
ignore_case = TRUE
# try <- qc(target, reference)
# attr(try, "differences")

#' @export
qc.ordered <- function(target, reference, threshold = 0, ..., string_dist = FALSE) {
  lvls <- levels(target)
  if(all(lvls != levels(reference))) {
    warning("Levels do not match, applying factor method")
    qc(as.character(target), as.character(reference), string_dist = string_dist)
  }

  diffs <- which_level(target) - which_level(reference)
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
target = factor(letters[1:5], letters, ordered = TRUE)
reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = TRUE)
threshold = 0
# qc(target, reference, string_dist = TRUE)
# attr(.Last.value, "differences")

#' @export
qc.factor <- function(target, reference, string_dist = FALSE) {
  message("qc.factor will default to character if not ordered")
  if(string_dist) warning("String distances will not be computed for factors", call. = FALSE)
  qc.character(target, reference, string_dist = FALSE, ignore_case = FALSE)
}

target = factor(letters[1:5], letters, ordered = FALSE)
reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = FALSE)
threshold = 0
# qc(target, reference, string_dist = FALSE)
# attr(.Last.value, "string_dist")

#' @export
qc.numeric <- function(target, reference, threshold = 0) {
  diffs <- target - reference
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
qc.integer <- function(target, reference, threshold = 0) {
  qc.numeric(target, reference, threshold = threshold)
}
# qc(target = 1:5, reference = c(1:2, 8, 10, 11), threshold = 1)

#' @export
qc.Date <- function(target, reference, threshold = 0) {
  qc.numeric(target, reference, threshold = threshold)
}

# qc(as.Date(c("2019-01-12", "2010-05-20")),
#    as.Date(c("2019-01-14", "2019-05-20")))
# attributes(.Last.value)

#' @export
qc.data.frame <- function(target, reference, index, string_dist = FALSE, ...) {
  if(!is_named(index)) names(index) <- index

  reind_ref <- reindex(reference, names(index), reference[[index]])

  cols <- colnames(reference)
  valid_cols <- cols[cols %in% colnames(target) & cols != names(index)]

  # valid_cols <- valid_cols[4]
  lapply(valid_cols,
         function(vc) {
           qc_col_implement(
             tar = target[[vc]],
             ref = reind_ref[[vc]],
             ind = target[[names(index)]],
             vc = vc)
         }) %>%
    Reduce(rbind, .)
}

# qc(target, reference, "index")

# implementation of qc for each column
qc_col_implement <- function(tar, ref, ind, vc) {
  temp <- qc(tar, ref)
  if(nrow(temp) == 0) return(NULL)
  cbind(data.frame(index = ind[attr(temp, "differences")],
                   comparison = rep(vc, nrow(temp)),
                   stringsAsFactors = FALSE),
        temp)
}

# target <- data.frame(
#   index = letters[1:4],
#   col1 = c(1:4),
#   col2 = c(2:5),
#   col3 = c("abc", "aaa", "bbb", "ccc"),
#   col4 = as.Date(c("2019-12-01", "2019-02-21", "2000-05-20", "2019-12-17")),
#   col5 = factor(letters[1:4], levels = letters, ordered = TRUE),
#   stringsAsFactors = FALSE
# )
#
# reference <- data.frame(
#   index = letters[1:4],
#   col1 = c(1, 2, 10, 4),
#   col2 = c(1, 3, 4, 4),
#   col3 = c("def", "aaa", "bbb", "nonsense"),
#   col4 = as.Date(c("2019-12-10", "2019-02-22", "2000-05-20", "2019-12-17")),
#   col5 = factor(letters[c(3, 7, 11, 13)], levels = letters, ordered = TRUE),
#   stringsAsFactors = FALSE
# )
#
# reference <- reference[c(2, 3, 1, 4), ]
# index = "index"
# str_dist = FALSE

# qc(target, reference, "index")


# Global variables ----------------------------------------------------------------------------

globalVariables(c("."))
