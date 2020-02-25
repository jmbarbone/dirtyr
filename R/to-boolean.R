#' To boolean
#'
#' Conditionally transforms a non-boolean vector into a boolean vector
#'
#' @param x A non-boolean vector
#' @param true A condition in which to evaluate as TRUE
#' @param false An optional condition to evlauate as FALSE
#' @param names Logical.  If TRUE, the result will print out with a name
#'
#' @export
#' @examples
#' x <- c("Y", "Y", "N", "Y", "N", "N/A", "Y - not sure", "YN", "YY")
#' to_boolean(x, "Y")
#' to_boolean(x, "Y", "N", names = TRUE)
#'
#' x <- c(1, 2, 3, 4)
#' to_boolean(x, 1, 2)
#' to_boolean(x, "<= 1", ">= 2", names = TRUE)

to_boolean <- function(x, true, false = NULL, names = FALSE) {
  stopifnot(!is.logical(x) || is.vector(x))
  UseMethod("to_boolean", x)
}

#' @export
to_boolean.default <- function(x, true, false = NULL, names = FALSE) {
  if(is.null(false)) {
    res <- logical(length(x))
    res[x == true & !is.na(x)] <- TRUE
  } else {
    res <- rep(NA, length(x))
    res[x == true & !is.na(x)] <- TRUE
    res[x == false & !is.na(x)] <- FALSE
  }
  if(names) names(res) <- x
  res
}

#' @export
to_boolean.numeric <- function(x, true, false = NULL, names = FALSE) {
  if(is.null(false)) {
    res <- logical(length(x))
  } else {
    res <- rep(NA, length(x))
  }

  switch(class(true),
         character = {
           text <- sprintf("res[x %s] <- TRUE", true)
           eval(parse(text = text))
         },
         numeric = {res[x == true] <- TRUE},
         integer = {res[x == true] <- TRUE})

  switch(class(false),
         character = {
           text <- sprintf("res[x %s] <- FALSE", false)
           eval(parse(text = text))
         },
         numeric = {res[x == false] <- FALSE},
         integer = {res[x == false] <- FALSE})
  if(names) names(res) <- x
  res
}


# Notes -------------------------------------------------------------------

'
Allow equations for factor levels and characters.

f = factor(letters, letters, ordered = TRUE)
true = " < 5"
true = " < e" or true = " < "e"" ?
'

to_boolean.ordered <- function(true, false = NULL) {

  switch(bool_factor_parse(true),
         eq = "eval as equation",
         dg = "eval as numeric",
         eq_dg = "eval as equation with numeric level",
         "default" = "not equation and not numeric; just characters"
         )
}

bool_factor_parse <- function(x) {
  x <- "< 5"
  eq <- grepl("^[<>=]", x)
  dg <- grepl("[[:digit:]]", x)

  paste(c("eq", "dg")[c(eq, dg)], collapse = "_")

}
