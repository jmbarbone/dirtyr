#' Which NA functions
#'
#' Which wrappers for finding NA values
#'
#' @param x a vector of values
#'
#' @name which_na
#' @export

which_na <- function(x) {
  UseMethod("which_na", x)
}

which_na.default <- function(x) {
  which(is.na(x))
}

#' @export
#' @rdname which_na
which_na_inf <- function(x) {
  which(is_na_inf(x))
}

#' @export
#' @rdname which_na
which_min_na <- function(x) {
  which.min(is.na(x))
}

#' @export
#' @rdname which_na
which_min_na <- function(x) {
  which.min(is_na_inf(x))
}

#' @export
#' @rdname which_na
which_nin <- function(x) {
  which(remove_names(is_nin(x)), useNames = FALSE)
}


#' Additional na functions
#'
#' Additional functions to sort for NA values
#'
#' @param x a vector or data.frame of values
#'
#' @export
#' @name nas

is_na_inf <- function(x) {
  is.na(x) | is.infinite(x)
}

#' @export
#' @rdname nas
count_na <- function(x) {
  UseMethod("count_na", x)
}

#' @export
count_na.default <- function(x) {
  sum(is.na(x))
}

#' @export
count_na.data.frame <- function(x) {
  vapply(df, count_na, integer(1), USE.NAMES = TRUE)
}

#' @export
#' @rdname nas
is_nin <- function(x) {
  vapply(x, is_nin, logical(1), USE.NAMES = TRUE)
}

#' @export
#' @rdname nas
any_nin <- function(x) {
  UseMethod("any_nin",)
}

#' @export
any_nin.default <- function(x) {
  is.null(x) | is.na(x) | is.infinite(x)
}

#' @export
any_nin.data.frame <- function(x) {
  vapply(x, any_nin, logical(1), USE.NAMES = TRUE)
}

# is_nin(x)
# which_nin(x)
# any_nin(x)
# anyNA(x)

#' Get NA values
#'
#' Return NA values from a data set.
#'
#' @param .data A data.frame.
#' @param vars Column names of the data set to check on
#' @param ... Additional arguments passed to methods
#'
#' @importFrom stats complete.cases
#'
#' @export

get_na <- function(.data, vars, ...) {
  UseMethod("get_na", .data)
}

#' @export
get_na.data.frame <- function(.data, vars = NULL, ...) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[!complete.cases(.data[, vars]), ]
}

#' @export
get_na.data.table <- function(.data, vars = NULL, ...) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[!complete.cases(.data[, vars, with = FALSE])]
}

#' @export
#' @rdname get_na
get_na_inf <- function(.data, ...) {
  UseMethod("get_na_inf", .data)
}

#' @export
get_na_inf.data.frame <- function(.data, vars = NULL, ...) {
  if(is.null(vars)) vars <- colnames(.data)
  .data[apply(.data[, vars], 1, is_na_inf), ]
}

#' Filter for missing data
#'
#' Filter rows for missing data
#'
#' @param df A data.frame.
#' @param ... Columns to include.  If not included, all columns filtered on.
#'
#' @details
#' `...` supports tidy select
#'
#' @importFrom dplyr filter_at
#' @importFrom dplyr filter_all
#' @importFrom dplyr vars
#' @importFrom dplyr any_vars
#' @importFrom rlang enquos
#' @export
#' @name keep_na
#' @examples
#' \dontrun{
#' load(dirtyr)
#' keep_na(dirty, V8)
#' keep_nin(dirty, V4)
#' }

keep_na <- function(df, ...) {
  v <- enquos(...)

  if(length(v) == 0) {
    filter_all(df, any_vars(anyNA(.)))
  } else {
    filter_at(df, vars(!!! v ), any_vars(anyNA(.)))
  }
}



#' @export
#' @rdname keep_na
keep_nin <- function(df, ...) {
  v <- enquos(...)

  if(length(v) == 0) {
    filter_all(df, any_vars(any_nin(.)))
  } else {
    filter_at(df, vars(!!! v ), any_vars(any_nin(.)))
  }
}



# df <- dplyr::sample_n(airquality, 10000, T)
#
# microbenchmark::microbenchmark(
#   # a = subset(df, vapply(as.data.frame(t(df)), anyNA, logical(1))),
#   anyNA   = subset(df, apply(df, 1, anyNA)),
#   is_nin  = subset(df, apply(df, 1, is_nin)),
#   any_nin = subset(df, apply(df, 1, any_nin))
# )

# x <- c(1, "a", NA, Inf, -Inf, NULL)

#' Class NA
#'
#' Adds NA value according to class
#'
#' @param x An object
#'
#' @noRd
#' @examples
#' lapply(list(character(1),
#'             factor(letters[1:5], levels = letters),
#'             factor(letters[1:5], levels = letters,  ordered = TRUE),
#'             numeric(1),
#'             double(1),
#'             integer(1),
#'             logical(1),
#'             as.Date("2020-02-29"),
#'             as.POSIXct("2020-02-29 14:38")),
#'        class_na) %>%
#'   sapply(class, simplify = TRUE)

class_na <- function(x) {
  cl <- class(x)[1]
  switch(cl,
         factor = factor(NA,
                         levels = levels(x)),
         ordered = factor(NA,
                          levels = levels(x),
                          ordered = TRUE),
         numeric = NaN,
         do.call(sprintf("as.%s", cl), args = list(x = NA))
  )
}

# globalVariables ---------------------------------------------------------

globalVariables(c("df", "."))
