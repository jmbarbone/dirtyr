#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom tibble as_tibble
tibble::as_tibble

data_frame <- function(..., row.names = NULL, check.rows = !is.null(row.names)) {
  data.frame(...,
             row.names = row.names,
             check.rows = check.rows,
             check.names = FALSE,
             fix.empty.names = FALSE,
             stringsAsFactors = FALSE)
}

# df <- data.frame(
#   A = c(1, 2, NA, 4, 5, NA),
#   B = c(6, NA, 8, 9, 9, NaN),
#   C = c("A", "B", "C", "D", "E", NA_character_)
# )

if_then <- function(x, FUN, y) {
  if (FUN(x)) y else x
}

is_named <- function(x) {
  !is.null(names(x))
}

insert <- function(x, y, p) {
  pos <- seq_along(x)
  c(x[pos < p], y, x[pos >= p])
}

# insert(letters[1:4], "xx", 4)

suppress_wm <- function(x) {
  suppressWarnings(suppressMessages(x))
}

short_class <- function(x) {
  cl <- class(x)[1]
  switch(cl,
         ordered = "ordered",
         integer = "numeric",
         factor = "text",
         cl)
}

## this is just, as.numeric(f) isn't it?
which_level <- function(f) {
  sapply(f, function(f) which(f == levels(f)), USE.NAMES = FALSE)
}
# f <- factor(letters)
# which_level(f)
# as.numeric(f)

none <- function(..., na.rm = FALSE) {
  !any(..., na.rm = na.rm)
}

all_na <- function(x, ...) {
  UseMethod("all_na", x)
}

all_na.default <- function(x) {
  all(is.nan(x) | is.na(x))
}

all_na.character <- function(x, convert = FALSE) {
  found <- which(x == "NaN")
  if (convert) {
    x[found] <- NA_character_
  } else if (length(found) > 0) {
    warning("These values may be NA types `convert`ed to character:\n",
            paste(paste0("    ",  x[found]), collapse = "\n"),
            call. = FALSE)
  }
  all_na.default(x)
}

is_namespace_missing <- function(namespace) {
  !requireNamespace(namespace, quietly = TRUE)
}

require_namespace <- function(namespace) {
  if (is_namespace_missing(namespace)) {
    stop(sprintf("Package \"%s\" needed for this function to work",
                 namespace),
         call. = FALSE)
  }
}

remove_na <- function(x) {
  x[!is.na(x)]
}

r_bind_fun <-  function(x, y) {
  rbind(x, y,
        deparse.level = 0,
        make.row.names = FALSE,
        factor.exclude = TRUE,
        stringsAsFactors = FALSE)
}

#' rbind() for lists
#'
#' Apply rbind to a list of data.frames
#'
#' @param ls List of data frames to bind
r_bind <- function(ls) {
  Reduce(r_bind_fun, ls)
}

unique_name_check <- function(x) {
  if (is_named(x)) {
    x <- names(x)
  }
  all(unique(x) == x)
}

