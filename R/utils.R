#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

df <- data.frame(
  A = c(1, 2, NA, 4, 5, NA),
  B = c(6, NA, 8, 9, 9, NaN),
  C = c("A", "B", "C", "D", "E", NA_character_)
)

if_then <- function(x, FUN, y) {
  if(FUN(x)) y else x
}

make_dirtyr <- function(n = 1e7) {
  set.seed(42)
  dirty <- as.data.frame(matrix(data = stats::runif(n), ncol = 20))
  dirty[dirty < .05] <- NaN
  dirty[dirty > .99] <- -Inf
  dirty[dirty > .95] <- Inf
  dirty
}
dirty <- make_dirtyr(100)

# saveRDS(make_dirtyr(), "data/dirty.rds")
# saveRDS(make_dirtyr(100), "data/dirty_small.rds")

is_named <- function(x) {
  # !is.null(names(x))
}

insert <- function(x, y, p) {
  pos <- seq_along(x)
  c(x[pos < p], y, x[pos >= p])
}

# insert(letters[1:4], "xx", 4)

suppress_wm <- function(x) {
  suppressWarnings(suppressMessages(x))
}

#' @importFrom dplyr anti_join
#' @importFrom purrr safely

safe_anti_join <- safely(anti_join)

short_class <- function(x) {
  cl <- class(x)
  if("ordered" %in% cl) {
    "ordered"
  } else if(cl == "integer") {
    "numeric"
  } else if("factor" %in% cl) {
    "text"
  } else {
    cl
  }
}

which_level <- function(f) {
  sapply(f, function(f) which(f == levels(f)), USE.NAMES = FALSE)
}

all_na <- function(x, ...) {
  UseMethod("all_na", x)
}

all_na.default <- function(x) {
  all(is.nan(x) | is.na(x))
}

all_na.character <- function(x, convert = FALSE) {
  found <- which(x == "NaN")
  if(convert) {
    x[found] <- NA_character_
  } else if(length(found) > 0) {
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
    stop(sprintf("Package \"%s\" needed for this function to work. Please install it.", namespace),
         call. = FALSE)
  }
}
