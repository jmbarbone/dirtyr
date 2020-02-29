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
