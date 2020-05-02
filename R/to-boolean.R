#' To boolean
#'
#' Conditionally transforms a non-boolean vector into a boolean vector
#'
#' @param x A non-boolean vector
#' @param true A condition in which to evaluate as TRUE
#' @param false An optional condition to evaluate as FALSE
#' @param na An optional condition to evaluate as NA.  If `na` is set,
#'   so must either `true` or `false`.
#' @param names Logical.  If TRUE, the result will print out with a name
#'
#' @details
#' If the `class()` of `true`, `false`, or `na` is `function`, the function will
#'   be applied to the vector `x` and when evaluated as `TRUE` will return the
#'   respective value of the argument.
#' A warning will appear when `true`, `false`, and `na` are all set together.
#' Only two should be set at a time as any value that has not been matched
#'   will default to the unused argument.
#'
#' @importFrom R6 R6Class
#' @export
#' @examples
#' x <- c("Y", "Y", "N", "Y", "N", "N/A", "Y - not sure", "YN", "YY")
#' to_boolean(x, "Y")
#' to_boolean(x, "Y", names = TRUE)
#' to_boolean(x, "Y", "N", names = TRUE)
#' to_boolean(x,
#'            true = function(x) grepl("^Y", x),
#'            na = "N/A",
#'            names = TRUE)
#'
#' x <- sample(c("Yes", "Maybe", "No", "??"), 10, TRUE)
#' to_boolean(x, c("Yes", "Maybe"), "No", names = TRUE)
#'
#' x <- c(1, 2, 3, 4)
#' to_boolean(x, 1, 2)
#' to_boolean(x, "<= 1", ">= 3")

to_boolean <- function(x, true = NULL, false = NULL, na = NULL, names = FALSE) {
  r6_to_boolean$new(x, true, false, na, names)$bool_eval()
}

r6_to_boolean <- R6Class(
  "to_boolean",
  public = list(
    vals = NULL,
    true = NULL,
    false = NULL,
    na = NULL,
    names = FALSE,

    initialize = function(vals, true = NULL, false = NULL, na = NULL, names = FALSE) {
      self$vals <- vals
      self$true <- true
      self$false <- false
      self$na <- na
      self$names <- names

      if(is.null(true) & is.null(false)) {
        stop("`true` or `false` has to be set.", call. = FALSE)
      }

      if(!is.null(true) & !is.null(false) & !is.null(na)) {
        warning("Not recommended to set `true`, `false`, and `na`",
                " all together, consider setting only 2", call. = FALSE)
      }

      private$len <- length(vals)
    },

    loc = function(e) {
      if(is.null(e)) return(integer())
      switch(class(e),
             character = {
               if(length(e) == 1 && grepl("[<=>]", e)) {
                 which(eval(parse(text = sprintf("self$vals %s", e))))
               } else {
                 which(self$vals %in% e)
               }
             },
             numeric = which(self$vals == e),
             integer = which(self$vals == e),
             `function` = which(e(self$vals)),
             stop(sprintf("Class << %s >> not supported for `e`", class(e)),
                  call. = FALSE)
      )
    },

    bool_eval = function() {
      loct <- self$loc(self$true)
      locf <- self$loc(self$false)
      locn <- self$loc(self$na)

      lt <- length(loct)
      lf <- length(locf)
      ln <- length(locn)

      if(ln) {
        if(lt) { ## true and na
          out <- logical(private$len)
        } else { ## false and na
          out <- !logical(private$len)
        }
      } else {
        if(lt) {
          if(lf) { ## true and false
            out <- rep(NA, private$len)
          } else { ## true
            out <- logical(private$len)
          }
        } else { ## false
          out <- !logical(private$len)
        }
      }

      if(lt) out[loct] <- TRUE
      if(lf) out[locf] <- FALSE
      if(ln) out[locn] <- NA
      if(self$names) names(out) <- self$vals
      out
    }
  ),

  private = list(
    len = integer()
  )
)
