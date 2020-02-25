#' "QC" data frames
#'
#' Compare values of two data.frames base on an index
#'
#' @details
#' All values from the target and reference will be returned as characters.
#'
#' @param reference,target tables to join
#' @param index The name or the index
#' @param string_distances Logical.  If true, distances will be calculated.
#' @param ... additional arguments passed to [stringdist::stringdist]
#'
#' @return
#' A data.frame with the index values and differences in target and comparisons.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr full_join
#' @importFrom purrr map_dfr
#'
#' @export

# qc_data_frame(df1, df2, "Patient #", string_distances = FALSE)
# reference <- df1
# target <- df2
# index <- "Patient #"
# string_distances <- TRUE

qc_data_frames <- function(reference, target, index, string_distances = FALSE, ...) {

  if(string_distances) {
    if(is_namespace_missing("stringdist")) {
      warning("Package stringdist is not installed.  String distances will not be computed",
              call. = FALSE)
      string_distances = FALSE
    }
  }

  cn1 <- colnames(reference)
  cn1 <- cn1[cn1 != index]

  col_names <- cn1[match(colnames(target), cn1, nomatch = 0L)]

  index_values <- reference[[index]][match(target[[index]], reference[[index]], nomatch = 0L)]

  ref <- reference[reference[[index]] %in% index_values, ]
  tar <- target[target[[index]] %in% index_values, ]

  map_dfr(col_names,
          function(x) {

            a <- safe_anti_join(ref[c(index, x)],
                                tar[c(index, x)],
                                by = c(index, x))$result
            b <- safe_anti_join(tar[c(index, x)],
                                ref[c(index, x)],
                                by = c(index, x))$result

            if(is.null(a) & is.null(b)) {
              return(data.frame(
                index = NA_character_,
                reference = NA_character_,
                target = NA_character_,
                comparison = x,
                difference = NaN,
                stringsAsFactors = FALSE))
            }

            res <- full_join(a, b,
                             by = index,
                             suffix = c("_reference", "_target")) %>%
              `names<-`(c("index", "reference", "target")) %>%
              mutate(comparison = x)

            cl <- unique(c(short_class(res$reference), short_class(res$target)))
            if(length(cl) > 1) cl <- "incomparible"

            n_rows <- nrow(res)

            if(n_rows == 0) {
              res$difference <- rep(NaN, n_rows)
            } else {
              res$difference <- switch(cl,
                                       character = {
                                         if(string_distances) {
                                           mapply(stringdist::stringdist,
                                                  a = res$reference,
                                                  b = res$target,
                                                  ...,
                                                  USE.NAMES = FALSE)
                                         } else {
                                           rep(NaN, n_rows)
                                         }},
                                       numeric = with(res, reference - target),
                                       ordered = with(res, which_level(reference) - which_level(target)),
                                       NaN)
            }
            mutate_at(res, c("reference", "target", "comparison"), as.character)
          })
}

safe_anti_join <- purrr::safely(dplyr::anti_join)

globalVariables(c("comparison"))
