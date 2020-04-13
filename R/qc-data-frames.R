#' "QC" data frames
#'
#' Compare values of two data.frames base on an index
#'
#' @details
#' All values from the target and reference will be returned as characters.
#' `keep_all` is passed to the `keep_all` argument in `reindex()`.
#' If this is `TRUE`,
#'
#' @param target Targer (test) table
#' @param reference Reference table
#' @param index The name or the index
#' @param keep_all Logical, passed to `reindex()` (see details)
#' @param ... additional arguments passed to [stringdist::stringdist]
#'
#' @return
#' A data.frame with the index values and differences in target and comparisons.
#'
#' @export

qc_data_frame <- function(target, reference, index, keep_all = FALSE, ...) {
  if(!is_named(index)) names(index) <- index

  reind_tar <- reindex(target, names(index), reference[[index]], keep_all = keep_all)

  cols <- colnames(reference)
  valid_cols <- cols[cols %in% colnames(target) & cols != names(index)]

  res <- r_bind(lapply(
    valid_cols,
    function(vc) {
      qc_col_implement(
        tar = reind_tar[[vc]],
        ref = reference[[vc]],
        ind = reference[[index]],
        vc = vc)
    }))
  # as_tibble(res[order(res[[index]]), ])
  as_tibble(res)
}

# implementation of qc for each column
qc_col_implement <- function(tar, ref, ind, vc) {
  temp <- qc(tar, ref)
  if(is.null(temp) || nrow(temp) == 0) return(NULL)
  diff_attr <- attr(temp, "differences")
  cbind(data_frame(index = ind[diff_attr | is.na(diff_attr)],
                   comparison = rep(vc, nrow(temp))),
        temp)
}


# qc(test_data_target$index,
#    test_data_reference$index)
#
#
# qc(test_data_target[1:2],
#    test_data_reference[1:2],
#    "index")

check_index_df <- function(index) {
  UseMethod("check_index_df", index)
}


# Todos -------------------------------------------------------------------

## Add listing argument for setting parameters at each column

if(FALSE) {
  list(var1 = list(param1 = 1,
                   param2 = TRUE),
       var2 = list(param1 = 2,
                   param2 = FALSE))
  ## allow iteration
  list(var1 = list(param1 = 1, param2 = FALSE),
       param2 = TRUE)
}
