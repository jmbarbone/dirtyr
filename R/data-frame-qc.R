data_frame_qc <- function(df1, df2, index = NULL) {

  is_named <- function(x) {
    length(names(x)) != 0
  }

  ## Determine position of index
  if(is.null(index)) {
    i <- j <- 1
  } else if (is_named(index)) {
    i <- which(colnames(df1) == name(index))
    j <- which(colnames(df2) == index[[1]])
  } else {
    i <- which(colnames(df1) == index)
    j <- which(colnames(df2) == index)
  }

  ## Check that indexes are correct

  df1 <- df1[order(df1[[i]]), ]
  df2 <- df2[order(df2[[j]]), ]

  # match(df1[[i]], df2[[j]])
  # df1[[i]]

  index_mismatch_i <- which(is.na(match(df2[[j]], df1[[i]])))
  index_mismatch_j <- which(is.na(match(df1[[i]], df2[[j]])))

  if(length(c(index_mismatch_i, index_mismatch_j)) > 0) {
    warning("Indexes do not match:\n",
            "In df1 but not in df2:\n",
            paste0(na.omit(df1[index_mismatch_i, i, drop = TRUE]), collapse = " .. "),
            "\n\nIn df 2 but not in df1:\n",
            paste0(na.omit(df2[index_mismatch_j, j, drop = TRUE]), collapse = " .. "),
            call. = FALSE)

  }

  matching_index <- unique(df1[[i]][na.omit(match(df2[[j]], df1[[i]]))])
  df1 <- unique(subset(df1, df1[[i]] %in% matching_index))
  df2 <- unique(subset(df2, df2[[j]] %in% matching_index))

  order_i <- order(df1[[i]])
  order_j <- order(df2[[j]])

  ## Check values
  res <- lapply(colnames(df1[-i][2:3]),
                function(x) {
                  diffs <- which(as.character(df1[[x]][order_i]) != as.character(df2[[x]][order_j]))
                  if(length(diffs) == 0) return(NULL)
                  data.frame(
                    index_no     = diffs,
                    index_name_1 = as.character(df1[diffs, i, drop = TRUE]),
                    index_name_2 = as.character(df2[diffs, j, drop = TRUE]),

                    col_name     = rep(x, length(diffs)),
                    df1_vals     = as.character(df1[diffs, x, drop = TRUE]),
                    df2_vals     = as.character(df2[diffs, x, drop = TRUE]),
                    stringsAsFactors = FALSE)
                })
  Reduce(rbind, res)

}

df1 <- data.frame(i = 1:4,
                  a = 1:4,
                  b = letters[1:4],
                  c = LETTERS[1:4],
                  stringsAsFactors = FALSE)
df2 <- data.frame(i = c(1, 2, 3, 4),
                  a = c(1, 3, 3, 4),
                  b = letters[c(2, 2, 3, 4)],
                  c = LETTERS[1:4],
                  stringsAsFactors = FALSE)

data_frame_qc(df1, df2) %>% as_tibble
