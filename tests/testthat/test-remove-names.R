context("Remove names")

test_that("Vectors", {
  vec <- 1:26
  names(vec) <- letters
  res <- remove_names(vec)
  expect_null(names(res))
})

test_that("Matrix", {
  mat <- matrix(seq(16), nrow = 4, dimnames = list(letters[1:4], LETTERS[1:4]))
  res <- remove_names(mat)
  expect_null(names(res))
})

test_that("data.frame", {
  df <- data.frame(a = 1:4,
                   b = 5:8,
                   row.names = letters[1:4])
  res <- remove_names(df)
  expect_null(names(res))
  expect_equal(row.names(res), as.character(1:4))
})
