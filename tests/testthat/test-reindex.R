context("reindex")

test_that("index.default", {
  a <- setNames(nm = letters[1:4])
  b <- setNames(nm = letters[2:6])
  res1 <- c(b = "b", c = "c", d = "d", e = NA_character_, f = NA_character_)
  res2 <- append(c(a = "a"), res1)
  expect_equal(reindex(a, b),
               reindex(a, new_index = b))
  expect_equal(reindex(a, b), res1)
  expect_equal(reindex(a, b, keep_all = TRUE), res2)

  ## unnamed
  expect_equal(reindex(a, b),
               reindex(a, remove_names(b)))
  expect_equal(reindex(a, b), keep_all = TRUE,
               reindex(a, remove_names(b)), keep_all = TRUE)

  expect_equal(reindex(a, new_index = b),
               reindex(a, new_index = remove_names(b)))
  expect_equal(reindex(a, new_index = b), keep_all = TRUE,
               reindex(a, new_index = remove_names(b)), keep_all = TRUE)
})

test_that("index.data.frame", {
  a <- data_frame(index = letters[1:4],
                  var1 = 1:4,
                  var2 = letters[1:4])
  b <- data_frame(index = letters[2:6],
                  var1 = 2:6,
                  var2 = letters[1:5])

  res1 <- a[which(!is.na(match(a$index, b$index))), ]

  res2 <- list(data_frame(index = "a",
                          var1 = NA_integer_,
                          var2 = NA_character_),
               res1,
               data_frame(index = c("e", "f"),
                          var1 = rep(NA_integer_, 2),
                          var2 = rep(NA_character_, 2))) %>%
    r_bind()


  expect_equal(reindex(a, "index", b$index), res1)
  expect_equal(reindex(a, "index", b$index, keep_all = TRUE), res2)
})
