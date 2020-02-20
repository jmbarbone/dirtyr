context("To boolean")

test_that("Characters correctly recognized.", {
  x <- c("a", "a", "b", "a", "c", "b")

  res <- to_boolean(x, "a")
  exp <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(res, exp)

  res <- to_boolean(x, "a", "b")
  exp <- c(TRUE, TRUE, FALSE, TRUE, NA, FALSE)
  expect_equal(res, exp)
})

test_that("Numeric: numeric", {
  x <- c(0, 0, 1, 2, 3, 3, 1)

  res <- to_boolean(x, 1)
  exp <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  expect_equal(res, exp)

  res <- to_boolean(x, 1, 0)
  exp <- c(FALSE, FALSE, TRUE, NA, NA, NA, TRUE)
  expect_equal(res, exp)
})

test_that("Numeric: character", {
  x <- c(0, 1, 2, 3)

  res <- to_boolean(x, "<= 1")
  exp <- c(T, T, F, F)
  expect_equal(res, exp)

  res <- to_boolean(x, "> 1", "== 0")
  exp <- c(F, NA, T, T)
  expect_equal(res, exp)

})

test_that("Numeric: Mixed", {
  x <- c(0, 1, 2, 3)

  res <- to_boolean(x, 2, "< 1")
  exp <- c(FALSE, NA, TRUE, NA)
  expect_equal(res, exp)
})
