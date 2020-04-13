context("utils testing")

test_that("require_namespace", {
  expect_error(require_namespace("not-real"),
               'Package "not-real" needed for this function to work')

  expect_null(require_namespace("testthat"))
})

test_that("are different works", {
  res <- are_different(
    c(a = 1, b = 2, c = NA_character_, d = NA_character_),
    c(a = 1, b = 3, c = NA_character_, d = 0)
  )
  exp <- c(a = FALSE, b = TRUE, c = FALSE, d = TRUE)
  expect_equal(res, exp)
})
