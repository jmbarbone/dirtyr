context("utils testing")

test_that("require_namespace", {
  expect_error(require_namespace("not-real"),
               'Package "not-real" needed for this function to work')

  expect_null(require_namespace("testthat"))
})
