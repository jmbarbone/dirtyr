context("reverese")

test_that("reverse works", {
  x <- iris %>% reverse() %>% head()
  y <- iris %>% tail() %>% reverse()
  expect_equal(x, y)

  x <- dplyr::storms %>% reverse() %>% head()
  y <- dplyr::storms %>% tail() %>% reverse()
  expect_identical(x, y)
})
