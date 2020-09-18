context("reverese")

test_that("reverse works", {
  x <- iris %>% reverse() %>% head()
  y <- iris %>% tail() %>% reverse()
  expect_equal(x, y)

  x <- dplyr::storms %>% reverse() %>% head()
  y <- dplyr::storms %>% tail() %>% reverse()
  expect_identical(x, y)

  # Should work with single column
  x <- data.frame(a = 1:26,
                  row.names = letters)
  rn1 <- row.names(reverse(x))
  rn2 <- row.names(reverse(x, row.names = FALSE))

  expect_equal(rn1, reverse(letters))
  expect_equal(rn2, letters)
})
