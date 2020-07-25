context("Appending character-numeric columns")

test_that("it works", {
  fake <- data.frame(v1 = c(1, 2, ".", 4),
                     v2 = c(0.0, 2.5, 5.0, "AQL"))

  res <- append_charnum_cols(fake, c("v1", "v2"))
  expect_visible(res)
  # append_charnum_cols(fake, c("v1", "v2"), beside = FALSE)
})
