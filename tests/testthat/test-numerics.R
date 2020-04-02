context("numerics")


test_that("character example", {
  x <- c(as.character(1:5), NA, letters[1:2], "1e4")

  res <- to_numeric(x)
  exp <- c(1, 2, 3, 4, 5, NA_real_, NA_real_, 10000)
  expect_setequal(res, exp)

  res <- maybe_numeric(x)
  exp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)
  expect_setequal(res, exp)

  res <- maybe_numeric(x, names = TRUE)
  expect_named(res)
  expect_setequal(names(res), x)
})


test_that("factor example", {
  f <- factor(c(seq(0, 1, .2), "No", "Na_character_"))

  res <- to_numeric(f)
  exp <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, NA_real_, NA_real_)
  expect_setequal(res, exp)

  res <- maybe_numeric(f)
  exp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_setequal(res, exp)

  res <- maybe_numeric(f, names = TRUE)
  expect_named(res)
  expect_setequal(names(res), f)
})
