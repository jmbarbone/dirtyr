context("numerics")


test_that("character example", {
  x <- c(1.1, -2.103, 3.001, 4, -5, NA, letters[1:2], "1e4")

  res <- to_numeric(x)
  exp <- c(1.1, -2.103, 3.001, 4, -5, NA_real_, NA_real_, NA_real_, 10000)
  out <- capture.output(data.frame(x, res, exp))
  expect_equal(res, exp,
               info = out)

  res <- maybe_numeric(x)
  exp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  out <- data.frame(x, res, exp)
  expect_equal(res, exp, info = capture.output(out))

  res <- maybe_numeric(x, names = TRUE)
  expect_named(res)
  expect_equal(names(res), x)

  y <- c(1.1, 2, 3, NA, "e", 1e4, 1e-2)
  res <- maybe_integer(y)
  exp <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
  out <- data.frame(y, res, exp)
  expect_equal(res, exp, info = capture.output(out))

  res <- maybe_numeric(y, names = TRUE)
  expect_named(res)
  out <- capture.output(data.frame(y, names(res)))
  expect_equal(names(res), y, info = capture.output(out))
})


test_that("factor example", {
  f <- factor(c(seq(0, 1, .2), "No", "Na_character_", NA))

  res <- to_numeric(f)
  exp <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, NA_real_, NA_real_, NA_real_)
  out <- capture.output(data.frame(f, res, exp))
  expect_equal(res, exp, info = out)

  res <- maybe_numeric(f)
  exp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  out <- capture.output(data.frame(f, res, exp))
  expect_equal(res, exp, info = out)

  res <- maybe_numeric(f, names = TRUE)
  expect_named(res)
  expect_equal(names(res), as.character(f))

  res <- maybe_integer(f, names = TRUE)
  exp <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  expect_equivalent(res, exp)
  expect_equal(names(res), as.character(f))
})
