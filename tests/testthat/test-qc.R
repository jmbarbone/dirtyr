context("qc")

target <- data.frame(
  index = letters[1:4],
  doubles = c(1.0, 1.5, 10.2, 10.2),
  integers = c(2:5),
  characters = c("abc", "aaa", "bbb", "ccc"),
  dates = as.Date(c("2019-12-01", "2019-02-21", "2000-05-20", "2019-12-17")),
  factors = factor(letters[1:4], levels = letters, ordered = FALSE),
  ordered = factor(LETTERS[1:4], levels = LETTERS, ordered = TRUE),
  stringsAsFactors = FALSE
)

reference <- data.frame(
  index = letters[1:5],
  doubles = c(1.1, 1.52, 10.2, 10.2, 0.5),
  integers = c(1L, 3L, 4L, 4L, 4L),
  characters = c("def", "aaa", "bbb", "nonsense", "extra"),
  dates = as.Date(c("2019-12-10", "2019-02-22", "2000-05-20", "2019-12-17", "1999-12-31")),
  factors = factor(letters[c(3, 7, 11, 13, 1)], levels = letters, ordered = FALSE),
  ordered = factor(LETTERS[c(3, 7, 11, 13, 1)], levels = LETTERS, ordered = TRUE),
  stringsAsFactors = FALSE
)[c(2, 3, 5, 1, 4), ] ## reordered

index = c(index = "index")
df <- target
new_index <- reference[["index"]]
str_dist = FALSE
add_empty = TRUE

# qc(target, reference, "index")


# orderered ---------------------------------------------------------------

test_that("ordered", {
  target = factor(letters[1:5], letters, ordered = TRUE)
  reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = TRUE)
  threshold = 0
  res <- qc(target, reference, string_dist = TRUE)
  a <- attr(.Last.value, "differences")
  expect_visible(res)
  expect_visible(a)
})

# factor ------------------------------------------------------------------

test_that("factor", {
  target = factor(letters[1:5], letters, ordered = FALSE)
  reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = FALSE)
  threshold = 0
  res <- qc(target, reference, string_dist = FALSE)
  a <- attr(.Last.value, "string_dist")
  expect_visible(res)
  expect_visible(a)
})


# numeric -----------------------------------------------------------------


test_that("numeric", {

})


# integer -----------------------------------------------------------------

test_that("integer", {
  res <- qc(target = 1:5, reference = c(1:2, 8, 10, 11), threshold = 1)
  a <- attr(res, "differences")
  expect_visible(res)
  expect_visible(a)
})


# Date --------------------------------------------------------------------

test_that("Date", {
  res <- qc(as.Date(c("2019-01-12", "2010-05-20")),
            as.Date(c("2019-01-14", "2019-05-20")))
  a <- attributes(.Last.value)
  expect_visible(res)
  expect_visible(a)
})


# POSXITct ----------------------------------------------------------------

test_that("POSITct", {

})

# character ---------------------------------------------------------------

test_that("character", {

  ## > Equal length ----
  x <- c("this", "that", "those", "what?")
  y <- c("thas", "THAT", "what are those?", "what")
  exp <- data_frame(target = x[-2],
                    reference = y[-2],
                    difference = c(1, 10, 1))
  res <- qc(x, y, string_dist = TRUE, ignore_case = TRUE)
  expect_equivalent(res, exp)

  ##  >> attributes ----
  b <- c(TRUE, FALSE, TRUE, TRUE)
  a <- attr(res, "differences")
  expect_equal(a, b)

  ## > Named vectors ----
  x <- c(a = 1, b = 2, c = NA_character_, d = NA_character_)
  y <- c(a = 1, b = 3, c = NA_character_, d = 0)
  res <- qc(x, y)
  exp <- data_frame(target = c(2, NA_character_),
                    reference = c('3', '0'),
                    difference = rep(NA_real_, 2))
  expect_equivalent(res, exp)

  ### > attributes----
  a <- attr(res, "differences")
  b <- c(a = FALSE, b = TRUE, c = FALSE, d = TRUE)
  expect_equal(a, b)

})


# logical -----------------------------------------------------------------


test_that("logical", {

  ## > Equal length ----
  x <- c(  NA, TRUE,  TRUE, FALSE, NA)
  y <- c(TRUE, TRUE, FALSE, FALSE, NA)
  exp <- data_frame(target = c(NA_character_, TRUE),
                    reference = c("TRUE", "FALSE"),
                    difference = c(NA_real_, 1.0))
  res <- qc(x, y)
  expect_equivalent(res, exp)

  ## >> attributes ----
  a <- attr(res, "differences")
  b <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(a, b)


  ## > All NAs ----
  ## This will force the evaluation of x according to class(y)
  x <- rep(NA, 5)
  y <- character(5)
  expect_warning(qc(x, y), "target is all `NA` ... trying method for: character")

  expect_warning(qc(as.character(x), y), NA)
  expect_warning(qc(x, rep(NA_real_, 5)),
                    "target is all `NA` ... trying method for: numeric")

})


# utils -------------------------------------------------------------------

test_that("qc_name_check", {
  xx <- x <- setNames(nm = letters[c(1, 3, 5)])
  yy <- y <- setNames(nm = letters[2:6])
  qc_name_check(xx, yy)
  expect_equal(xx, reindex(x, new_index = y, keep_all = TRUE))
  expect_equal(yy, reindex(y, new_index = x, keep_all = TRUE))

})


# reindex(target, "index", reference$index, add_empty = TRUE)

