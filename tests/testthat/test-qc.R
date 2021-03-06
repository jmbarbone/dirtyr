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


# ordered -----------------------------------------------------------------

test_that("ordered equal named lengths", {

  ## > Equal length
  lvls <- c(LETTERS[1:10])

  x <- c(A = "A", B = "C", D = "D", E = "A", G = "J")
  y <- setNames(nm = LETTERS)[c(1:5, 10)]
  x <- factor(x, levels = lvls, ordered = TRUE)
  y <- factor(y, levels = lvls, ordered = TRUE)

  exp <- data_frame(index = c("B", "C", "E", "G", "J"),
                    target = c("C", NA_character_, "A", "J", NA_character_),
                    reference = c("B", "C", "E", NA_character_, "J"),
                    difference = c(1, NA_real_, -4, NA_real_, NA_real_))
  res <- qc(x, y)
  expect_equivalent(res, exp)

  expect_equivalent(attr(res, "difference"),
                    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))

  expect_warning(qc(x, y, string_dist = TRUE),
                 "String distances will not be computed for factors")

  ## Threshold
  exp <- data_frame(index = c("C", "E", "G", "J"),
                    target = c(NA_character_, "A", "J", NA_character_),
                    reference = c("C", "E", NA_character_, "J"),
                    difference = c(NA_real_, -4, NA_real_, NA_real_))
  res <- qc(x, y, threshold = 2L)
  expect_equivalent(res, exp)

  expect_identical(attr(res, "difference"),
                   c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE))

  z <- factor(x, levels = lvls[1:4], ordered = FALSE)
  exp <- data_frame(index = "G",
                    target = "J",
                    reference = NA_character_,
                    difference = NA_real_)
  expect_warning(qc(x, z), "Levels do not match, applying factor method")
  expect_equivalent(suppressWarnings(qc(x, z)), exp)
})

# factor ------------------------------------------------------------------

test_that("factor equal levels and string warning", {
  lvls <- c(LETTERS[1:6])

  x <- setNames(nm = factor(LETTERS[2:4], levels = lvls, ordered = FALSE))
  y <- setNames(nm = factor(LETTERS[1:6][-2], levels = lvls, ordered = FALSE))

  exp <- data_frame(idnex = c("A", "B", "E", "F"),
                    target = c(NA_character_, "B", NA_character_, NA_character_),
                    reference = c("A", NA_character_, "E", "F"),
                    difference = rep(NA_real_, 4))
  res <- qc(x, y)
  expect_equivalent(res, exp)

  a <- attr(res, "differences")
  b <- c(TRUE, TRUE, FALSE,  FALSE, TRUE, TRUE)
  expect_equal(a, b)

  expect_warning(qc(x, y, string_dist = TRUE),
                 "String distances will not be computed for factors")
})


# numeric -----------------------------------------------------------------


test_that("numeric", {
  x <- c(a = 2.1, b = 1.0, f = 0.2, c = -1.5)
  y <- c(a = 2.1, b = 1.0, c = 2.7)
  exp <- data_frame(index = c("c", "f"),
                    target = as.character(c(-1.5, 0.2)),
                    reference = c(2.7, NA_character_),
                    difference = c(-4.2, NA_real_)
  )
  res <- qc(x, y)
  expect_equivalent(res, exp)
  expect_equal(attr(res, "difference"),
               c(a = FALSE, b = FALSE, c = TRUE, f = TRUE))
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
  expect_true(TRUE)
})

# character ---------------------------------------------------------------

test_that("character", {

  ## > Equal length
  x <- c("this", "that", "those", "what?")
  y <- c("thas", "THAT", "what are those?", "what")
  exp <- data_frame(index = c(1, 3, 4),
                    target = x[-2],
                    reference = y[-2],
                    difference = c(1, 10, 1))
  res <- qc(x, y, string_dist = TRUE, ignore_case = TRUE)
  expect_equivalent(res, exp)

  ##  >> attributes
  b <- c(TRUE, FALSE, TRUE, TRUE)
  a <- attr(res, "differences")
  expect_equal(a, b)

  ## > Named vectors
  x <- c(a = 1, b = 2, c = NA_character_, d = NA_character_)
  y <- c(a = 1, b = 3, c = NA_character_, d = 0)
  res <- qc(x, y)
  exp <- data_frame(index = c("b", "d"),
                    target = c(2, NA_character_),
                    reference = c('3', '0'),
                    difference = rep(NA_real_, 2))
  expect_equivalent(res, exp)

  ### > attributes
  a <- attr(res, "differences")
  b <- c(a = FALSE, b = TRUE, c = FALSE, d = TRUE)
  expect_equal(a, b)

  ## > No differences ---
  x <- setNames(nm = letters[1:5])
  y <- setNames(nm = letters[c(3, 4, 1, 5, 2)])
  expect_message(qc(x, y), "No differences found")

})


# logical -----------------------------------------------------------------


test_that("logical", {

  ## > Equal length
  x <- c(  NA, TRUE,  TRUE, FALSE, NA)
  y <- c(TRUE, TRUE, FALSE, FALSE, NA)
  exp <- data_frame(index = c(1, 3),
                    target = c(NA_character_, TRUE),
                    reference = c("TRUE", "FALSE"),
                    difference = c(NA_real_, 1.0))
  res <- qc(x, y)
  expect_equivalent(res, exp)

  ## >> attributes
  a <- attr(res, "differences")
  b <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_equal(a, b)


  ## > All NAs
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

