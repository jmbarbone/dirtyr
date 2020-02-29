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

qc(target, reference, "index")


test_that("ordered", {
  target = factor(letters[1:5], letters, ordered = TRUE)
  reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = TRUE)
  threshold = 0
  qc(target, reference, string_dist = TRUE)
  attr(.Last.value, "differences")
})

test_that("factor", {
  target = factor(letters[1:5], letters, ordered = FALSE)
  reference = factor(c("a", "b", "j", "e", "e"), letters, ordered = FALSE)
  threshold = 0
  qc(target, reference, string_dist = FALSE)
  attr(.Last.value, "string_dist")
})

test_that("numeric", {

})

test_that("integer", {
  res <- qc(target = 1:5, reference = c(1:2, 8, 10, 11), threshold = 1)
  attr(res, "differences")
})

test_that("Date", {
  qc(as.Date(c("2019-01-12", "2010-05-20")),
     as.Date(c("2019-01-14", "2019-05-20")))
  attributes(.Last.value)
})

test_that("characters", {
  target = c("this", "that", "those", "what?")
  reference = c("thas", "THAT", "what are those?", "what")
  string_dist = TRUE
  ignore_case = TRUE
  try <- qc(target, reference)
  attr(try, "differences")
})
